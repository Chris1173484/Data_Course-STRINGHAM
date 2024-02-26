#load packages
library(tidyverse)
library(tidyr)
library(dbplyr)
library(gganimate)

# Load Data
dat <- read_csv("BioLog_Plate_Data.csv")


#1:Clean this data into tidy (long) form
data <- pivot_longer(dat,cols=starts_with("Hr_"), # converting our hour columns to one column 
                     names_to="hour",
                     values_to = "value") %>%
  rename(sample_id='Sample ID',  #change variable column names to lowercase
         rep='Rep',
         well='Well',
         dilution='Dilution',
         substrate='Substrate') %>% 
  mutate(hour = gsub("Hr_", "", hour))



#2:Create a new column specifying whether a sample is from soil or water
data <- data %>%
  mutate(Type = case_when(
    grepl("Clear_Creek|Waste_Water", sample_id) ~ "Water",  #finding the ones that contain this
    grepl("Soil_1|Soil_2", sample_id) ~ "Soil",
    TRUE ~ "Unknown"
  ))


#3 Generates a plot that matches this one (note just plotting dilution == 0.1)
df<- data[data$dilution==0.1,]# getting only the data that has 0.1 dilution
df$hour <- as.numeric(df$hour)# change charcter to numeric

ggplot(df,aes(x =hour, y =value,color=Type)) +
  geom_smooth(method="loess",se=FALSE)+ # () means getting rid of gray backgroun
  facet_wrap(~ substrate)+# Separate plots for each substrate
  labs(
    title = "Just dilution 0.1",
    x = "Time",
    y = "Absorbance")+
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(),
        strip.background = element_blank(),  
        strip.text = element_text(color = "black"),  
        strip.placement = "outside")

#4:Generates an animated plot that matches this one (absorbance values are mean of all 3 replicates for each group)
df2<- data %>%
  group_by(hour,dilution,sample_id) %>% 
  summarise (mean_absorbance=mean(value,na.rm = TRUE))

df2$hour <- as.numeric(df2$hour) #this makes it have correct x axis values

ggplot(df2,aes(x=hour,y=mean_absorbance,color=sample_id))+
  geom_line()+
  facet_wrap(~dilution)+
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.5))+
  labs(x="Time",
       y="Mean_abosorbance")+
  theme_bw() +
  theme(panel.border = element_blank(),
        strip.background = element_blank(),  
        strip.text = element_text(color = "black"),  
        strip.placement = "outside")+
  transition_reveal(mean_absorbance)

