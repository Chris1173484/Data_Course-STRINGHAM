library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(caret)
library(patchwork)
library(skimr)
library(GGally)
library(lindia)
library(modelr)

#1. Read in the unicef data (10 pts) 
data <- read_csv("unicef-u5mr.csv")

#2. Get it into tidy format (10 pts)
data$CountryName <- gsub("&", "", data$CountryName)  # remove &'s
data$CountryName <- gsub(" ","_",data$CountryName)#replace spaces with underscore
data$CountryName <- tolower(data$CountryName) # all lowercase

tidy <- pivot_longer(data,cols=starts_with("U5MR."),# changed it into a longer data frame
                     names_to = "Year",
                     values_to = "mortality_rate")
tidy$Year <- sub("^U5MR\\.","",tidy$Year)# remove U5MR.
tidy2 <- na.omit(tidy)#remove NAs
tidy2$Year <- as.numeric(tidy2$Year)

#3 Plot each country?s U5MR over time (20 points)
STRINGHAM_Plot_1 <- 
  ggplot(tidy2,aes(x=Year,y=mortality_rate,group=CountryName))+
  geom_line()+
  facet_wrap(~Continent)+
  labs(title = "U5MR Over Time",
       x="Year",
       y="U5MR")
STRINGHAM_Plot_1
#4. Save this plot as LASTNAME_Plot_1.png (5 pts)
ggsave("STRINGHAM_Plot_1.png",plot=STRINGHAM_Plot_1,width=10,height=6,units="in",dpi=300)

#5. Create another plot that shows the mean U5MR for all the countries within a given continent at each year (20 pts)
tidy2 <- tidy2 %>%
  group_by(Continent,Year) %>%
  mutate(mean = mean(mortality_rate, na.rm = TRUE))


STRINGHAM_Plot_2  <- 
  ggplot(tidy2, aes(x = Year, y = mean,color=Continent)) +
  geom_line()+
  labs(title = "Mean U5MR Over Time ",
       x = "Year",
       y = "Mean_U5MR")
STRINGHAM_Plot_2
#6 save that plot as LASTNAME_Plot_2.png (5 pts)
ggsave("STRINGHAM_Plot_2.png",plot=STRINGHAM_Plot_2,width=10,height=6,units="in",dpi=300)

#7.Create three models of U5MR (20 pts)
mod1 <- lm(data=tidy2, formula = mortality_rate~Year)
mod2 <-lm(data=tidy2,formula=mortality_rate~Year+Continent)
mod3 <- lm(data=tidy2,formula=mortality_rate~Year*Continent)
df <- gather_predictions(tidy2,mod1,mod2,mod3)

# 8. Compare the three models with respect to their performance
summary(mod1)
summary(mod2)
summary(mod3) # I would pick  model 3 because it seems to provide the best fit among the three as it captures the 
#interaction effects between the Year and Continent resulting in a higher  adjusted R-Squared value.The p-value is
# also extremely low.

#9 Plot the 3 models’ predictions like so: (10 pts)
mod4 <- lm(data=df, formula = mortality_rate~Year)
mod5 <-lm(data=df,formula=mortality_rate~Year+Continent)
mod6 <- lm(data=df,formula=mortality_rate~Year*Continent)


ggplot(df,aes(x=Year,y=pred,color=Continent))+
  geom_smooth(method="lm",se=FALSE)+
  facet_wrap(~model)+
  labs(y="Predicted_U5mR")

