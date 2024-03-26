#Week 3 TUESDAY

#1 build a data frame from mtcars with only rows that have  more than 4 cylinders (cyl)
df <- mtcars[mtcars$cyl>4,]

#2
#pull out just the miles per gallon of those cars (mpg) and find the mean,min,max

min(df$mpg)
mean(df$mpg)
max(df$mpg)



# object types ####
## logical 1 dimensions ####
c(TRUE,TRUE,FALSE)
## numeric 1d ####
1:10
## character 1d ####
letters[1:3]
## integer 1d ####
c(1L,2L,3L)
## data.frame 2 dimensions
dff <- mtcars[mtcars$mpg>3,]
## Factor ####
as.factor(letters)
haircolors <- c("brown","blonde","black","red", "red","black")
c(as.factor(haircolors),"purple")
as.character(as.factor(haircolors),"purple")
haircolors


# type conversion ####
1:5 # numeric
as.character(1:5) # convert to character
as.numeric(letters)
as.numeric(c("1","b","35"))
as.logical(c("true","t","F","False"))
x <- as.logical(c("true","t","F","False"))
sum(x,na.rm = TRUE)


# data frames ####
str(mtcars)
names(mtcars)

# as.character(mtcars)
as.character(mtcars$mpg)
mtcars[,"mpg"]

# for loop assigns numeric to character
for (col  in names(mtcars)){
  mtcars[,col] <- as.character(mtcars[,col])
}

apply(mtcars,2,as.logical) # use this instead of the for loop


mtcars
str(mtcars)
data("mtcars") # reset built in data fram

path <- "./Data/cleaned_bird_data.csv"
df <- read.csv(path)
str(df)

for (birds in names(df) ){
  df[,birds] <- as.character(df[,birds])

}
str(df)  


# write the new file to your family
write.csv(df,file = "./Data/cleaned_bird_data_chr.csv")

# " apply" family functions ####
apply(mtcars,2,as.logical)

## packages ####
library(tidyverse)
#filter helps us subset data frames by rows
mtcars %>% 
  filter(mpg>19 & vs==1)
  

# "%>% " pipe # make sure you load library(tidyverse) to use the pipe function,control shift M
# thing on the left becomes first argument to thing on the right
mtcars$mpg %>% mean()



# THURSDAY
library(palmerpenguins)
library(tidyverse)
penguins %>% names

# subset this penguins data frame to only have those observations where  bill_length_mm>40
df <- penguins[penguins$bill_length_mm>40,]
df
#or use this, use the & to add more specifics
x <- penguins %>% 
    filter(bill_length_mm>40 & sex=="female") 
x$body_mass_g %>% mean 

#this also works , find mean body mass of female long beaked penguins
penguins %>% 
  filter(bill_length_mm>40 & sex=="female") %>% 
  pluck("body_mass_g") %>% 
  mean

#do the same but for each species
penguins %>% 
  filter(bill_length_mm>40 & sex=="female") %>% 
  group_by(species,island) %>% # commas to separate all grouping columns
  summarize(mean_body_mass=mean(body_mass_g),
            min_body_mass=min(body_mass_g),
            max_body_mass=max(body_mass_g),
            sd_body_mass=sd(body_mass_g),
            N=n()) %>% 
  arrange(desc(mean_body_mass)) %>% 
  write_csv("./Data/penguins_summary.csv")


# find the fatties penguins ( body_mass>5000)
#count how many are male and how many are female
# return the max body mass for males and females
#bonus : add new column to penguins that says whether they re a fattie

penguins %>% 
  filter(body_mass_g>5000) %>% 
  group_by(sex) %>% 
  summarize(N=n(),# N is how to count
            max_body_mass=max(body_mass_g))

penguins %>% 
  mutate(fatties=body_mass_g>5000) # for changing and adding new columns


#or to put the word fatty instead of true or false
x <- 
penguins %>% 
  mutate(fatstack=case_when(body_mass_g>5000~"fattie",
                            body_mass_g<= 5000 ~"skinny"))

x %>% 
  filter(!is.na(sex)) %>% 
  ggplot(mapping = aes(x=body_mass_g,
                       y=bill_length_mm,
                       color=fatstack,
                       shape=fatstack))+
  geom_point()+
  geom_smooth()+
  #scale_color_viridis_d(option = 'plasma',end=.8)+
  scale_color_manual(values = c("violet","turquoise"))+
  theme_dark()+
  theme(axis.text = element_text(angle=180,face='italic'))
  
# WEEK 4 TUESDAY , continued 
#these are called global aesthetics
names(penguins)
ggplot(penguins,mapping = aes(x = flipper_length_mm,
                              y=body_mass_g,color=species,alpha=bill_depth_mm))+
  
  # geom_col()
  #this is for specific aesthetics, each geom is covering up the last layer ,so order correctly
  geom_path(aes(group=species))+
  stat_ellipse()+
  geom_point(aes(color=sex))+
  geom_polygon()+
  geom_hex()+
  geom_bin2d()+
  geom_boxplot()+
  geom_hline(yintercept=4500,linewidth=25,color='magenta',
             linetype='1121',alpha=.25)+
  geom_point(color='yellow',aes(alpha=bill_depth_mm))+
  theme(axis.title=element_text(face = 'italic',size=12,angle=30),
        legend.background = element_rect(fill='hotpink',color='blue',linewidth = 5))

#WEEK 4 THURSDAY
#make an intersecting plot of the penguin data
library(tidyverse)
library(palmerpenguins)
ggplot(penguins,mapping=aes(x=flipper_length_mm,
                            y=species,color=species,alpha=bill_depth_mm))+
  geom_boxplot(color='green')+
  theme(axis.text.x = element_text(color='blue'),
        axis.text.y=element_text(color='purple'))+
  labs(title = "Species vs Flipper length",
       x='Flipper Length',
       y='Species')

#show and tell
install.packages("leaflet")# good for making map things

penguins %>% 
  ggplot(aes(x=species,
             y=body_mass_g))+
  geom_boxplot()+
  geom_jitter(height = 0,width = .1,alpha=.2)

penguins %>% 
   ggplot(aes(x=body_mass_g,fill=species))+
  geom_histogram(alpha=.25)+
  labs(title="penguins",
       x='other penguins',
       y='other other penguins')

install.packages("GGally")
library(GGally)
library(palmerpenguins)

penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x=bill_length_mm,y=body_mass_g))+
  geom_point(aes(color=sex),size=4,alpha=.75)+
  facet_wrap(~species)+
  scale_color_viridis_d(end=.8)+
  labs(x="Bill depth (mm)",y="Body mass (g)",color="sex")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(face='bold',size=12),
        axis.title = element_text(face='bold',size='12'))


#THURSDAY 2/15
library(tidyverse)
df <- read_csv("C:/Users/cstri/DataClass/Data_Course_STRINGHAM/Data/Bird_Measurements.csv")
library(janitor)
library(skimr)
skim(df)
df <- df %>% 
  clean_names()


# these are the columns we want to keep in our data frame, everything not in this is excluded
keep <- c("Family","Species_number","Species_name","English_name","Clutch_size","Egg_mass","Mating_System") %>% 
  str_to_lower()


male <- df %>% 
  clean_names() %>% #this is cleaning the names making all lowercases, removing special characters and spaces
  select(keep,starts_with("m_"),-ends_with("_n")) %>% #selecting from the keep frame, the ones that start with "m_" and ends with "_n"
  mutate(sex="Male")#making a new column named sex , with Male in it

female <- df %>% 
  clean_names() %>% #this is cleaning the names making all lowercases, removing special characters and spaces
  select(keep,starts_with("f_"),-ends_with("_n")) %>% 
  mutate(sex="Female")

unsexed <- df %>% # same as above
  clean_names() %>% 
  select(keep,starts_with("unsexed_"),-ends_with("_n")) %>% 
  mutate(sex="unsexed")

clean <- 
  male %>% # this is no joining all the data
  full_join(female) %>% 
  full_join(unsexed)


library(tidyverse)

iris$Species %>% 
  stringr::str_to_title() %>% 
  unique()


iris$Sepal.Length %>% 
  round() %>% 
  max(0)

rnorm(100,0,5) %>% 
  abs() %>% 
  mean()

seq(1,100,0.01) %>% 
  round(1) %>% 
  median()

library(tidyverse)

#kaggle# site for project ideas
#data.x # x is a state for data you want to look in


# TUESDAY 2/6
library(tidyverse)
library(ggimage)
library(gganimate)
library(patchwork)
library(gapminder)

#more plotting today
warmup<- gapminder %>% 
  ggplot(mapping = aes(x=lifeExp,
                       y=pop))+
  geom_area()+
  theme(axis.title = element_text(face="bold",size=32))+
  labs(title = "Cool stuff ",
       x="hahah",
       y="lol")


#THURSDAY 2/8 and 2/13
library(tidyverse)
df <- read_csv("C:/Users/cstri/DataClass/Data_Course_STRINGHAM/Data/wide_income_rent.csv")


# when data set is not set up how ggplot likes it this is how you switch it
df %>% 
  pivot_longer(-variable,names_to = "state",values_to = "amount") %>% 
  pivot_wider(names_from = variable,
              values_from = amount) %>% 
  ggplot(aes(x=state,y=rent))+
  geom_col()+
  theme(axis.text.x = element_text(angle=90,hjust=.5,vjust = .5,size=6))


# if one variable is across multiple columns..pivot longer
# if multiple variables are in a single column..pivot wider

table1
table2
table2 %>% 
  pivot_wider(names_from=type,values_from=count)

table3 %>%
  separate(rate,into=c("Cases","Population"))
table4a
table4b
table5 %>% 
  separate(rate,into=c("cases","population"),convert=TRUE) %>% 
  mutate(year=paste0(century,year) %>% as.numeric())  %>% 
  select(-century)

library(tidyverse)
library(readxl)
dat <- read_xlsx("C:/Users/cstri/DataClass/Data_Course_STRINGHAM/Data/messy_bp.xlsx",skip=3)
bp <- dat %>% 
  select(-starts_with("HR"))

bp <- 
  bp %>% 
  pivot_longer(starts_with("BP"),
               names_to="visit",
               values_to = "bp") %>% 
  mutate(visit=case_when(visit == "BP...8"~1,
                         visit ==  "BP...10"~2,
                         visit ==  "BP...12"~3)) %>% 
  separate(bp,into=c("systolic","diastolic")) 


hr <- dat %>% 
  select(-starts_with("BP"))

hr <- 
  hr %>% 
  pivot_longer(starts_with("HR"),
               names_to="visit",
               values_to = "hr") %>% 
  mutate(visit=case_when(visit == "HR...9"~1,
                         visit ==  "HR...11"~2,
                         visit ==  "HR...13"~3)) 

df <- full_join(bp,hr)

#cleanup stuff
library(janitor)
janitor::clean_names()
#clean up column names to be sane and not evil
df <- df %>% 
  clean_names()

df$race %>% unique
df <- 
  df %>% 
  mutate(race=case_when(race=="Caucasian"|race =="WHITE"~"White",
                        TRUE~race)) %>% 
  mutate(birthdate=paste(year_birth,month_of_birth,day_birth,sep="-") %>% as.POSIXct()) %>% 
  mutate(systolic=systolic %>% as.numeric(),
         diastolic=diastolic %>% as.numeric()) %>% 
  select(-pat_id,-month_of_birth,-day_birth,-year_birth) %>% 
  mutate(hispanic=case_when(hispanic=="Hispanic"~TRUE,
                            TRUE~FALSE)) %>% 
  pivot_longer(cols=c("systolic","diastolic"),names_to="bp_type",values_to="bp")


df %>% 
  ggplot(aes(x=visit,y=hr,color=sex))+
  geom_path()+
  facet_wrap(~race)

df %>% 
  ggplot(aes(x=visit,y=bp,color=bp_type))+
  geom_path()+
  facet_wrap(~race)
#Week 3 TUESDAY

#1 build a data frame from mtcars with only rows that have  more than 4 cylinders (cyl)
df <- mtcars[mtcars$cyl>4,]

#2
#pull out just the miles per gallon of those cars (mpg) and find the mean,min,max

min(df$mpg)
mean(df$mpg)
max(df$mpg)



# object types ####
## logical 1 dimensions ####
c(TRUE,TRUE,FALSE)
## numeric 1d ####
1:10
## character 1d ####
letters[1:3]
## integer 1d ####
c(1L,2L,3L)
## data.frame 2 dimensions
dff <- mtcars[mtcars$mpg>3,]
## Factor ####
as.factor(letters)
haircolors <- c("brown","blonde","black","red", "red","black")
c(as.factor(haircolors),"purple")
as.character(as.factor(haircolors),"purple")
haircolors


# type conversion ####
1:5 # numeric
as.character(1:5) # convert to character
as.numeric(letters)
as.numeric(c("1","b","35"))
as.logical(c("true","t","F","False"))
x <- as.logical(c("true","t","F","False"))
sum(x,na.rm = TRUE)


# data frames ####
str(mtcars)
names(mtcars)

# as.character(mtcars)
as.character(mtcars$mpg)
mtcars[,"mpg"]

# for loop assigns numeric to character
for (col  in names(mtcars)){
  mtcars[,col] <- as.character(mtcars[,col])
}

apply(mtcars,2,as.logical) # use this instead of the for loop


mtcars
str(mtcars)
data("mtcars") # reset built in data fram

path <- "./Data/cleaned_bird_data.csv"
df <- read.csv(path)
str(df)

for (birds in names(df) ){
  df[,birds] <- as.character(df[,birds])
  
}
str(df)  


# write the new file to your family
write.csv(df,file = "./Data/cleaned_bird_data_chr.csv")

# " apply" family functions ####
apply(mtcars,2,as.logical)

## packages ####
library(tidyverse)
#filter helps us subset data frames by rows
mtcars %>% 
  filter(mpg>19 & vs==1)


# "%>% " pipe # make sure you load library(tidyverse) to use the pipe function,control shift M
# thing on the left becomes first argument to thing on the right
mtcars$mpg %>% mean()



# THURSDAY
library(palmerpenguins)
library(tidyverse)
penguins %>% names

# subset this penguins data frame to only have those observations where  bill_length_mm>40
df <- penguins[penguins$bill_length_mm>40,]
df
#or use this, use the & to add more specifics
x <- penguins %>% 
  filter(bill_length_mm>40 & sex=="female") 
x$body_mass_g %>% mean 

#this also works , find mean body mass of female long beaked penguins
penguins %>% 
  filter(bill_length_mm>40 & sex=="female") %>% 
  pluck("body_mass_g") %>% 
  mean

#do the same but for each species
penguins %>% 
  filter(bill_length_mm>40 & sex=="female") %>% 
  group_by(species,island) %>% # commas to separate all grouping columns
  summarize(mean_body_mass=mean(body_mass_g),
            min_body_mass=min(body_mass_g),
            max_body_mass=max(body_mass_g),
            sd_body_mass=sd(body_mass_g),
            N=n()) %>% 
  arrange(desc(mean_body_mass)) %>% 
  write_csv("./Data/penguins_summary.csv")


# find the fatties penguins ( body_mass>5000)
#count how many are male and how many are female
# return the max body mass for males and females
#bonus : add new column to penguins that says whether they re a fattie

penguins %>% 
  filter(body_mass_g>5000) %>% 
  group_by(sex) %>% 
  summarize(N=n(),# N is how to count
            max_body_mass=max(body_mass_g))

penguins %>% 
  mutate(fatties=body_mass_g>5000) # for changing and adding new columns


#or to put the word fatty instead of true or false
x <- 
  penguins %>% 
  mutate(fatstack=case_when(body_mass_g>5000~"fattie",
                            body_mass_g<= 5000 ~"skinny"))

x %>% 
  filter(!is.na(sex)) %>% 
  ggplot(mapping = aes(x=body_mass_g,
                       y=bill_length_mm,
                       color=fatstack,
                       shape=fatstack))+
  geom_point()+
  geom_smooth()+
  #scale_color_viridis_d(option = 'plasma',end=.8)+
  scale_color_manual(values = c("violet","turquoise"))+
  theme_dark()+
  theme(axis.text = element_text(angle=180,face='italic'))

#these are called global aesthetics
names(penguins)
ggplot(penguins,mapping = aes(x = flipper_length_mm,
                              y=body_mass_g,color=species,alpha=bill_depth_mm))+
  
  # geom_col()
  #this is for specific aesthetics, each geom is covering up the last layer ,so order correctly
  geom_path(aes(group=species))+
  stat_ellipse()+
  geom_point(aes(color=sex))+
  geom_polygon()+
  geom_hex()+
  geom_bin2d()+
  geom_boxplot()+
  geom_hline(yintercept=4500,linewidth=25,color='magenta',
             linetype='1121',alpha=.25)+
  geom_point(color='yellow',aes(alpha=bill_depth_mm))+
  theme(axis.title=element_text(face = 'italic',size=12,angle=30),
        legend.background = element_rect(fill='hotpink',color='blue',linewidth = 5))




  

