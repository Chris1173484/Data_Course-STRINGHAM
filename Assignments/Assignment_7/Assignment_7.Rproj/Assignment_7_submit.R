library(tidyverse)
library(tidyr)
library(ggplot2)

#1: Import the Assignment_7/Utah_Religions_by_County.csv
dat <- read_csv("Utah_Religions_by_County.csv")

#2:Clean it up into "tidy" shape
dat$County <- gsub(" ","_",dat$County)


tidy_data <- pivot_longer(dat,     #sort into a relgion column
                          cols = c(`Assemblies of God`, `Episcopal Church`, `Pentecostal Church of God`, `Greek Orthodox`, `LDS`, `Southern Baptist Convention`, `United Methodist Church`, `Buddhism-Mahayana`, `Catholic`, `Evangelical`, `Muslim`, `Non Denominational`, `Orthodox`), 
                          names_to = "Religion", 
                          values_to = "Count") %>% 
  rename(Non_Religious='Non-Religious')

tidy_data$Religion <- gsub(" ","_",tidy_data$Religion) # remove spaces


#3: Explore the data
ggplot(tidy_data, aes(x = Pop_2010)) + # population distribution
  geom_histogram(binwidth = 10000, fill = "skyblue", color = "black") +
  labs(title = "Population Distribution",
       x = "Population",
       y = "Frequency")

ggplot(tidy_data, aes(x = Pop_2010, y = Religious)) + # relationship between population and the religious ratio across different observations
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Population vs. Religious Ratio",
       x = "Population",
       y = "Religious Ratio")


ggplot(tidy_data, aes(x = `Count`, y = `Pop_2010`)) + #relationship between the count of religious affiliation and the population size for each  county
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(title = "Relgious vs Non Religious",
       x = "Non Religious",
       y = "Relivious")

#4:Address these questions
  #1 “Does population of a county correlate with the proportion of any specific religious group in that county?”


religious_group <- "Catholic" # we filter the data to only include a specific relgioun, Catholic
subset_data <- tidy_data[tidy_data$Religion == religious_group, ]

ggplot(subset_data, aes(x = Pop_2010, y = Count)) +# I then made a scatter plot showing population vs Proportion of catholic
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") + # added a linear line to show trend
  labs(title = paste("Population vs. Proportion of", religious_group),
       x = "Population",
       y = paste("Proportion of", religious_group)) +
  theme_minimal()

correlation <- cor(subset_data$Pop_2010, subset_data$Count)# calculated the correlation coefficient 
correlation # it was 0.1591744, showing that as population size increases in a county, it tends 
# to be a slight increase in proportino of a specific relgious group


  #2“Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?”
ggplot(subset_data, aes(x = Count, y = Non_Religious)) +#scatterplot showing proportino of specific relgious groups and non relgious groups
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +# linear regression line
  labs(title = paste("Proportion of", religious_group, "vs. Proportion of Non-religious"),
       x = paste("Proportion of", religious_group),
       y = "Proportion of Non-religious") +
  theme_minimal()
correlation <- cor(subset_data$Count, subset_data$Non_Religious)#calcuate the correlation between the two
correlation# came out to be 0.08595778, shows a weak postiive relationship, it shows a trend but not a very strong one
