#2:-In it, describe your idea for a data analysis final project
#I wil be comparing sea level differences dated fromaround 1880-2014, from the US environmental protection agency
# using data from CSIRO and NOAA. NAsA also measures sea level using global satellites

#3-Also, Describe the sort of data you will use, potential sources, and what the data might look like
# it will mostly be charts and grpahs with dates, averages, ranges, min/maxes, predective models as well

#5:-Generate a plot (using ggplot) using either your real or fake data that shows at least one expected result
final_project <- read.csv("C:/Users/cstri/DataClass/Data_course_STRINGHAM/project/sea_levels_2015.csv")
final_project
library(tidyverse)
final_project %>% 
  ggplot(mapping = aes(x=Time,
                       y=GMSL))+
  geom_point()
