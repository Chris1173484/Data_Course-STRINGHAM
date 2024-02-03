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

kaggle# site for project ideas
data.x # x is a state for data you want to look in




