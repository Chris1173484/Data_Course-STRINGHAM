# SETUP ####
library(tidyverse)
library(readxl)
library(measurements)

# DATA ####
path <- "~/Data_Class_STRINGHAM/human_heights.xlsx"
dat <- read_xlsx("human_heights.xlsx")

# CLEAN ####
dat <- 
dat %>% 
  pivot_longer(everything(),
               names_to = "sex",
               values_to = "height") %>% 
  separate(height, into = c("feet","inches"),convert = TRUE) %>% 
  mutate(inches = (feet*12) + inches) %>% 
  mutate(cm=conv_unit(inches, from='in',to='cm'))

dat %>% 
  ggplot(aes(x=cm,fill=sex)) +
  geom_density(alpha=.5)

t.test(dat$cm~factor(dat$sex))

mod <- glm(data=dat,
    formula=cm~sex)
summary(mod)



mpg %>% 
  ggplot(aes(x=displ,y=cty))+
  geom_point()+
  geom_smooth(method="glm")

glm(data=mpg,
    formula=cty~displ) %>% 
  summary()

library(easystats)
