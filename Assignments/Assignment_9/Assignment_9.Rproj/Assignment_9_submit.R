library(tidyverse)
library(modelr)
library(easystats)
library(GGally)
library(ggpubr)
library(pacman)
library(knitr)
library(bookdown)
library(rmarkdown)
pacman::p_load('knitr', 'lme4', 'lmerTest', 'effects', 'dplyr', 'plotrix', 'car', 'bookdown')

##Read in data
dat <- rmodelrdat <- read_csv("GradSchool_Admissions.csv")


#looking at comparisons
fig_0 <- dat %>% 
  select(admit,gre,gpa,rank) %>% 
  ggpairs()


#sticking with gre and gpa
#graphs
fig_1 <- dat %>% 
  ggplot(aes(x=gre,y=gpa))+
  geom_point()+
  facet_grid(admit~rank)+
  theme_minimal()
  fig_1
  
  ggpubr::ggqqplot(dat$gre)
  

#models

mod1 <- glm(data=dat,
            formula=gre~gpa+admit)

mod2 <- glm(data=dat,
             formula=gre~gpa*admit)


mod3 <- glm(data=dat,
            formula=gre~admit*rank)

full_mod <- glm(data=dat,
                formula=gre~gpa*admit*rank)

#comparing
step <- MASS::stepAIC(full_mod,trace = 0)
mod4 <- glm(data=dat,
            formula=step$formula)

comps <- compare_performance(mod1,mod2,mod3,
                             rank=TRUE)
comps
comps %>% 
  plot()

#predictions,doesnt seem to be following the trend as hard as an incline
fig_2 <- dat %>% 
  gather_predictions(mod1,mod2,mod3) %>% 
  ggplot(aes(x=gre,y=pred,color=model))+
  geom_segment(aes(x=0,y=0,xend=800,yend=650),linetype=2,color="black",alpha=.5)+
  geom_smooth(method="lm",se=FALSE)+
  facet_wrap(~rank)+
  theme_minimal()+
  scale_color_viridis_d()+
  labs(title = "Predictions vs Observations",
       subtitle="Dashed line indicates perfect overlap between observed values and predctions")
  fig_2
  
set.seed(123)
training_samples <- caret::createDataPartition(seq_along(dat$gre),
                                               p=.8)
train_data <- dat[training_samples$Resample1,]
test_data <- dat[-training_samples$Resample1,]

mod2_formula <- mod2$formula
mod1_formula <- mod1$formula

mod2 <- glm(data=train_data,
            formula=mod2_formula)

mod1 <- glm(data=train_data,
            formula=mod1_formula)


fig_3 <- gather_predictions(test_data,mod2,mod1) %>% 
  ggplot(aes(x=gre,y=pred,color=model))+
  geom_segment(aes(x=0,y=0,xend=800,yend=650),linetype=2,color="black",alpha=.5)+
  geom_smooth(method="lm",se=FALSE)+
  facet_wrap(~rank)+
theme_minimal()+
  scale_color_viridis_d()+
  labs(title = "Predictions vs Observations",
       subtitle="Dashed line indicates perfect overlap between observed values and predctions")

fig_3
mod2 %>% model_parameters()
