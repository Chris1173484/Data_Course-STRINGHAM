library(tidyverse)
library(mgcv)
library(modelr)

#1 loads the “/Data/mushroom_growth.csv” data set
dat <- read_csv('mushroom_growth.csv')

#2 creates several plots exploring relationships between the response and predictors
ggplot(dat, aes(x = Temperature ,y=GrowthRate, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Species)

ggplot(dat, aes(x = Light, y = GrowthRate, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Species)

ggplot(dat, aes(x = Nitrogen, y = GrowthRate, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Species)


#3 defines at least 4 models that explain the dependent variable “GrowthRate”
lm_model <- lm(GrowthRate ~ Temperature + Light + Nitrogen + Humidity, data = dat)

lm_model_2 <- lm(GrowthRate ~ Temperature * Light  * Humidity, data = dat)

lm_model_3 <- lm(GrowthRate~Nitrogen +Light,data=dat)

lm_model_4 <- lm(GrowthRate~Temperature+Humidity,data=dat)


#4 calculates the mean sq. error of each model
mean(lm_model$residuals^2)
mean(lm_model_2$residuals^2)
mean(lm_model_3$residuals^2)
mean(lm_model_4$residuals^2)


#5 selects the best model you tried
#lm_model_2, I like this one because it has the lowest MSE of the four.
#A lower MSE indicates better predictive performance

#6 adds predictions based on new hypothetical values for the independent variables used in your model
dat2 <- dat %>% 
  add_predictions(lm_model_2)


pred=predict(lm_model_2,newdata=dat2)

hyp_preds <- data.frame(GrowthRate=dat2$GrowthRate,
                        pred=pred)

dat2$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"

fullpreds <- full_join(dat2,hyp_preds)

#7 plots these predictions alongside the real data
ggplot(fullpreds,aes(x=GrowthRate,y=pred,color=PredictionType))+
  geom_point()+
  geom_point(aes(y=Light),color="Turquoise")

