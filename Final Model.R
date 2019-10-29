library(readr)
library(caret)
library(tidyverse)
library(car)
library(estimatr)
train_model <- read_csv("cleaned_train.csv")
test_model <- read_csv("cleaned_test.csv")
train_model$X1 <- NULL
train_model$status_group <- NULL


test_model$X1 <- NULL
test_model$status_group <- NULL

train_model <- train_model %>% filter(year_recorded > construction_year)
  
  
#Calculae time failure on train set
train_model$time_failure <- train_model$year_recorded - train_model$construction_year

#Remove original variables
train_model$construction_year <- NULL
train_model$year_recorded <- NULL

#First regression including all variables
reg1 <- lm(time_failure ~., train_model)
summary(reg1)

#Joint hypothesis test
linearHypothesis(reg1, c("longitude = 0", "latitude = 0", "basin = 0", "region = 0", "ward = 0"))

#Second regression removing variables which are not important
reg2 <- lm(time_failure ~. -latitude -waterpoint_type -amount_tsh -id -management -longitude -source,train_model)
summary(reg2)
ncvTest(reg2)
plot(reg2)
plot(density(resid(reg2)))


reg3 <- lm_robust(time_failure ~. - installer -latitude -waterpoint_type -amount_tsh -id -management -longitude -source,train_model, se_type = "HC3")
summary(reg3)
#Predict values for the test set using final regression
pred <-predict(reg3, test_model)
data.frame(R2 = R2(pred, test_model$time_failure), RMSE = RMSE(pred, test_model$time_failure))
summary(pred)



test_model$time_failure <- pred
write.csv(test_model, "final_predict.csv")
