rm(list=ls())
library(readxl)
library(Hmisc)
library(MASS)
library(caret)
library(regclass)
library(ISLR)
library(boot)
library(vcd)
library(pROC)
library (ROCR)

mydata1<-read.csv("npc_model_20201130-20211101_Updated2.csv")
summary(mydata1)
head(mydata1)
str(mydata1)

np <- as.numeric(mydata1$no_pilot)
np
notreach <- as.numeric(mydata1$Not.easy.to.reach)
as.numeric(mydata1$total_weight)
as.numeric(mydata1$total_distance)
as.numeric(mydata1$supply)
as.numeric(mydata1$demand)
as.numeric(mydata1$days_until)
as.numeric(mydata1$days_from_request)

#Model Fitting
mylogit<-glm(np ~ mydata1$weekday_of_mission + mydata1$total_weight+ mydata1$total_distance+ mydata1$supply +mydata1$demand + mydata1$days_until + mydata1$days_from_request + mydata1$passenger_success_ratio
             + mydata1$route_success_ratio + notreach +mydata1$to_airport_elevation ,data=mydata1,family=binomial(link="logit"))

#coefficients
summary(mylogit) 

confmat<-confusion_matrix(mylogit) 
confmat
##########
library(leaps)


rr <- regsubsets(np ~ mydata1$weekday_of_mission  + mydata1$month+ mydata1$appt_time_type +    mydata1$from_airport_id +mydata1$from_airport_elevation + mydata1$from_airport_freq+ 
                   mydata1$from_runway_length + mydata1$to_airport_id + mydata1$to_airport_elevation +mydata1$to_airport_freq +mydata1$to_runway_length+ mydata1$age + mydata1$illness + mydata1$companion_count         
                 +mydata1$total_weight+ mydata1$total_distance+ mydata1$supply +mydata1$demand + mydata1$days_until + mydata1$days_from_request + mydata1$passenger_success_ratio
           + mydata1$route_success_ratio + notreach +mydata1$leg_distance +mydata1$air_leg_count+ mydata1$total_leg_count +mydata1$num_legs_taken+ mydata1$booking_lead_time ,data=mydata1,
           nbest = 1,       # 1 best model for each number of predictors
           nvmax = NULL,    # NULL for no limit on number of variables
           force.in = NULL, force.out = NULL,
           method = "exhaustive")
summary.out <- summary(rr)
as.data.frame(summary.out$outmat)

## Adjusted R2
plot(rr, scale = "adjr2", main = "Adjusted R^2")

aa <- as.factor(mydata1$weekday_of_mission)
aaa <- as.factor(mydata1$month)

mylogit1<-glm(aa  + aa + mydata1$appt_time_type +    mydata1$from_airport_id +mydata1$from_airport_elevation + mydata1$from_airport_freq+ 
                mydata1$from_runway_length + mydata1$to_airport_id + mydata1$to_airport_elevation +mydata1$to_airport_freq +mydata1$to_runway_length+ mydata1$age + mydata1$illness + mydata1$companion_count         
              +mydata1$total_weight+ mydata1$total_distance+ mydata1$supply +mydata1$demand + mydata1$days_until + mydata1$days_from_request + mydata1$passenger_success_ratio
              + mydata1$route_success_ratio + notreach +mydata1$leg_distance +mydata1$air_leg_count+ mydata1$total_leg_count +mydata1$num_legs_taken+ mydata1$booking_lead_time ,data=mydata1,family=binomial(link="logit"))

#coefficients
#coefficients
summary(mylogit1) 

confmat<-confusion_matrix(mylogit1) 
confmat

bestglm(np, family = gaussian, IC = "BIC", t = "default", 
        CVArgs = "default", qLevel = 0.99, TopModels = 5, 
        method = "exhaustive", intercept = TRUE, weights = NULL, 
        nvmax = "default", RequireFullEnumerationQ = FALSE)

### Run Best Subsets Regression
best_subsets <- ols_step_best_subset(mylogit, details = TRUE)
best_subsets