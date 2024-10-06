# Salary Analysis Exercise: Adapted from Chapter 4 Exercise 1
# Author: Bella Black 
# Date Created: 10.6.24
# Last Modified: 10.6.24

# A full professor of statistics is interested in estimating the third qtl 
# of salaries for full professors with 6 years of experience in that rank. 
# Data derived from the 2005-2006 Salary Report of Academic Statisticians are 
# available in ProfessorSalaries.txt.  The SampleSize variable indicates 
# how many data points were available for given years of Experience in the 
# original report. The ThirdQuartile variable explains the third quartile of 
# salaries for full professors with given years of Experience.


data  <- read.table('HW3 10.6.24/ProfessorSalaries.txt', header=TRUE)

# CALCULATIONS: xbar is weighted average of experience 
# ybar is weighted average of third quartile salary
xbar = sum(data$SampleSize*data$Experience)/sum(data$SampleSize)
ybar = sum(data$SampleSize*data$ThirdQuartile)/sum(data$SampleSize)

WSXX <- sum(data$SampleSize*(data$Experience-xbar)^2)
WSXY <- sum(data$SampleSize*(data$Experience-xbar)*(data$ThirdQuartile-ybar))
beta1hat <- WSXY / WSXX

# fit the model, predict 3rd qtl salary for profs with a certain amount of years of experience
years_exp <- 6
model_fit <- lm(ThirdQuartile~Experience, data=data, weights=SampleSize)

new_data <- data.frame(Experience = years_exp)
predicted_salary <- predict(model_fit, newdata=new_data)

# outputs
xbar
ybar
WSXX
WSXY
beta1hat
predicted_salary
summary(model_fit)
anova(model_fit)
