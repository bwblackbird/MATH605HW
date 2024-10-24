# Midterm Question 4
# Author: Bella Black 
# Date Created: 10.11.24
# Last Modified: 10.11.24

#load data, fit linear model, print output
revenue_data  <- read.csv('Midterm 10.11.24/fertilizer.csv', header=TRUE)
revenue_fit <- lm(log(revenue) ~ log(production_cost), data=revenue_data)

alpha <- 0.05
df <- 38
t_critical <- qt(1 - alpha/2, df)

# output
plot(log(revenue) ~ log(production_cost), data=revenue_data)
summary(revenue_fit)
t_critical
