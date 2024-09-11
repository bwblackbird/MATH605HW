# Exercise 2.8.1 
# Author: Bella Black 
# Date Created: 9.10.24
# Last Modified: 9.11.24

#load data, fit linear model, print output
playbilldata  <- read.csv('HW1 9.12.24/playbill.csv', header=TRUE)
plot(CurrentWeek ~ LastWeek, data=playbilldata)
fit_pb <- lm(CurrentWeek ~ LastWeek, data=playbilldata)
summary(fit_pb)

# --- 2.8.1 (a) ---
# Find a 95% confidence interval for the slope of the regression model, β₁. 
# Is 1 a plausible value for β₁? Give a reason to support your answer.

confint(fit_pb, level=0.95)

# Yes, 1 is a plausible value for β₁ because it is
# within the 95% confidence interval [0.95, 1.01].

# --- 2.8.1 (b) ---
# Test the null hypothesis H sub0 : beta sub0 = 10000 
# against a two-sided alternative. Interpret your result.






