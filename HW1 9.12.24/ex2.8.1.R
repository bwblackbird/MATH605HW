# Exercise 2.8.1 
# Author: Bella Black 
# Date Created: 9.10.24
# Last Modified: 9.11.24

#load data, fit linear model, print output
playbilldata  <- read.csv('HW1 9.12.24/playbill.csv', header=TRUE)
plot(CurrentWeek ~ LastWeek, data=playbilldata)
fit_pb <- lm(CurrentWeek ~ LastWeek, data=playbilldata)
summary(fit_pb)

# construct confidence interval
confint(fit_pb, level=0.95)[2, ]

# extract intercept, standard error
intercept <- coef(summary(fit_pb))["(Intercept)", "Estimate"]
se_intercept <- coef(summary(fit_pb))["(Intercept)", "Std. Error"]

# compute the test statistic T = (intercept - 10000) / std. error
t_value <- (intercept - 10000) / se_intercept
t_value

# degrees of freedom
df <- fit_pb$df.residual

# two-tailed p-value
p_value <- 2 * (1 - pt(abs(t_value), df))
p_value

# plot fitted regression line
abline(fit_pb)

# construct 95% prediction interval
pred_point <- data.frame(LastWeek = 400000)
predict(fit_pb, newdata=pred_point, interval="prediction", level=0.95)

# analysis of variance
anova(fit_pb)

# visualize residuals
plot(fit_pb)

# --- 2.8.1 (a) ---
# Q: Find a 95% confidence interval for the slope of the regression model, β₁.
# A: [0.95, 1.01]
# Q: Is 1 a plausible value for β₁?
# A: Yes, because it is within the 95% confidence interval.

# --- 2.8.1 (b) ---
# Test the null hypothesis H₀ : β₀ = 10000 against a two-sided alternative.
# Q: Interpret your result.
# A: The p_value is 0.75; this is much greater than alpha = 0.05, 
# so we should accept the null hypothesis. 

# --- 2.8.1 (c) ---
# Use the fitted regression model to estimate the gross box office results 
# for the current week (in $) for a production with $400,000 in gross box 
# office the previous week. 
# Q: Find a 95% prediction interval for the gross box office results. 
# A: [$359,832, $439,442]
# Q: Is $450,000 a feasible value?
# A: No, it lies outside of the 95% prediction interval.

# --- 2.8.1 (d) ---
# Some promoters of Broadway plays use the prediction rule that next week's
# gross box office results will be equal to this week's results.
# Q: Comment on the appropriateness of this rule.
# A: For the most part, this is a fine rule to follow, as there is a 
# clear correlation between the current and previous weeks. Looking at
# the plots of the residuals, we can see that all but 3 points were
# predicted well. 




