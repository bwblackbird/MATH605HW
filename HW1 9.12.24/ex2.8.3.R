# Exercise 2.8.3
# Author: Bella Black 
# Date Created: 9.10.24
# Last Modified: 9.11.24

#load data, fit linear model, print output
invoicedata  <- read.table('HW1 9.12.24/invoices.txt', header=TRUE)
plot(Time ~ Invoices, data=invoicedata)
fit_i <- lm(Time ~ Invoices, data=invoicedata)
summary(fit_i)

# construct confidence interval
confint(fit_i, level=0.95)

# extract beta0, beta1, standard errors
coef_beta0 <- coef(summary(fit_i))["(Intercept)", "Estimate"]
coef_beta1 <- coef(summary(fit_i))["Invoices", "Estimate"]
se_beta0 <- coef(summary(fit_i))["(Intercept)", "Std. Error"]
se_beta1 <- coef(summary(fit_i))["Invoices", "Std. Error"]

# compute the test statistic T = (intercept - 0.01) / std. error
beta1_null <- 0.01
t_statistic <- (coef_beta1 - beta1_null) / se_beta1
t_statistic

# degrees of freedom
df <- fit_i$df.residual

# two-tailed p-value
p_value <- 2 * (1 - pt(abs(t_statistic), df))
p_value

# plot fitted regression line
abline(fit_i)

# construct 95% prediction interval for 130 invoices
pred_point <- data.frame(Invoices = 130)
predict(fit_i, newdata=pred_point, interval="prediction", level=0.95)

# --- 2.8.3 (a) ---
# Q: Find a 95% confidence interval for the start-up time, i.e., β₀.
# A: [0.39, 0.89]

# --- 2.8.3 (b) ---
# Suppose that a best practice benchmark for the average processing time 
# for an additional invoice is 0.01 hours (or 6 minutes).
# Test the null hypothesis H₀ : β₁ = 0.01 against a two-sided alternative. 
# Q: Interpret your result.
# A: The p-value is 0.13. This is slightly greater than alpha = 0.05, 
# so we fail to reject the null hypothesis. 

# --- 2.8.3 (c) --- 
# Q: Find a point estimate and a 95% prediction interval for the time 
# taken to process 130 invoices. 
# A: Point estimate: 2.11, 95% prediction interval: [1.42, 2.80]