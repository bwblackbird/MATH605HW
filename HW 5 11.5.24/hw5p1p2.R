# Homework 5
# Bella Black
# 11.5.24

# data
hw5data  <- read.table('HW 5 11.5.24/HW4_2wayANOVA.txt', header=TRUE)
table <- data.frame(A=c(0,1,0,1), B=c(0,0,1,1))

#model 1 
m1fit <- lm(y~A+B, data=hw5data)
rss_m1 <- sum(residuals(m1fit)^2)
predictions_m1 <- predict(m1fit, newdata=table)

# model 2
m2fit <- lm(y~A+B+A:B, data=hw5data)
rss_m2 <- sum(residuals(m2fit)^2)
predictions_m2 <- predict(m2fit, newdata=table)

# averages
avg_y_A0_B0 <- mean(hw5data$y[hw5data$A == 0 & hw5data$B == 0])
avg_y_A1_B0 <- mean(hw5data$y[hw5data$A == 1 & hw5data$B == 0])
avg_y_A0_B1 <- mean(hw5data$y[hw5data$A == 0 & hw5data$B == 1])
avg_y_A1_B1 <- mean(hw5data$y[hw5data$A == 1 & hw5data$B == 1])

# model 3 
m3fit <- lm(y~I(A+B), data=hw5data)
rss_m3 <- sum(residuals(m3fit)^2)
predictions_m3 <- predict(m3fit, newdata=table)

# M1 outputs
summary(m1fit)
rss_m1
predictions_m1

# M2 outputs
summary(m2fit)
rss_m2
predictions_m2

# averages outputs
avg_y_A0_B0
avg_y_A1_B0
avg_y_A0_B1
avg_y_A1_B1

# anova
anova(m1fit, m2fit)

# M3 outputs
summary(m3fit)
rss_m3
predictions_m3

