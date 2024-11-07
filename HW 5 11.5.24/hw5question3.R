# Homework 5
# Bella Black
# 11.6.24

# data
kdata  <- read.table('HW 5 11.5.24/krafft.txt', header=TRUE)

# fit the model
model_fit <- lm(KPOINT~RA+VTINV+DIPINV+HEAT, data=kdata)

# outputs
library(car)
vif(model_fit)

par(mfrow = c(2, 2))
plot(model_fit)

avPlots(model_fit)
