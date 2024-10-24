# NYC Italian Restaurant Exercise
# Author: Bella Black 
# Date Created: 10.23.24
# Last Modified: 10.23.24


#load data, fit linear model, find prediction interval
nyc  <- read.csv('HW4 10.23.24/nyc.csv', header=TRUE)
lmfit <- lm(Price ~ Food+Decor+Service+East, data=nyc)

pred_int <- predict(lmfit, newdata=data.frame(Food=24, Decor=21, Service=22, East=1), level=0.95, interval='prediction')

#matrices
y <- nyc[,'Price']

X <- as.matrix(nyc[,c('Food', 'Decor', 'Service', 'East')])
X <- cbind(1, X)

xtx <- t(X) %*% X
xtx_inv <- solve(xtx)
xnew <- c(1, Food=24, Decor=21, Service=22, East=1)

# std error calculations 
beta_hat <- coef(lmfit)
y_hat <- sum(xnew * beta_hat)
n <- nrow(X)
p <- length(beta_hat) - 1

RSS <- sum(resid(lmfit)^2)
sigma_sqd <- RSS / (n - p - 1)

se_pred <- sqrt(sigma_sqd * (1 + xnew %*% xtx_inv %*% xnew))

# 95% confidence prediction interval
alpha <- 0.05
t_value <- qt(1 - alpha/2, df=n - p - 1)
lwr_bnd <- y_hat - t_value * se_pred
upr_bnd <- y_hat + t_value * se_pred
manual_predint <- c(lwr_bnd, upr_bnd)


# outputs
pred_int
manual_predint
