# Data for weekday and Sunday circulation of 89 US newspapers are
# given in circulation.txt. We are interested in predicting the Sunday 
# circulation of a newspaper that has a weekday circulation of 210,000.

circ <- read.table('HW2 9.23.24/circulation.txt', header=TRUE, sep='\t')
plot(Sunday ~ Weekday, data=circ)
model_fit <- lm(Sunday ~ Weekday, data=circ)
summary(model_fit)
abline(model_fit)
plot(model_fit)

# find leverage values greater than 4/n
n <- nrow(circ)
lev_vals <- hatvalues(model_fit)
lev_threshold <- 4 / n
high_lev_points <- which(leverage_vals > threshold)


# find  standardized residuals with absolute value greater than 2
residuals <- rstandard(model_fit)
outliers <- which(abs(residuals) > 2)

#find high leverage points that are not outliers
high_lev_not_outliers <- setdiff(high_lev_points, high_residuals)

# find cook's distance greater than 4/(n-2)
cooks_distances <- cooks.distance(model_fit)
cooks_threshold <- 4 / (n - 2)
high_cooks <- which(cooks_distances > cooks_threshold)

cat("High Leverage Points: ", high_lev_points, "\n")
cat("Outliers: ", outliers, "\n")
cat("High Leverage Points that are not Outliers: ", high_lev_not_outliers, "\n")
cat("High Cooks Distances: ", high_cooks, "\n")

