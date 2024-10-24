# Question 1 Calculations

beta1_est <- -8.94
beta1_se <- 35.43
alpha <- 0.05
df <- 35 

t_critical <- qt(1 - alpha/2, df)
t_value <- beta1_est / beta1_se
p_value <- pt(t_value, df, lower=FALSE)
  
# output
t_critical
t_value
p_value
