#HW6 Bella Black


lasso <- read.table('HW6 11.17.24/HW_LASSO.txt', header=TRUE, sep=',')
library(glmnet) # load the necessary package 
y <- lasso[,1] # the first column is Y 
x <- as.matrix(lasso[,-1]) # the rest of columns are the covariates 
lassoFit <- glmnet(x=x, y=y) 
plot(lassoFit) 

print(lassoFit)

coeffs_matrix3 <- as.matrix(coef(lassoFit, s=1.0770))  # 3 variables
coeffs_matrix4 <- as.matrix(coef(lassoFit, s=0.4056))  # 4 variables
significant_vars3 <- coeffs_matrix3[coeffs_matrix3 != 0, ]
significant_vars4 <- coeffs_matrix4[coeffs_matrix4 != 0, ]

# output
significant_vars3
significant_vars4

