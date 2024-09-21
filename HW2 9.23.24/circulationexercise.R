# Data for weekday and Sunday circulation of 89 US newspapers are
# given in circulation.txt. We are interested in predicting the Sunday 
# circulation of a newspaper that has a weekday circulation of 210,000.

circ <- read.table('HW2 9.23.24/circulation.txt', header=TRUE, sep='\t')
plot(Sunday ~ Weekday, data=circ)
model <- lm(Sunday ~ Weekday, data=circ)
summary(model)
abline(model)
plot(model)

