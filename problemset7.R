##Problem Set 7: THIS WILL BE EXACTLY LIKE YOUR EXAM. 

library(wooldridge)
data("wage1")
data("bwght")
## how many workers are in the wage 1 sample?
nrow(wage1)

## sample mean of hourly wage?
mean(wage1$wage)

## sample standard deviation
sd(wage1$educ)

## run the OLS regression of wage on education
reg <- lm(wage ~ educ, data = wage1)
b1 <- reg$coefficients[2]  ##where b1 is slope
b1

## find the OLS intercept = b0
b0 <- reg$coefficients[1]
b0

## predicted hourly wage: wagehat = b0hat + b1hat
educ <- 14
wagehat <- b0+b1 * educ
wagehat

## using saved b1, how much does the predicted hourly wage change when educ rises by 2 years? 
##### to solve above, use deltawagehat = b1 * deltaeduc
deltawage <- b1 * 2
deltawage

## SSR
SSR <- sum(reg$residuals^2)
SSR

## R^2 (use 1-SSR/SST or summary(reg)$r.squared)
SST <- sum((wage1$wage-mean(wage1$wage))^2)
1- SSR/SST
summary(reg)$r.squared  ## fact check

## using a different data set
reg2 <- lm(bwght ~ cigs, data = bwght)
b1 <- coef(reg2)["cigs"]
b1 * 5

mean_16 <- mean(wage1$wage[wage1$educ == 16])
mean_16
