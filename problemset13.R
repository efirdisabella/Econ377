## problem set 13 

## question 1: Under MLR 1-6; (bjhat-bj)/sd(bjhat) has a normal distribution, but (bjhat-bj)/se(bjhat)
# -> the se replaces the unknown o with the estimate ohat, which varies from sampel to sample



## question 3: a multiple regression w/ k = 3 has n = 169, how many df does tstat bj have
k <- 3
n <- 169
df <- n - k - 1
df


## question 4: find the t-statistic
bjhat <- 2.45
sebj <- 1.03
t <- bjhat/sebj
t




## question 7: reject H0 if t > c and h1 = bj>0


## question 14 find t value by looking at summary for the asked variable
library(wooldridge)
data(bwght)
reg <- lm(bwght ~ cigs + faminc, data = bwght)
summary(reg)$coefficients


## question 17







## question 20
data(hprice1)
reg <- lm(log(price) ~ log(sqrft) + bdrms, data = hprice1)
library(wooldridge)
b <- coef(reg)
logprice_hat <- b[1] + b[2]*log(2000) + b[3]*3
price_hat <- exp(logprice_hat)
price_hat
