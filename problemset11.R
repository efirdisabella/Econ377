## uestion 1

# %chnge y
b1 <- 0.065
b2 <- -0.019 

100 * (b1 * 4) + (b2 * 3)

## wht % does predicted wge chnge when expereince rises by 2 years and tenure rises by 1, education cp
library(wooldridge)
data(wage1)
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b1 <- reg$coefficients[2]
b1
b2 <- reg$coefficients[3]
100 * (b1 * 2 + b2 * 1)
b <- reg$coefficients 
  100 * (b[3]*2 + b[4]*1)
##  uestion 3 
  exp(b[1] + b[2]*12 + b[3]*10 + b[4]*3)
  
 # 4: teh smple verge of the residuls is 0, the smple covrince between ech xj nd the residuls is 0, the regression pssses through the point of the smple mens Y= b0ht + b1hataa * x11 ...bk * xk
  
  ## 6 : ybar= b0 + b1 * x1 + b2 * x2
  b1 <- 2.1
  b0 <- 5.6
b2 <- -1.9  
x1 <- 7.3 
x2 <- 4.6
b0 + (b1 * x1) + (b2 * x2)

## 7 
#[x] wage on income in dollars and income in thousands of dollars, 
#a regression with 5 independent variables, estimated on 4 observations
# voteA on expendA, expendB and totexpend, where totexpend = expendA + expendB

## 9: Which situation makes MLR4, , fail (so that the variables are endogenous) Hint: MLR4 can fail because of omitted variables or the wrong functional form.
# ^^^A variable that affects Y and is correlated with x1 is left out of the regression.

## 11 
beta_tilde1 <- 1.53
beta_hat2 <- 1.46
delta_tilde1 <- -0.75

beta_hat1 <- beta_tilde1 - (beta_hat2 * delta_tilde1)
round(beta_hat1, 2)

## 14
reg_multi <- lm(bwght ~ cigs + faminc, data = bwght)
beta_hat_2 <- coef(reg_multi)["faminc"]
reg_aux <- lm(faminc ~ cigs, data = bwght)
delta_tilde_1 <- coef(reg_aux)["cigs"]
beta_hat_2 * delta_tilde_1

# 16