## Properties of OLS
##1. The sum and average of residuals is always = 0
##2. the sample covariance between xn and the residuals is always =0
##3. (xbar, ybar) always on the OLS regression line


## goodness of fit = R^2
uihat = yi - yihat
yi = yihat - uihat

SST = (yi-ybar)^2
## or
SST = (n-1) * S2y  ##sample variance : (1/n-1)* sumator (yi-ybar)^2

SSE = (yihat - ybar)^2
SSE <- sum((reg$fitted.values - ybar)**2)
## or
SSE = n-1 * syhat2 ## where syhat2 represents the variance of the predicted value

SSR = uihat2 ## variance in residuals: 
SSR = Suhat2

SST = SSE + SSR

R2 = SSE/SST or 1-SSR/SST

## finding R^2
summary(reg)$r.squared

## OLS b0hat and b1hat
b0hat <- (mean(y) - mean(x)) + b1hat
sxy <- cov(x,y)
sx2 <- var(x)
b1hat <- sxy/sx2

## predicting yhat
yhat <- b0hat + b1hat * x

## predicting changed in y
changeyhat <- b1hat * changex

## standard error solving - df, sigma hat, SSR, Rj2

## degrees of freedom
n <- nrow(wage1)
df <- 526 - 2 - 1

## sigma-hat = sqrt(SSR / df)
SSR <- sum(reg$residuals^2)
sigma_hat <- sqrt(SSR/df)
sigma_hat

## SST_j for educ
SSTj <- (n-1) * var(wage1$educ)

## R_j^2: regress educ on the other x, then take the R-squared
reg2 <- lm(educ ~ exper, data = wage1)
rj2 <- summary(reg2)$r.squared

## se(educ) = sigma-hat / sqrt(SST_j * (1 - R_j^2)); does it match summary()?

sigma_hat/sqrt(SSTj * (1-rj2))

## Standard error of beta1 (sebhat)
## standard error (se(b1))
reg <- lm(wage ~ educ, data=wage1)
SSR <- sum(reg$residuals^2)
n <- nrow(wage1)
SSTx <- (n-1) * var(wage1$educ)
sigma <- sqrt(SSR / (n-2))
seb1 <- sigma / sqrt(SSTx)
seb1 <- summary(reg)$coefficients[2,2]# Row, column

# ^^^^^^or^^^^^^^^^^
# se(b1hat) = ohat/sqrt(SSTx) when you are given SSR and a vector 
# Question 8 R Code Template
x <- c(2, 3, 5, 6, 11)   # Replace with your test's X values
SSR <- 53                # Replace with your test's SSR value
n <- length(x)
sigma_hat <- sqrt(SSR / (n - 2))
sst_x <- sum((x - mean(x))^2)
se_beta1 <- sigma_hat / sqrt(sst_x)
print(round(se_beta1, 2))


## labeling coefficients
data=wage1
reg <- lm(log(wage) ~ educ + exper + tenure, data=wage1)
b0 <- reg$coefficients[1]
b1 <- reg$coefficients[2]
b2 <- reg$coefficients[3]
b3 <- reg$coefficients[4]
## predict wage at educ = 16, exper = 4, tenure = 2 (remember exp())
exp(b0 + b1 * 16 + b2 * 4 + b3 *2)

## % change in wage: +1 year educ; +1 exper and +1 tenure
100 * b1* 1 
100 * (b2 * 1 + b3 * 1)

#===================================================================
# PROBLEM SET 7                                                    =
#===================================================================
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

#========================================================
# PROBLEM SET 8
#========================================================
## regression of y on x, every y is multiplied by a number (b1 is the OLS Slope)
b1 <- 1.72
b1 * 1000

## regression of y on x, b1hat = 0.03, x values are divided by 10 what is the new OLS slope (b1hat?)
##HINT: if x is multiplied by c, b1hat is multiplied by 1/c and b0hat does not change. Dividing k is multiplying by c= 1/k
b1hat <- 0.03
c <- 1/10
b1hat * 10

## in a regression of y on x, every value of x is multiplied by a number k. The slope would be divided by that number, the int would not change, and r^2 would not change

## salary thousands -> dollars
library(wooldridge)
data("ceosal1")
reg <- lm(salary ~ sales, data = ceosal1)
b1 <- reg$coefficients[2]
b1 * 1000

## log-level regression what % does y change when x rises
## log-level: %change y = 100 * b1 * change x
b1 <- 0.056
100 * b1 * 1

## log-log regression y change when x rises
## HINT: log-log : %change y = b1hat * %changex
b1 <- 0.99
change_x <- 13
b1 * change_x

## level-log regression y change when x changes
#HINT:  level-log : change y = (b1/100) * %changex
b1 <- 38.2
(b1/100)*3

## log(rent) = 2.1 + 0.45 log(income). b1 of log-log regression?
# a 1% rise in income is associated with about an 0.45% rise in rent

## what percent does predicted wage change with k more amount experience
# %changewage = 100 * b1 * change xj
data("wage1")
reg <- lm(log(wage) ~ exper, data = wage1)
b1 <- reg$coefficients[2]
b1
100 * b1 * 10

#=================================================================
#  PROBLEM SET 9
#==============================================================
## problem set 9 
## first, the ones on the quiz: 

## a log-log regression of CEO Sal  gives log(salaryhat)= b0hat + b1hat * log(sales). when sal is changed from thousands of dollars to dollars, b1 does not change, only b0 changes

## log- level regression of log(wge ) on tenure, with tenure mesured in months, gives a slope of 0.0022. By about wht percent does wage rise with one more year of tenure?
slope <- 0.0022
deltax <- 12
percent_change <- 100 * slope * deltax

## a regression gives log(salaryhat)= 2.21+0.47*log(sales), with slry in thousands of dollars. If salary is measured in dollars instead, what is the new intercept
b0 <- 2.21 
new_int <- b0 + log(1000)
new_int

## in which model is the slope read as %changey = 100 * b1 * changex
## correct answer: log(y) = b0 + b1x + u

## a log-log regression gives log(y)^ = 3.7 + 1.36 * log(x). By about what percent does y change when x falls by 11%
## calculation: %change_y = b1hat * (%change_x)
b1 <- 1.36
changex <- -11
b1 * changex

library(wooldridge)

# Load the datasets
data("ceosal1")
data("wage1")

# ==========================================
# Question 6: Log-level regression on ceosal1
# ==========================================
# Run log-level regression: log(salary) on roe
reg6 <- lm(log(salary) ~ roe, data = ceosal1)
beta1_6 <- coef(reg6)["roe"]

# Percentage change when roe rises by 5 points (% delta y = beta_1 * delta_x * 100)
pct_change_6 <- beta1_6 * 5 * 100
cat("Question 6 Answer:", round(pct_change_6, 2), "%\n")

# ==========================================
# Question 7: Level-log regression on ceosal1
# ==========================================
# Run level-log regression: salary on log(sales)
reg7 <- lm(salary ~ log(sales), data = ceosal1)
beta1_7 <- coef(reg7)["log(sales)"]

# Change in salary when sales rise by 10% (delta y = (beta_1 / 100) * % delta x)
change_salary_7 <- (beta1_7 / 100) * 10
cat("Question 7 Answer:", round(change_salary_7, 2), "thousand dollars\n")

## theorem 2.1 says that under SLR1 - SLR4, E[b1hat]= b1 . Averaged over many random samples, b1hat equals b1. 

## in wage = b0 + b1 educ + U , which of the following is the most likely to contribute omitted-variable bias to the OLS slope on educ? 
## ^^^ the workers ability, which raises wages and tends to be higher for people with more education

## In a sample of workers, every worker has exactly 12 years of education. You regress wage on education. What goes wrong?
## ^^^ SLR3 fails: the X values are all the same ,so the OLS slope cannot be computed

## Under SLR1- SLR5, the error variance o^2 = 83 and SSTx = 27. Find Var(b1hat)
o2 <- 83
SSTx <- 27
var <- o2/SSTx
var

## Under SLR1–SLR5, the error standard deviation is o = 17$. A sample has n = 6$ observations, and the sample variance of x is Sx^2 = 6. Find sd(b1hat). Hint SSTx = (n-1)S^2x, then sd(b1hat) = o/squrt SSTx
n <- 5
o <- 8
x <- c(1,2,5,7,8)
var(x)
sx2 <- var(x)
SSTx <- (n-1) * sx2
sdb1 <- o/sqrt(SSTx)
sdb1

## holding everything else fixed, which change makes Var(b1hat) smaller? answer: More spread out values of X, aka a larger SSTx. A larger error variance (o2) makes it LARGER.

## Under SLR1-SLR5 th error standard deviation is o= 2. a sample has the five x values, find sd(b1hat): hint find SSTx = sum of (xi-xbar)^2
x <- c(2,5,6,8,10)
o <- 2
SSTx <- sum((x - mean(x))^2)
sdb1 <- o/sqrt(SSTx)
sdb1

## A simple regression on n = 17 observations has SSR = 53. Find the estimate of the error variance, ohat2. ohat2 = SSR / (n-2)
n <- 36
SSR <- 691
o2hat <- SSR/(n-2)
o2hat

# ==========================================
# Question 18: Standard error of regression (wage1)
# ==========================================
# Run regression of wage on educ
reg18 <- lm(wage ~ educ, data = wage1)

# sigma_hat is the residual standard error
sigma_hat_18 <- summary(reg18)$sigma
cat("Question 18 Answer:", round(sigma_hat_18, 2), "\n")


# ==========================================
# Question 19: Total Sum of Squares for educ (SST_X)
# ==========================================
# SST_X = sum of squared deviations from the mean of educ
sst_x_19 <- sum((wage1$educ - mean(wage1$educ))^2)
cat("Question 19 Answer:", round(sst_x_19, 2), "\n")

## problem set 8: 14,17,19

# Question 14: Under SLR1–SLR5, the error standard deviation is sigma = 2.
# A sample has the five X values: x = (2, 5, 6, 8, 10).
# Find sd(beta_hat_1) (the standard deviation of the OLS slope estimator).

# Define the X values
x <- c(2, 5, 6, 8, 10)

# Error standard deviation (sigma)
sigma <- 2

# Calculate SST_X (Total Sum of Squares of X)
sst_x <- sum((x - mean(x))^2)

# Calculate the standard deviation of beta_hat_1
sd_beta1 <- sigma / sqrt(sst_x)

# Print the result rounded to 2 decimal places
cat("sd(beta_hat_1):", round(sd_beta1, 2), "\n")

# ==========================================
# Question 17: Homoskedasticity (SLR5) & Variance Formula
# ==========================================
# Question 17 is a conceptual question about whether SLR5 (homoskedasticity) fails 
# when the spread of food spending around the regression line grows as income grows.
# 
# Answer choice: 
# "SLR5 (homoskedasticity) fails, so the variance formula of Theorem 2.2, 
# Var(beta_hat_1) = sigma^2 / SST_X, no longer applies (OLS is still unbiased)."
#
# No code needed for Question 17 as it's multiple choice, but here is a quick 
# check in R to demonstrate heteroskedasticity if you want to see it:

library(wooldridge)
data("ceosal1")

# You can test for heteroskedasticity using residuals squared:
reg <- lm(salary ~ roe, data = ceosal1)
res_sq <- residuals(reg)^2
het_test <- lm(res_sq ~ roe, data = ceosal1)
summary(het_test)


# ==========================================
# Question 19: Total Sum of Squares for X (SST_X)
# ==========================================
# Under SLR1–SLR5, calculate SST_X given a specific sample of X values, 
# or using a dataset column (e.g., educ in wage1).

# Method A: If you have a specific vector of X values (similar to Question 14)
x_values <- c(2, 5, 6, 8, 10) # Replace with your specific X values if needed
sst_x_custom <- sum((x_values - mean(x_values))^2)
cat("SST_X for custom values:", round(sst_x_custom, 2), "\n")

# Method B: Using the wage1 dataset column 'educ' (from Wooldridge package)
sst_x_wage1 <- sum((wage1$educ - mean(wage1$educ))^2)
cat("SST_X for wage1 (educ):", round(sst_x_wage1, 2), "\n")



## in a regression of y on x, the OLS slope is b1hat. Every value of x is then divided by 1000
b1 <- 0.36
b1 * 1000

# ===========================================================================================
# PROBLEM SET 10
# =======================================================================================
## problem set 10
# 1,2,3,4,6,7,9,12,13,14,15

## QUESTION 1: A regression gives test score hat = 12 + 8 *log(income) What does the * mean?
# ^ a 1% rise in income is associated with a rise of about 0.08 points in the predicted test score
# log-level model: Y = b0 + b1 ln(x) -> to interpret b1, we use delta Y = (b1/100) * %changeX
# So, 8/100 * 1 = 0.08 points

##QUESTION 2: A log-level regression gives log(y) hat = 1.8+0.06x. By what percent does y change when x falls by 1? 
# hint: log-level= %change_y = 100 * b1hat * change_x, and a fall means deltax is negative

b1 <- 0.06
deltay <- 100 * b1 * -1
deltay

## QUESTION 3: Run the log-log regression. By what percent does predicted birth weight change when family income rises by 20%? 
library(wooldridge)
data("bwght")
reg <- lm(log(bwght)~log(faminc), data=bwght)
b1 <- reg$coefficients[2]
changeinc <- 20
changey <- b1 * changeinc
changey

## QUESTION 4: Suppose you could draw 1000 diff. random samples from teh same population and compute b1hat in each one. if OLS is unbiased, which statement is correct?
# The average of the 1000 estimates is close to b1, because E[b1hat]= b1 under SLR

## QUESTION 5: Why do we prefer an unbiased estimator with a smaller variance Var(b1hat)?
# it estimates land closer to b1 from sample to sample, so any one estimate is more reliable.

## QUESTION 6: Under SLR1-5, Var(b1hat)= o2/SSTx. Holding everything else fixed, whihc changes would make Var(b1hat) LARGER. 
# a smaller sample, X values bunched more tightly together (a smaller SSTx), and a larger error variance of o^2. 

## QUESTION 7: a simple regression on n=21 observations has ohat = 7.9, and the sample SD of x is Sx= 0.6. Find standard error se(b1hat)
# SSTx = (n-1) S^2x (square the standard deviation) then se(b1hat)= ohat/sqrt(SSTx)
n <- 21
ohat <- 7.9
sx <- 0.6
SSTx <- (n-1) * sx^2
seb1 <- ohat/sqrt(SSTx)
seb1

## QUESTION 9: under SLR, a regression has sd(b1hat) = 2.35. If the sampel were 4x as large, wth the x values spread out in the same way, what would sd(b1hat) be? 
# hint: sd(b1hat)= o/sqrt(SSTx). multiplying SSTx by k divides the sd by squrt k, NOT k
oldsdb1 <- 0.78
newsdb1 <- oldsdb1/sqrt(2)
newsdb1

## QUESTION 12: a multiple regression gives yhat = -1 + 0.9x1 + 2.3x2. Find the predicted value yhat at x1= 5 and x2=20 . 
b1 <- 0.9
b0 <- -1
x1 <- 5
b2 <- 2.3
x2 <- 20
b0 + (b1 * x1) + (b2 * x2)

## QUESTION 13 : a multiple linear regression gives yhat = 2 + 2.2x1 - 0.6x2. If x1 rises by 6 and x2 rises by 4, by how much does yhat change? 
# hint: changey = b1 * changex1 + b2 * change x2
2.2 * (6) + -0.6 * (4)

## QUESTION 14: what is the predicted hourly wage of a worker with 14 years of education, 10 years of experience, and 5 years of tenure? 
data(wage1)
reg <- lm(wage ~ educ + exper + tenure, data= wage1)
b <- reg$coefficients[1]
b2 <- reg$coefficients[2]
b3 <- reg$coefficients[3]
b4 <- reg$coefficients[4]
b + (b2 * 14) + (b3 * 10) + (b4 * 5)

## QUESTION 15:  Using the same regression of wage of educ, exper, and tenure, by how much does the predicted hourly wage change when tenure rises by 3 years, holding education and expereince fixed. 
# hint: change_wage = b3 * changetenure 
b4 * 3

## MLR log-log predicted salary 
data(ceosal1)
# Fit the log-log-level regression model
reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
# Predict salary for sales = 5000 and roe = 15
b <- coef(reg)
pred_salary <- exp(b[1] + b[2] * log(5000) + b[3] * 15)
print(pred_salary)



## MULTIPLE REGRESSION OF WAGE ON EDUC AND EXPER (b2 * o1)
reg_mult <- lm(wage ~ educ + exper, data = wage1)
# Regression of omitted variable (exper) on included variable (educ)
reg_aux <- lm(exper ~ educ, data = wage1)
# Compute beta_2 * delta_tilde_1 (the omitted-variable bias component)
beta_2 <- coef(reg_mult)["exper"]
delta_tilde_1 <- coef(reg_aux)["educ"]
ans_21 <- beta_2 * delta_tilde_1
print(ans_21)

# =============================================================================
# PROBLEM SET 11
# =============================================================================
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

## 6 : ybar= b0 + b1 * x1 + b2 * x2 sample mean ybar
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

## 11 MLR slope
beta_tilde1 <- 1.53
beta_hat2 <- 1.46
delta_tilde1 <- -0.75

beta_hat1 <- beta_tilde1 - (beta_hat2 * delta_tilde1)
round(beta_hat1, 2)

## 14 b2hat * delta tide 1
reg_multi <- lm(bwght ~ cigs + faminc, data = bwght)
beta_hat_2 <- coef(reg_multi)["faminc"]
reg_aux <- lm(faminc ~ cigs, data = bwght)
delta_tilde_1 <- coef(reg_aux)["cigs"]
beta_hat_2 * delta_tilde_1


## SSTj find
# Given: n and the sample variance of x_j
n     <- 8
var_x <- 0.55
SST_j <- (n - 1) * var_x
SST_j

# Given: n, sum of x, and sum of x squared
n    <- 8
sum_x  <- 39.2
sum_x2 <- 194.4
SST_j <- sum_x2 - (sum_x^2) / n
SST_j

## if you are given R2 and want coefficient variance
sigma2 <- 1.8      # error variance (or its estimate)
R2_j   <- 0.35     # R-squared from regressing x_j on the other predictors
SST_j  <- 3.9      # given or computed above

var_beta_j <- sigma2 / (SST_j * (1 - R2_j))
se_beta_j  <- sqrt(var_beta_j)
var_beta_j; se_beta_j

# ===============================================
# PROBLEM SET 12 - EXAM PREP
# ===============================================

## $Rj^2$ measures how well the other independent variables explain $X_j$ (exper), excluding the dependent variable ($\text{wage}$). Thus, you regress exper on educ and tenure.
# ^ The regression lm(exper ~ educ + tenure) would give R^2j

## find the standard error of the regression ohat.
# HINT: df = n - K - 1 , then ohat = sqrt(SSR/df)
n <- 37
k <- 2
SSR <- 2938
df <- n - k - 1
ohat <- sqrt(SSR/df)
ohat

## find se(bjhat)
ohat <- 11.5
SSTj <- 329
rj2 <- 0.34
sebj1 <- ohat/sqrt(SSTj * (1-rj2))
sebj1

## standard error of regression, ohat = sqrt(SSR/df)
data("hprice1")
reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
n <- nrow(hprice1)
k <- 3 ## number of independent variables in the regression, in this case 3
df <- n - k -1
SSR <- sum(reg$residuals^2)
sigma_hat <- sqrt(SSR/df)
sigma_hat
SSTj <- sum((hprice1$bdrms - mean(hprice1$bdrms))^2)
R2j <- summary(lm(bdrms ~ sqrft + lotsize, data = hprice1))$r.squared
sebj1 <- sigma_hat/sqrt(SSTj * (1 - R2j))
sebj1

## sanity check for above (bruh literally just do this fuck doing all the math)
summary(reg)$coefficients[3, 2]
summary(reg) ## Residual standard error on x degrees of freedom = ohat

## find SSTj
n <- 106
sxj <- 2.3    ## sample standard deviation
(n-1)* sxj^2

## as Rj2 gets closer to 1, se(bjhat) grows without bound, aka multicollineraity

## finding yihat and SSE
yihat <- reg$fitted.values
SSE <- sum((reg$fitted.values - ybar)**2)

# b0 Simple regression, given values
sigma2_hat <- 1.8
n          <- 8
x_bar      <- 4.9
SST_1      <- 3.9

var_b0_simple <- sigma2_hat * (1/n + x_bar^2 / SST_1)
var_b0_simple          # about 11.31
sqrt(var_b0_simple)    # standard error

# b0 Multiple regression, given the design matrix X (first column = 1s)
X <- cbind(1,
           c(4.2, 5.1, 3.8, 6.0, 5.5, 4.9, 5.3, 4.4),
           c(1.0, 2.1, 0.8, 2.9, 2.2, 1.9, 2.4, 1.2))

vcov_beta  <- sigma2_hat * solve(t(X) %*% X)
var_b0_mult <- vcov_beta[1, 1]
var_b0_mult

## DELTA TIDE 1 SOLVE
# Given data
x1 <- c(4.2, 5.1, 3.8, 6.0, 5.5, 4.9, 5.3, 4.4)
x2 <- c(1.0, 2.1, 0.8, 2.9, 2.2, 1.9, 2.4, 1.2)

# delta tilde 1: slope of x2 regressed on x1
delta1_tilde <- sum((x1 - mean(x1)) * (x2 - mean(x2))) / sum((x1 - mean(x1))^2)
delta1_tilde

# Equivalent
cov(x1, x2) / var(x1)
coef(lm(x2 ~ x1))[2]

# If given beta1_hat and beta2_hat from the multiple regression
beta1_hat <- 0.8
beta2_hat <- 1.5
beta1_tilde <- beta1_hat + beta2_hat * delta1_tilde
beta1_tilde          # the (biased) simple regression slope
beta2_hat * delta1_tilde   # the bias term


# ==============================================================
# OLS BIAS (OVB) & VARIANCE TOOLKIT: works from GIVEN values
# Notation: hat = full model, tilde = model with variable omitted
# ==============================================================

# ---------- 0. HELPER FUNCTIONS ----------
sst      <- function(x) sum((x - mean(x))^2)                     # SST_j
delta1   <- function(x1, x2) sum((x1 - mean(x1)) * (x2 - mean(x2))) / sst(x1)  # slope of x2 on x1
sigma2_hat <- function(SSR, n, k) SSR / (n - k - 1)              # k = # of slopes (excl. intercept)

# ==============================================================
# PART A: OMITTED VARIABLE BIAS
# True model: y = b0 + b1*x1 + b2*x2 + u
# Omit x2 and regress y on x1 only -> slope is beta1_tilde
# beta1_tilde = beta1_hat + beta2_hat * delta1_tilde
# ==============================================================

# --- A1. Given beta1_hat, beta2_hat, delta1_tilde ---
beta1_hat    <- 0.8
beta2_hat    <- 1.5
delta1_tilde <- 0.6

beta1_tilde <- beta1_hat + beta2_hat * delta1_tilde
bias        <- beta2_hat * delta1_tilde        # = beta1_tilde - beta1_hat
beta1_tilde
bias

# --- A2. Direction of bias (uses SIGNS of beta2 and delta1) ---
ovb_direction <- function(beta2, delta1) {
  if (beta2 == 0 || delta1 == 0) return("No bias")
  if (beta2 * delta1 > 0) "Upward bias (overestimates beta1)" else "Downward bias (underestimates beta1)"
}
ovb_direction(beta2_hat, delta1_tilde)
ovb_direction(-2, 0.5)     # negative * positive -> downward

# --- A3. Given raw data for x1 and x2: get delta1_tilde ---
x1 <- c(4.2, 5.1, 3.8, 6.0, 5.5, 4.9, 5.3, 4.4)
x2 <- c(1.0, 2.1, 0.8, 2.9, 2.2, 1.9, 2.4, 1.2)

delta1(x1, x2)
cov(x1, x2) / var(x1)             # same thing
coef(lm(x2 ~ x1))[2]              # same thing

# --- A4. Given summary stats (cov, var) instead of raw data ---
cov_x1x2 <- 0.9
var_x1   <- 0.55
cov_x1x2 / var_x1                 # delta1_tilde

# --- A5. Given correlation and SDs instead ---
r12 <- 0.85; sd_x1 <- 0.74; sd_x2 <- 0.70
r12 * sd_x2 / sd_x1               # delta1_tilde = corr * sd(x2)/sd(x1)

# --- A6. Given beta1_tilde and beta2_hat, delta1_tilde: back out beta1_hat ---
beta1_tilde_given <- 1.7
beta1_tilde_given - beta2_hat * delta1_tilde    # beta1_hat

# --- A7. Which variable is omitted? Bias in the opposite direction ---
# Same formula applies if you swap roles: bias = (effect of omitted on y) * (slope of omitted on included)

# ==============================================================
# PART B: VARIANCE OF OLS ESTIMATES: SIMPLE REGRESSION
# ==============================================================

# --- B1. Estimate sigma^2 from SSR (df = n - 2) ---
n   <- 8
SSR <- 10.8
s2  <- sigma2_hat(SSR, n, k = 1)     # 10.8 / 6 = 1.8
s2
sqrt(s2)                             # "standard error of the regression" (sigma_hat)

# --- B2. Var and SE of slope ---
SST_1 <- sst(x1)                     # or given directly
var_b1 <- s2 / SST_1
se_b1  <- sqrt(var_b1)
var_b1; se_b1

# --- B3. Var and SE of intercept ---
x_bar  <- mean(x1)
var_b0 <- s2 * (1/n + x_bar^2 / SST_1)
# equivalent: s2 * mean(x1^2) / SST_1
se_b0  <- sqrt(var_b0)
var_b0; se_b0

# --- B4. Covariance between b0 and b1 ---
cov_b0b1 <- -s2 * x_bar / SST_1

# --- B5. If given Var(x) instead of SST: SST = (n-1) * var(x) ---
var_x <- 0.55
(n - 1) * var_x

# --- B6. Theoretical (population) version: sigma^2 known ---
sigma2 <- 2.0
sigma2 / SST_1                       # Var(b1_hat | X)

# ==============================================================
# PART C: VARIANCE: MULTIPLE REGRESSION
# Var(bj_hat) = sigma^2 / [SST_j * (1 - R_j^2)]
# ==============================================================

# --- C1. Given sigma2, SST_j, R_j^2 ---
sigma2_val <- 1.8
SST_j      <- 3.9
R2_j       <- 0.35

var_bj <- sigma2_val / (SST_j * (1 - R2_j))
se_bj  <- sqrt(var_bj)
var_bj; se_bj

# --- C2. VIF = 1 / (1 - R_j^2)  ->  Var(bj) = sigma2 * VIF / SST_j ---
VIF <- 1 / (1 - R2_j)
VIF
sigma2_val * VIF / SST_j             # same as var_bj

# --- C3. Sigma2 estimated with k regressors ---
n <- 50; k <- 3; SSR <- 94.2
s2_mult <- SSR / (n - k - 1)
s2_mult

# --- C4. Given SE of beta_j and want R_j^2 or SST_j: rearrange ---
se_given <- 0.45
# R_j^2 = 1 - sigma2 / (SST_j * se^2)
1 - sigma2_val / (SST_j * se_given^2)

# --- C5. Compare simple vs multiple variance ---
#   Var(b1_tilde) = sigma^2 / SST_1          (omitted x2, smaller variance, but biased)
#   Var(b1_hat)   = sigma^2 / [SST_1(1-R^2)] (includes x2, unbiased, larger variance)
#   The true sigma^2 is smaller in the full model if x2 matters, so it isn't always a pure tradeoff.
sigma2_val / SST_j                   # tilde
sigma2_val / (SST_j * (1 - R2_j))    # hat (always >= tilde for same sigma2)

# --- C6. R_j^2 from data: regress x_j on the OTHER regressors ---
# summary(lm(x1 ~ x2 + x3, data = df))$r.squared

# ==============================================================
# PART D: MATRIX FORM  Var(beta_hat) = sigma^2 (X'X)^-1
# ==============================================================

X <- cbind(1, x1, x2)                # first column = intercept
colnames(X) <- c("(Intercept)", "x1", "x2")
n <- nrow(X); k <- ncol(X) - 1

sigma2_given <- 1.8                  # or estimated
V <- sigma2_given * solve(t(X) %*% X)
V                                    # full variance-covariance matrix
diag(V)                              # variances of b0, b1, b2
sqrt(diag(V))                        # standard errors
V["x1", "x1"]                        # Var(b1_hat) alone

# --- D2. If given y too: compute everything from scratch ---
y <- c(3.1, 4.9, 2.8, 6.5, 5.3, 4.6, 5.8, 3.6)
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y     # OLS coefficients
resid    <- y - X %*% beta_hat
SSR      <- sum(resid^2)
s2_hat   <- SSR / (n - k - 1)
V_hat    <- s2_hat * solve(t(X) %*% X)
beta_hat
s2_hat
sqrt(diag(V_hat))                    # SEs

# ==============================================================
# PART E: CHECK ANSWERS WITH lm() ON RAW DATA
# ==============================================================

df  <- data.frame(y = y, x1 = x1, x2 = x2)
fit <- lm(y ~ x1 + x2, data = df)          # full model (hat)
short <- lm(y ~ x1, data = df)             # omitted x2 (tilde)

summary(fit)
vcov(fit)                                  # variance-covariance matrix
sqrt(diag(vcov(fit)))                      # SEs
sigma(fit)^2                               # sigma2_hat

# Verify the OVB identity: beta1_tilde = beta1_hat + beta2_hat * delta1_tilde
b_tilde <- coef(short)["x1"]
b_hat   <- coef(fit)["x1"]
b2_hat  <- coef(fit)["x2"]
d1      <- coef(lm(x2 ~ x1, data = df))["x1"]

c(beta1_tilde  = unname(b_tilde),
  identity_rhs = unname(b_hat + b2_hat * d1))   # these should match exactly

# R_j^2 and VIF for x1 in the full model
R2_1 <- summary(lm(x1 ~ x2, data = df))$r.squared
1 / (1 - R2_1)                             # VIF

# Verify Var(b1_hat) formula against vcov()
sigma(fit)^2 / (sst(df$x1) * (1 - R2_1))
vcov(fit)["x1", "x1"]                      # should match

# ==============================================================
# QUICK REFERENCE
# ==============================================================
# SST_j            = sum((x - mean(x))^2) = (n-1)*var(x)
# sigma2_hat       = SSR / (n - k - 1)
# Var(b1) simple   = sigma2 / SST_1
# Var(b0) simple   = sigma2 * (1/n + xbar^2 / SST_1)
# Var(bj) multiple = sigma2 / [SST_j (1 - R_j^2)]
# VIF_j            = 1 / (1 - R_j^2)
# delta1_tilde     = Cov(x1,x2) / Var(x1)
# beta1_tilde      = beta1_hat + beta2_hat * delta1_tilde
# OVB              = beta2 * delta1  (sign = sign(beta2) * sign(delta1))