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
n <- 6
o <- 17
sx2 <- 6
SSTx <- (n-1) * sx2
sdb1 <- o/sqrt(SSTx)
sdb1

## holding everything else fixed, which change makes Var(b1hat) smaller? answer: More spread out values of X, aka a larger SSTx.

## Under SLR1-SLR5 th error standard deviation is o= 2. a sample has the five x values, find sd(b1hat): hint find SSTx = sum of (xi-xbar)^2
x <- c(2,5,6,8,10)
o <- 2
SSTx <- sum((x - mean(x))^2)
sdb1 <- o/sqrt(SSTx)
sdb1

## A simple regression on n = 17 observations has SSR = 53. Find the estimate of the error variance, ohat2. ohat2 = SSR / (n-2)
n <- 17
SSR <- 53
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
