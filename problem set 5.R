## Problem Set 


## The OLS slope is b^1= cov(x,y)/var(x) 
-7/5

## The OLS intercept is b^0 = ybar - b^1 * xbar 
3-(6/10)*14

## For the sample 
x <- (c(5,4,6))
y <- (c(7,7,9))
## cnt.,  find the OLS slope: B^1 = 
cov(x,y)/var(x)
## or (?) b^1 = sigma (xi - xbar) * (yi - ybar) / sigma (xi - xbar)^2

## b^0 = ybar - b^1 * xbar
y <- c(8,8,6)
x <- c(6,5,4)
ybar <- mean(y)
xbar <- mean (x)
bhat1 <- cov(x,y)/var(x)
bhat0 <- ybar - (bhat1 * xbar)

## a fitted regression line is y^ = b^0 + (b^1 * x)
bhat0 <- 1
bhat1 <- (9/10)
x <- 10
ybar <- bhat0 + bhat1 * x

## Find the OLS line and predict y^ at given x
x <- (c(2,2,4))
y <- (c(9,2,5))
xbar <- mean(x)
ybar <- mean (y)
## step 2: calculate the slope
bhat1 <- cov(x,y)/var(x)
bhat0 <- ybar - bhat1 * 8
yhat <- bhat0 +bhat1 *xbar
## this is chat's code 
 x <- c(2, 2, 4)
 y <- c(9, 2, 5)
 model <- lm(y ~ x)
 predict(model, newdata = data.frame(x = 8))
 as.numeric(predict(model, newdata = data.frame(x = 8)))
## high school GPA and ColGPA: E[colGPA | hsGPA]
 hsGPA <- 29/10
 colGPA <- (15/10) + (3/10) *hsGPA
 ## wage estimates
 bhat1 <- 2/10
wage = b0 + b1 *educ + U
 