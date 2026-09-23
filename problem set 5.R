## Problem Set 
--------------------------------------------------
## In the simple linear regression model, Y= b0+b1X+U, the dependent variable is Y.
  ## The error term U represents factors other than X that affect Y
  ## The zero conditional mean assumption E[U | X] = 0 says the average of U does not depend on X
  ## under the SLR model with E[Y | X =x] = b0 + b1x

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

## X and Y have the joint probabilty distribution of ...
(4*0.2)+(4*0.3)+(6*0.5)
## find cov(x,y)= E[X,Y]- E[x]*E[Y]
x <- c(0,6,3)
y <- c(3,8,7)
pj <- c(0.2,0.3,0.5)
ex <- sum(x*pj)
ey <- sum(y*pj)
exy <- (x*y)*pj
finalexy <- sum(exy)
finalexy -(ex*ey)

## The population regression slope is b1 = cov(x,y)/var(x). When asked to find b1
x <- c(0,4,2)
y <- c(1,6,6)
pj <- c(0.2, 0.3,0.5)
ex <- sum(x*pj)
ey <- sum(y*pj)
exy <- (x*y)*pj
sum(exy)
covariance <- sum(exy)-(ex*ey)
## continued, to find Var(x), = E[X^2]-E[X]^2. Square each x, then multiply by probabilities and sum.
then solve
