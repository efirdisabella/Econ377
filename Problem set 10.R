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
oldsdb1 <- 2.35
newsdb1 <- oldsdb1/sqrt(4)
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
