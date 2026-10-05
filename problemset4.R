## problem set 4
##----------------------------------------------
## For any constant c, the expected value E[c] equals c
## If X and Y are independent, then Cov (X,Y) equals 0. 
## The correlation Cor(X,Y) always lies between -1 and 1
## The variance of a random variable X equals E[X^2] - (E[X]^2)
## compared with the covariance, the correlation is unitless and always between -1 and 1
## E[Y | X=x] is best described as the average of Y among observations with X=x.
## if x and Y are independent, then Var(X +Y) equals Var(x) + Var(Y)


## Expectation is LINEAR. E[aX+b] = aE[X] +b . Given E[X] = 7, a= 6, and b=2, find E[aX+b]
exptx <- 7
a <- 6
b <- 2
expectation <- (a * exptx) + b
expectation 

## Find E[aX+bY]= aE[X] + bE[Y]
exptx <- 4
expty <- 3
a <- 5
b <- 2
expectxy <- (a * exptx) + (b * expty)
expectxy

## Find E[aX + bY -c] 
exptx <- 1
expty <- 3
a <- 1
b <- 2
c <- 3
expectedvalue <- (a * exptx) + (b * expty) - c
expectedvalue

## Find Var(x) given E[X] and E[X^2]. 
exptx <- 4
exptx2 <- 90
variance <- exptx2 - exptx^2
variance

## Given Var(X) and a, find Var(aX + b) : property= Var(aX+b)= a^2 * VarX
varx <- 5
a <- 3
varaxb <- a^2 * varx
varaxb

## Given Var(X), find standard deviation (sd)
varx <- 19
sd <- sqrt(varx)
sd

## Given Var(x) and Var(Y), find Var(X+Y), if X and Y are independent
varx <- 5
vary <- 2
varx + vary

## Given E[XY], E[X], and E[Y], find Cov(X,Y)
EXY <- 27
ex <- 2
ey <- 5
EXY - (ex*ey)

## X and Y take the paired values, with each pair equally likely. Find Cov(x,Y) , found by EXY - Ex*EY
x <- c(3,2,2)
y <- c(0,0,1)
p <- c(1/3, 1/3, 1/3)
ex <- sum(x*p)
ey <- sum(y*p)
exy <- sum((x * y)*p)
cov <- exy - (ex*ey)
cov

## Given Cov(X,Y), a1, a2, and any constanants b, find Cov(a1X + b1, a2Y +b2)
covxy <- 5
a1 <- 4
a2 <- 2
(a1 * a2) * covxy

## Given Cov(x,y), sd(x), sd(y), find Cor(X,Y)
covxy <- -2
sdx <- 3
sdy <- 2
corxy <- covxy/(sdx * sdy)
corxy

## given Varx, Vary, Cov(x,Y), a, and b, find Var(aX+bY)
varx <- 3
vary <- 3
covxy <- 3
a <- 3
b <- 2
a^2 * varx + b^2 * vary + 2*(a*b*covxy)

## Among the observations with X=x, the Y values are given. Find E[Y | X=x]
y <- c(0,5,1,10)
mean(y)

## given X=x, Y takes a set of values with given probabilties. Find E[Y | X=x]
y <- c(7,4,7)
p <- c(0.2, 0.3, 0.5)
sum(y*p)

## conditional expectation is linear. Given E[Y|X=x], a, and b, find E[aY+b | X=x]
EyXx <- 5
a <- 4
b <- 4
a * EyXx + b
