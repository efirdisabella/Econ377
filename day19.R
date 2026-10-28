## ECN 377 - Day 19 STARTER  |  Example 3.1;  omitted-variable bias (3.23)
## ------------------------------------------------------------------
## Type the examples along with class. Fill each ______ and ANSWER in the problems, then upload to GitHub.
## ------------------------------------------------------------------

library(wooldridge)


## ---- Example 3.1 (gpa1) ----
## SLR: colGPA on ACT (save the ACT slope)
data("gpa1")
reg <- lm(colGPA ~ ACT, data=gpa1)
b0 <- reg$coefficients[1]
b1 <- reg$coefficients[2]

## MLR: add hsGPA; change in colGPA-hat from +10 ACT, hsGPA fixed
reg1 <- lm(colGPA ~ ACT + hsGPA, data = gpa1)
b1 <- reg1$coefficients[2]
b2 <- reg1$coefficients[3]
b1 * 10

## the regression plane (first time only, in the Console: install.packages(c("car", "rgl")); Mac: rgl also needs XQuartz)
library(car)
scatter3d(colGPA ~ ACT + hsGPA, data = gpa1, fit = "linear")   # drag to rotate: flat in ACT, steep in hsGPA

## ---- Omitted-variable bias (Example 3.3, gpa1) ----
## delta1: regress hsGPA (omitted) on ACT (included)

reg2 <- lm(hsGPA ~ ACT, data= gpa1)
delta0 <- reg2$coefficients[1]
delta1 <- reg2$coefficients[2]



## check (3.23): SLR slope = MLR slope + (hsGPA slope) * delta1

b1 + b2 * delta1

## ================= PROBLEMS (your turn) =========================
## (a) colGPA-hat = 1.29 + 0.0094*ACT + 0.453*hsGPA
pred_a <- ______    # (a) colGPA at ACT = 30, hsGPA = 3.0   (hint: 1.29 + 0.0094*30 + 0.453*3.0)
## (b) b2 > 0 and delta1 > 0: SLR slope biased up or down?   ANSWER:
## (c) b2 > 0 and delta1 < 0: which way?                     ANSWER:
## (d) When is there NO omitted-variable bias?               ANSWER: