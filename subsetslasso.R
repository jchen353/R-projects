#1a
set.seed(1)
x <- rnorm(100)
nv <- rnorm(100)
#1b
y <- 2 + 1.5*x + 1*x^2 + .5*x^3 + nv
#1c
library(leaps)
data1 <-  data.frame(y, x)
subsets <-  regsubsets(y ~ poly(x, 10, raw=TRUE), data = data1, nvmax = 10)
summary1 <-  summary(subsets)
which.min(summary1$cp)
which.min(summary1$bic)
which.min(summary1$adjr2)
library(tidyverse)
library(ggplot2)
par(mfrow=c(2, 2))
plot(summary1$cp, type="l")
points(which.min(summary1$cp), summary1$cp[3], col="red", lwd=7)
plot(summary1$bic, type="l")
points(which.min(summary1$bic), summary1$bic[3], col="red")
plot(summary1$adjr2, type="l")
points(which.min(summary1$adjr2), summary1$adjr[1], col="red")
coefficients(subsets, id=3)

#1d
# forward
frward <-  regsubsets(y ~ poly(x, 10, raw=TRUE), data = data1, nvmax = 10, method="forward")
summaryfwd <-  summary(frward)
which.min(summaryfwd$cp)
which.min(summaryfwd$bic)
which.min(summaryfwd$adjr2)

# backward
bckward <-  regsubsets(y ~ poly(x, 10, raw=TRUE), data = data1, nvmax=10, method="backward")
summarybck <-  summary(bckward)
which.min(summarybck$cp)
which.min(summarybck$bic)
which.min(summarybck$adjr2)

#plot
par(mfrow=c(3, 2))
plot(summaryfwd$cp, type="l")
points(which.min(summaryfwd$cp), summaryfwd$cp[4], col="red", lwd=7)
plot(summarybck$cp, type="l")
points(which.min(summarybck$cp), summarybck$cp[4], col="red", lwd=7)
plot(summaryfwd$bic, type="l")
points(which.min(summaryfwd$bic), summaryfwd$bic[3], col="red", lwd=7)
plot(summarybck$bic, type="l")
points(which.min(summarybck$cp), summarybck$bic[4], col="red", lwd=7)
plot(summaryfwd$adjr2, type="l")
points(which.min(summaryfwd$adjr2), summaryfwd$adjr2[1], col="red", lwd=7)
plot(summarybck$adjr2, type="l")
points(which.min(summarybck$adjr2), summarybck$adjr2[1], col="red", lwd=7)
coef(frward,which.min(summaryfwd$cp))
coef(bckward,which.min(summarybck$cp))
coef(frward,which.min(summaryfwd$bic))
coef(bckward,which.min(summarybck$bic))
coef(frward,which.min(summaryfwd$adjr2))
coef(bckward,which.min(summarybck$adjr2))

#1e
library(glmnet)
lasso <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data1)[, -1]
cv.lasso <- cv.glmnet(lasso, y, alpha = 1)
plot(cv.lasso)
names(cv.lasso)
lam <- cv.lasso$lambda.min
lam
predict(cv.lasso, s = lam, type = "coefficients")[1:11, ]
#The lasso with minimum MSE is 0.04870833.

#1f
ynew <- 2 + 7*x^7 + nv
data2 <- data.frame(ynew, x)
new1 <- regsubsets(ynew ~ poly(x, 10), data = data2, nvmax = 10)
newsummary <- summary(new1)
#plot
par(mfrow = c(1, 3))
plot(newsummary$cp, type = "l")
points(which.min(newsummary$cp), newsummary$cp[which.min(newsummary$cp)], col = "red")
plot(newsummary$bic, type = "l")
points(which.min(newsummary$bic), newsummary$bic[which.min(newsummary$bic)], col = "red")
plot(newsummary$adjr2, type = "l")
points(which.max(newsummary$adjr2), newsummary$adjr2[which.max(newsummary$adjr2)], col = "red")
coef(new1,7)
#The lasso model is the best fit
#2a
set.seed(1)
p = 20
n = 1000
x1 = matrix(rnorm(n*p),n,p)
b <- rnorm(p)
b[4] <- 0
b[8] <- 0
b[12] <- 0
b[16] <- 0
b[20] <- 0
err <- rnorm(p)
y1 <- x*b + err

#2b
training <- sample(seq(1000), 100, replace = FALSE)
testing <- -training
xtraining <- x1[training, ]
xtesting <- x1[testing, ]
ytraining <- y1[training]
ytesting <- y1[testing]

#2c
trainingmodel = regsubsets(y1 ~ ., data = training, nvmax = p)




