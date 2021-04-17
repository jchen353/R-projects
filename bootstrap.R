#1a
set.seed(1)
x = rnorm(100)
y = x-2*x^2+rnorm(100)
#1b
plot(x,y)
#1ci
library(boot)
set.seed(1)
df <- data.frame(x, y)
glm1 <- glm(y ~ x)
cv.glm(df, glm1)$delta[1]
#1cii
glm2 <- glm(y ~ poly(x, 2))
cv.glm(df, glm2)$delta[1]
#1ciii
glm3 <- glm(y ~ poly(x, 3))
cv.glm(df, glm3)$delta[1]
#1civ
glm4 <- glm(y ~ poly(x, 4))
cv.glm(df, glm4)$delta[1]
#1di
set.seed(5)
glm11 <- glm(y ~ x)
cv.glm(df, glm1)$delta[1]
#1dii
glm22 <- glm(y ~ poly(x, 2))
cv.glm(df, glm2)$delta[1]
#1diii
glm33 <- glm(y ~ poly(x, 3))
cv.glm(df, glm3)$delta[1]
#1div
glm44 <- glm(y ~ poly(x, 4))
cv.glm(df, glm4)$delta[1]
#1e
#see pdf
#1f
summary(glm1)
summary(glm2)
summary(glm3)
summary(glm4)

#2a
library(MASS)
View(Boston)
u <- mean(Boston$medv)
u
#2b
stanerror <- sd(Boston$medv)/sqrt(length(Boston$medv))
stanerror
#2c
bootstrap <- function(data, index) {
  u1 <- mean(data[index])
  return(u1)
}
boot(Boston$medv, bootstrap, 1000)
#2d
confid = 22.53281 + 2*0.4090852
confid
confid1 = 22.53281 - 2*0.4090852
confid1
t.test(Boston$medv)
#2e
m<- median(Boston$medv)
m
#2f
bootstrapmed <- function(data, index) {
  m1 <- median(data[index])
  return(m1)
}
boot(Boston$medv, bootstrapmed, 1000)
#2g
u0.1 <- quantile(Boston$medv, c(.1))
u0.1
#2h
bootstrapquan <- function(data, index) {
  u0.1err <- quantile(data[index], c(.1))
  return(u0.1err)
}
boot(Boston$medv, bootstrapquan, 1000)
