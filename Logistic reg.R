#1
#1a
library(ISLR)
data("Auto")
pairs(Auto)
Auto
#1b
cor(subset(Auto, select = -name))
#1c
mlr <- lm(mpg ~ cylinders+displacement+horsepower+weight+acceleration+year+origin, data = Auto)
summary(mlr)
#There is a relationship between the predictors and the response. 
#The p-value is small, meaning that the null hypothesis is unlikely. 
#Displacement, weight, year and origin appear to have a statistically significant relationship to the response.
#The coefficient for year is 0.750773 suggests that with every additional unit of year, mpg increases by 0.750773. 
#1d
plot(mlr)
#The residual plot suggests a large amount of outliers, suggesting that there may not be a linear relationship. 
#The leverage plot identifies 14 as having high leverage. 
#1e
mlr1 <- lm(mpg ~ cylinders + displacement + cylinders * displacement + cylinders:displacement, data = Auto)
summary(mlr1)
#It appears that the interaction is statisically insignificant, with R-squared also decreasing. 
#1f
mlr2 <- lm(mpg ~ cylinders + displacement + log(horsepower) + sqrt(weight) + I(acceleration^2) + year + origin, data = Auto)
summary(mlr2)
#It appears that the new transformation have allowed a better linear relationship to form, as indicated by the p values and R-squared. 

#2 
#a
set.seed(1)
x <- rnorm(100)
#b
eps <- rnorm(100, 0, sqrt(0.25))
#c
y <- -1 + 0.5*x + eps
#d
plot(x,y)
#There appears to be a linear relationship with some outliers. 
#e
lslm <- lm(y ~ x)
summary(lslm)
#The model appears to represent the population well, with the p value of the F statisitic supporting the alternative hypothesis. 
#f
plot(y ~ x); abline(lslm, col="red"); abline(-1, 0.5, col="blue")
legend("bottomright", c("Least Square", "Population Regression"),col = c("red","blue"), lwd = 2, lty = 1:2, pt.cex = 1, cex = 0.5) 
#g
lslm1 <- lm(y ~ x + I(x^2))
summary(lslm1)
#The model appears to be similar to the model without the interaction term
#The R-squared value increased indicating a better representation.
#h
set.seed(1)
x1 <- rnorm(100)
eps1 <- rnorm(100, 0, sqrt(0.1))
y1 <- -1 + 0.5*x1 + eps1
plot(x1,y1)
ln <- lm(y1 ~ x1)
summary(ln)
plot(y1 ~ x1); abline(ln, col="red"); abline(-1, 0.5, col="blue")
legend("bottomright", c("Least Square", "Population Regression"),col = c("red","blue"), lwd = 2, lty = 1:2, pt.cex = 1, cex = 0.5) 
#The R-squared value increased, indicating better fit.
#i 
set.seed(1)
x2 <- rnorm(100)
eps2 <- rnorm(100, 0, sqrt(0.5))
y2 <- -1 + 0.5*x2 + eps2
plot(x2,y2)
mn <- lm(y2 ~ x2)
summary(mn)
plot(y2 ~ x2); abline(mn, col="red"); abline(-1, 0.5, col="blue")
legend("bottomright", c("Least Square", "Population Regression"),col = c("red","blue"), lwd = 2, lty = 1:2, pt.cex = 1, cex = 0.5) 
#the R-squared value decreased, indicating worse fit.
#j
confint(lslm)
confint(ln)
confint(mn)
#The second model appears to be covering lower values and the third model appears to be covering higher values.
#This makes sense because of the change in variance. 


