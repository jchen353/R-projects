#1

#a
library(MASS)
Boston
?Boston
dim(Boston)
#506 rows and 14 columns
#b
pairs(Boston)
#Some of the variables appear to be correlated but it is hard to tell
#c
plot(crim ~ zn, data = Boston)
cor(Boston)
#Crim appears to be associated with the other predictors.   
#d
summary(Boston$crim)
summary(Boston$tax)
summary(Boston$ptratio)
hist(Boston$crim, breaks = 2, col = "red")
hist(Boston$crim, breaks = 10)
hist(Boston$cri, breaks = 2)
highcrime <- subset(Boston, crim > 5)
nrow(highcrime)/ nrow(Boston)
#20% of the suburbs have crime rates above 5
highcrime <- subset(Boston, crim > 25)
nrow(highcrime)/ nrow(Boston)
#2% of the suburbs have crime rates above 25
hist(Boston$tax, breaks = 10)
hightax <- subset(Boston, tax > 680)
nrow(hightax)/ nrow(Boston)
#~1% of the suburbs have tax rates above 680
hightax <- subset(Boston, tax > 666)
nrow(hightax)/ nrow(Boston)
#~1% of the suburbs have tax rates above 680
hightax <- subset(Boston, tax > 600)
nrow(hightax)/ nrow(Boston)
#27% of the suburbs have tax rates above 600
hist(Boston$ptratio, breaks = 10)
highptratio <- subset(Boston, ptratio > 20)
nrow(highptratio)/ nrow(Boston)
#~40% of the suburbs have pupil teacher ratios above 600
#e
?Boston
nrow(subset(Boston, chas = 1))
nrow(subset(Boston, chas == 1))
#35 suburbs bound the Charles river
#f
summary(Boston$ptratio)
#median is 19.05 pupils per teacher
#g
summary(Boston$medv)
lowmedv <- subset(Boston, medv ==min(medv))
lowmedv
summary(Boston)
#Suburb 399 has a crime rate of ~38. This is extremely high, over the 75th percentile. Suburb 406 has a even higher crime rate, of ~68.
#The values of the other predictors are listed too, with indus, nox, age, rad, tax, ptraio, and black all being higher than the 75 percentile.
#This is not desirable, given that most of these predictors reflect that the suburb is not being cared for. The age of the buildings, high tax rates 
#oxide levels and ptratio suggest an area of poverty with limited growth oppurtunities.
#Rm and chas are below average, which is negative because having more rooms and bounding the Charles river likely means a higher property value. 
#h
rm7plus <- subset(Boston, rm > 7)
rm7plus
nrow(rm7plus)
#64 suburbs have more than 7 rooms
rm8plus <- subset(Boston, rm > 8)
nrow(rm8plus)
#13 suburbs have more than 8 rooms
summary(rm8plus)
#Suburbs that average more than 8 rooms have a higher tax rate, lower age, lower oxygen levels,
#have a higher indus, lower zn, lower ptratio, lower black population and higher rad accessibility. 

#3

#a
library(ISLR)
Auto
?Auto
regres1 <- lm(mpg ~ horsepower, data = Auto)
summary(regres1)
predict(regres1, data.frame(horsepower = 98), interval = "confidence")
predict(regres1, data.frame(horsepower = 98), interval = "prediction")
#Yes, there is a relationship between the predictor and the response. The F-statstic is greater than 1. 
#The predictor and response have a 60% Ajusted R-squared.
#The relationship is negative, higher horsepower leads to a lower mpg.
#The 95% Confidence interval for horsepower = 98 is (23.97308, 24.96108)
#The 95% Prediction interval for horsepower = 98 is (14.8094, 34.12476)
#b
plot(Auto$mpg ~ Auto$horsepower, data = Auto)
abline(regres1, col = 'red')
#c
plot(regres1)
#I dont see any problem with the fits 

