#1a
data <- read.csv("/Users/jonathanchen/Downloads/data1_3.csv")
data
regres <- lm(Y ~ X1, data = data)
summary(regres)
#1b
plot(data$Y ~ data$X1, data = data)
abline(regres, col = 'red')
#1c
par(mfrow = c(2, 2))
plot(regres)
#the residuals appear to be heteroskedastic, the red line is slightly curved, 
#and the residuals seem to change directions as the fitted Y values increases.
lmtest::bptest(regres)
#The Breusch-Pagan test is suitable because it tests for heteroskedascity. 
#the Breusch-Pagan test gives us a p-value less than a significance level of 0.05,
#we can reject the null hypothesis. Heteroskedascity is present.
library(car)
coefs <- names(coef(regres))
linearHypothesis(regres, coefs[1], test = "Chisq")
#the Chi-Square test is better for testing for independence
#The p-value is less than the significance level of 0.05
#we can reject the null hypothesis, the slope coefficient is not equal to 0.
linearHypothesis(regres, coefs[1], test = "F")
#The F-test supports our finding from the Chi-Sqaure test.
#1d
regres1 <- lm(Y ~ X1+X2+X3+X4+X5+X6, data = data)
summary(regres1)
par(mfrow = c(2, 2))
plot(regres1)
#1e
#The second model appears to fit the data better. Graphically, the model appears
#to approximate the data better. 
#1f
library(caret)
model <- train(Y ~ X1, data = data, method = "lm", trControl = train.control)
model
#I selected K-fold because it can give more accurate estimates of the error than LOOCV.
#The lower the RMSE, the better the model. As we can see, the RMSE is very high, 
#meaning that this model is not very good. 
train.control <- trainControl(method = "cv", number = 10)
model1 <- train(Y ~., data = data, method = "lm", trControl = train.control)
model1
#The RMSE is lower for this model, meaning that this model is a better fit.


#2a
data1 <- read.csv("/Users/jonathanchen/Downloads/data2_3.csv")
data1[1:5,]
logisreg <- glm(Y ~ X1 + X2, data = data1, family = binomial)
summary(logisreg)
cmatrix <- predict(logisreg, type = "response")
c <- ifelse(cmatrix > 0.5, "Up", "Down")
table(c, data1$Y)
correct <- (885+4)/(885+108+3+4)
wrong <- 1 - correct
wrong

library(ROCR)
glm.probs <- predict(logisreg, type = "response")
glm.probs[1:5]

pred1 <- prediction(glm.probs, data1$Y) 
perf_test <- performance(pred1,"tpr","fpr") 
plot(perf_test,colorize=TRUE)
abline(coef = c(0,1))

#2b
library(MASS)
lda.fit <- lda(Y ~ X1 + X2, data = data1)
lda.fit
ldapred1 <- predict(lda.fit, type = "response")
ldaclass <- ldapred1$class
table(ldaclass, data1$Y)
mean(ldaclass == data1$Y)

lda.probs <- predict(lda.fit, type = "response")
pred.lda <- prediction(lda.probs$posterior[,2], data1$Y)

perf.lda <- performance(pred.lda, "tpr", "fpr") 
plot(perf.lda, col="red")
abline(coef = c(0,1))

#2c
qda.fit <- qda(Y ~ X1 + X2, data = data1)
qda.fit
qdapred1 <- predict(qda.fit, type = "response")
qdaclass <- qdapred1$class
table(qdaclass, data1$Y)
mean(qdaclass == data1$Y)

qda.probs <- predict(qda.fit, type = "response")
pred.qda <- prediction(lda.probs$posterior[,2], data1$Y)

perf.qda <- performance(pred.qda, "tpr", "fpr") 
plot(perf.qda, col="red")
abline(coef = c(0,1))

#2d
library(tidyverse)
set.seed(123)
summary(data1)

trainingdata <- data1[data1$X1 < 0.37,]
testingdata <- data1[data1$X1 >= 0.37,]

dim(trainingdata)
dim(testingdata)
dim(data1)

logisreg1 <- glm(Y ~ X1 + X2, data = trainingdata, family = binomial)
summary(logisreg1)
predictTest <- predict(logisreg1, newdata = testingdata, type = "response")
predictclass <- ifelse(predictTest > 0.5, "Up", "Down")
table(predictclass, testingdata$Y)

glm.probs1 <- predict(logisreg1, type = "response")
glm.probs1[1:5]

pred2 <- prediction(glm.probs1, trainingdata$Y) 
perf_test1 <- performance(pred2,"tpr","fpr") 
plot(perf_test1,colorize=TRUE)
abline(coef = c(0,1))

newlda <- lda(Y ~ X1 + X2, data = trainingdata)
ldapredic <- predict(newlda, testingdata)
ldaclass1 <- ldapredic$class
table(ldaclass1, testingdata$Y)
mean(ldaclass1 == testingdata$Y)

lda.probs1 <- predict(newlda, type = "response")
pred1.lda <- prediction(lda.probs1$posterior[,2], trainingdata$Y)

perf1.lda <- performance(pred1.lda, "tpr", "fpr") 
plot(perf1.lda, col="red")
abline(coef = c(0,1))

newqda <- qda(Y ~ X1 + X2, data = trainingdata)
qdapredic <- predict(newqda, testingdata)
qdaclass1 <- qdapredic$class
table(qdaclass1, testingdata$Y)
mean(qdaclass1 == testingdata$Y)

qda1.probs <- predict(newqda, type = "response")
pred1.qda <- prediction(qda1.probs$posterior[,2], trainingdata$Y)

perf1.qda <- performance(pred1.qda, "tpr", "fpr") 
plot(perf1.qda, col="red")
abline(coef = c(0,1))
