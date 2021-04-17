#1a
library(ISLR)
summary(Weekly)
plot(Weekly)
dim(Weekly)
#1b
weeklylag <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Weekly, family = binomial)
summary(weeklylag)
#1c
confu <- predict(weeklylag, type = "response")
confu1 <- ifelse(confu > 0.5, "Up", "Down")
table(confu1, Weekly$Direction)
#1d
training <- Weekly[Weekly$Year <= 2008,]
dim(training)
testing <- Weekly[Weekly$Year > 2008,]
dim(testing)
regres1 <- glm(Direction ~ Lag2, data = training, family = binomial)
summary(regres1)
confu2 <- predict(regres1, newdata = testing, type = "response")
confu3 <- ifelse(confu2 > 0.5, "Up", "Down")
table(confu3, testing$Direction)
mean(confu3 == testing$Direction)
#1e
library(MASS)
weeklylda <- lda(Direction ~ Lag2, data = training, family = binomial)
ldapred <- predict(weeklylda, testing)
ldaclass <- ldapred$class
table(ldaclass, testing$Direction)
mean(ldaclass == testing$Direction)
#1f
weeklyqda <- qda(Direction ~ Lag2, data = training, family = binomial)
qdapred <- predict(weeklyqda, testing)
qdaclass <- qdapred$class
table(qdaclass, testing$Direction)
mean(qdaclass == testing$Direction)
#1g
library(class)
set.seed(1)
knnmodel <- knn(as.matrix(training$Lag2), as.matrix(testing$Lag2), training$Direction, k=1)
table(knnmodel, testing$Direction)
mean(knnmodel == testing$Direction)
#1h
#Answer in PDF
#1i
regres2 <- glm(Direction ~ Lag1+Lag2+Lag3:Lag4+I(Lag5*Volume), data = training, family = binomial)
summary(regres2)
regmodel<- predict(regres2, newdata = testing, type = "response")
regmodel1 <- ifelse(regmodel > 0.5, "Up", "Down")
table(regmodel1, testing$Direction)
mean(regmodel1 == testing$Direction)

weeklylda1 <- lda(Direction ~ Lag3 + Lag4, data = training, family = binomial)
ldapred1 <- predict(weeklylda1, testing)
ldaclass1 <- ldapred1$class
table(ldaclass1, testing$Direction)
mean(ldaclass1 == testing$Direction)

weeklyqda1 <- qda(Direction ~ Lag3 + Lag4, data = training, family = binomial)
qdapred1 <- predict(weeklyqda1, testing)
qdaclass1 <- qdapred1$class
table(qdaclass1, testing$Direction)
mean(qdaclass1 == testing$Direction)

set.seed(1)
knnmodel1 <- knn(as.matrix(training$Lag2), as.matrix(testing$Lag2), training$Direction, k=2)
table(knnmodel1, testing$Direction)
mean(knnmodel1 == testing$Direction)

set.seed(1)
knnmodel2 <- knn(as.matrix(training$Lag2), as.matrix(testing$Lag2), training$Direction, k=5)
table(knnmodel2, testing$Direction)
mean(knnmodel2 == testing$Direction)


#2a
twolags <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
summary(twolags)
#2b
twolags1 <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-1,], family = binomial)
summary(twolags1)
#2c
predict.glm(twolags1, Weekly[1,], type = "response") > 0.5
Weekly[1,]$Direction
#2d
error <- rep(0, dim(Weekly)[1])
for(i in 1:dim(Weekly)[1])
{forloop <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-i,], family = binomial)
prediction <- predict.glm(forloop, Weekly[i,], type = "response") > 0.5
correctpred <- Weekly[i,]$Direction == "Up"
if (prediction != correctpred)
  error[i] <- 1
}
error
#2e
mean(error)