#1a
library(ISLR)
library(tree)
set.seed(123)
tsample <- sample(1:nrow(OJ), 800)
otrain <- OJ[tsample,]
otest <- OJ[-tsample,]
#1b
otree <- tree(Purchase ~ ., otrain)
summary(otree)
#Variables used in the tree model are "LoyalCH" and "PriceDiff".
#The mean deviance is 0.7625.
#The training error rate is 16.5%.
#The tree has 9 terminal nodes. 
#1c
otree
#PriceDiff < 0.085 is a terminal node. There are 76 observations.
#The deviance is 103.5. The prediction is "MM".
#42.105% of observations take on the value of "CH" and 57.895% of observation take on "MM".
#1d
plot(otree)
text(otree, pretty = 0)
#According to the graph, LoyalCH values lower than 0.469289 take on the value "MM".
#LoyalCH values less than 0.705699 can take on the values "MM" or "CH" depending on the PriceDiff value.
#LoyalCH values greater than 0.705699 take on the value "CH".
#1e
tpred <- predict(otree, otest, type = "class")
table(tpred, otest$Purchase)
terror = mean(tpred != otest$Purchase)
terror
#Test error is 18.52%
#1f
ocv <- cv.tree(otree, FUN = prune.misclass)
ocv
#1g
plot(ocv$size, ocv$dev, type = "b")
#1h
#Tree size 5 corresponds to the lowest cross-validated classification error rate,
#followed by tree size 8.
#1i
oprune = prune.misclass(otree, best=5)
summary(oprune)
#error rate is 16.5%
plot(oprune)
text(oprune, pretty = 0)
#1j
#The unpruned tree training error is 17.13%. The pruned tree training error is 18.75%.
#The training error for the pruned tree is higher, which makes sense because by pruning the
#tree, we reduce the variance of the model and increased the bias. 
#1k
ppred <- predict(oprune, otest, type = "class")
table(ppred, otest$Purchase)
terror1 <- mean(predict(oprune, otest, type='class')!=otest$Purchase)
terror1
#The test error rate for the unpruned tree was 18.89%. The test error rate for the pruned tree
#was 18.52%. The test error rate was higher after pruning. This is because by pruning the tree
#we reduced the variance of the model and increased the biasness, in favor of interpretability. 
#The increase in biasness was greater that the reduction of the variance, which is why the model became less accurate. 


#2a
library(e1071)
osvm <- svm(Purchase ~ ., otrain, kernel = "linear", cost = 0.01)
summary(osvm)
#There are 442 support vectors. There are 2 classes. 
#2b
svmtrainpred <- predict(osvm, otrain)
svmtestpred <- predict(osvm, otest)
svmtrainerror <- mean(svmtrainpred != otrain$Purchase)
svmtesterror <- mean(svmtestpred != otest$Purchase)

svmtrainerror
#The training error rate is 16.5%
svmtesterror
#The testing error rate is 17.78%
#2c
stune <- tune(svm, Purchase ~ ., data = otrain, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 5, 10)))
summary(stune)
bestmod <- stune$best.model
summary(bestmod)
#Optimal cost is 5. The associated error is 0.17
#2d
bsvm <- svm(Purchase ~ ., data = otrain, kernel = "linear", cost = 5)
summary(bsvm)
svmtrainpred1 <- predict(bsvm, otrain)
svmtestpred1 <- predict(bsvm, otest)
svmtrainerror1 <- mean(svmtrainpred1 != otrain$Purchase)
svmtesterror1 <- mean(svmtestpred1 != otest$Purchase)

svmtrainerror1
#With the new value for cost, the training error rate is 16.375%
svmtesterror1
#With the new value for cost, the testing error rate is 16.2963%

#2e
rsvm <- svm(Purchase ~ ., data = otrain, kernel = "radial", cost = 0.01)
summary(rsvm)
#There are 629 support vectors. 
svmtrainpred2 <- predict(rsvm, otrain)
svmtestpred2 <- predict(rsvm, otest)
svmtrainerror2 <- mean(svmtrainpred2 != otrain$Purchase)
svmtesterror2 <- mean(svmtestpred2 != otest$Purchase)

svmtrainerror2
#The training error rate is 39.125%
svmtesterror2
#The testing error rate is 38.51852%
stune1 <- tune(svm, Purchase ~ ., data = otrain, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10)))
summary(stune1)
bestmod1 <- stune1$best.model
summary(bestmod1)
#The optimal cost is 5. The associated error is 0.16.

bsvm1 <- svm(Purchase ~ ., data = otrain, kernel = "radial", cost = 5)
summary(bsvm1)
svmtrainpred3 <- predict(bsvm1, otrain)
svmtestpred3 <- predict(bsvm1, otest)
svmtrainerror3 <- mean(svmtrainpred3 != otrain$Purchase)
svmtesterror3 <- mean(svmtestpred3 != otest$Purchase)

svmtrainerror3
#With the new value for cost, the training error rate is 13.75%
svmtesterror3
#With the new value for cost, the testing error rate is 20%

#2f
psvm <- svm(Purchase ~ ., data = otrain, kernel = "polynomial", cost = 0.01, degree=2)
summary(psvm)
#There are 631 support vectors.
svmtrainpred4 <- predict(psvm, otrain)
svmtestpred4 <- predict(psvm, otest)
svmtrainerror4 <- mean(svmtrainpred4 != otrain$Purchase)
svmtesterror4 <- mean(svmtestpred4 != otest$Purchase)

svmtrainerror4
#The training error rate is 37.25%
svmtesterror4
#The testing error rate is 37.40741%
stune2 <- tune(svm, Purchase ~ ., data = otrain, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10)), degree = 2)
summary(stune2)

bestmod2 <- stune2$best.model
summary(bestmod2)
#The optimal cost is 5. The associated error is 0.1725.
bsvm2 <- svm(Purchase ~ ., data = otrain, kernel = "polynomial", cost = 5)
summary(bsvm2)
svmtrainpred5 <- predict(bsvm2, otrain)
svmtestpred5 <- predict(bsvm2, otest)
svmtrainerror5 <- mean(svmtrainpred5 != otrain$Purchase)
svmtesterror5 <- mean(svmtestpred5 != otest$Purchase)

svmtrainerror5
#With the new value for cost, the training error rate is 13%
svmtesterror5
#With the new value for cost, the testing error rate is 21.48148%
#2g
#The radial model appears to give the best results on the data.


#3a
Hitters = na.omit(Hitters)
Hitters$Salary = log(Hitters$Salary)
#3b
htrain <- Hitters[1:200, ]
htest <- Hitters[-(1:200),]
#3c
library(gbm)
lambdas <- seq(.1, .001, by = -.001)
mat <- matrix(NA, nrow = length(lambdas))
for(i in 1:length(lambdas)){
boost.hitter <- gbm(Salary ~ ., data = htrain, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
htrainpred <- predict(boost.hitter, htrain, n.trees = 1000)
mat[i] <- mean((htrainpred-htrain$Salary)^2)
}
plot(lambdas,mat,type="b")
#3d
mat1 <- matrix(NA, nrow = length(lambdas))
for(i in 1:length(lambdas)){
  boost.hitter1 <- gbm(Salary ~ ., data = htrain, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
  htestpred <- predict(boost.hitter1, htest, n.trees = 1000)
  mat1[i] <- mean((htestpred-htest$Salary)^2)
}
plot(lambdas,mat1,type="b")
#3e
summary(boost.hitter)
#CAtBat is the most important predictor, followed by CRuns, CRBI, CHits, and Cwalks.
#3f
library(randomForest)
hbag <- randomForest(Salary ~ ., data = htrain, mtry = 19, importance = TRUE)
hbag
hbagpred<-predict(hbag,htest)
hbagmse<-mean((hbagpred-htest$Salary)^2)
hbagmse*100
#Test set MSE is 0.7211099%.
