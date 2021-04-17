#1a
library(ISLR)
data <- College
set.seed(123)
training <- sample(1: dim(data)[1], dim(data)[1]/2)
testing <- -training
trainds <- data[training,]
testds <- data[testing,]
nrow(data)
nrow(trainds)
nrow(testds)
names(trainds)
#1b
lmodel <- lm(Apps ~ ., data = trainds)
summary(lmodel)
lmpred <- predict(lmodel, testds)
mean((testds[, "Apps"] - lmpred)^2)
#test error is 1373995
#1c
set.seed(123)
library(glmnet)
x <- model.matrix(Apps ~ ., data = data)
y <- data$Apps
olambda <- cv.glmnet(x, y, alpha = 0)
blambda <- olambda$lambda.min
blambda
ridge = glmnet(x, y, alpha=0, lambda = blambda) 
coef(ridge)
trainmatrix <- model.matrix(Apps ~ ., data = trainds)
testmatrix <- model.matrix(Apps ~ ., data = testds)
rpred = predict(ridge, newx = testmatrix, s = blambda) 
mean((testds[, "Apps"] - rpred)^2)
#test error is 1743565
#1d
set.seed(123)
olambda1 <- cv.glmnet(x, y, alpha = 1)
blambda1 <- olambda1$lambda.min
blambda1
lasso = glmnet(x, y, alpha=1, lambda = blambda1) 
coef(lasso)
lpred = predict(lasso, newx = testmatrix, s = blambda1) 
mean((testds[, "Apps"] - lpred)^2)
#test error is 1186643
#1e
library(pls)
set.seed(2)
pcrfit <- pcr(Apps ~ ., data = trainds, scale = TRUE, validation = "CV")
summary(pcrfit)
pcrpred <- predict(pcrfit, testds, ncomp=6) 
mean((testds[, "Apps"] - pcrpred)^2)
#test error is 3005430
#1f
set.seed(3)
plsfit = plsr(Apps ~ ., data = trainds, scale = TRUE, validation="CV")
summary(plsfit)
plspred <- predict(plsfit, testds, ncomp=2) 
mean((testds[, "Apps"] - plspred)^2)
#test error is 3008049
#1g
#Out of all the models that we created, the lasso model would be the most accurate.
#It had the lowest test error, 1186643. The differences in test errorsbetween the 5 models 
#varied greatly, with values ranging between 11886643 to 3008049.  

#2a
library(MASS)
data1 <- Boston
names(data1)
library(caret)
library(leaps)
#Best subset
models <- regsubsets(crim ~ ., data = data1, nvmax = 13)
omodel <- summary(models)
omodel
data.frame(
  Adj.R2 = which.max(omodel$adjr2),
  CP = which.min(omodel$cp),
  BIC = which.min(omodel$bic)
)
#Adjusted R2 is telling us that the model with 9 predictors is the best model, whereas
#CP is telling us that the model with 8 predictors is the best model.BIC tells us that
#the model with 3 predictors is the best model. 

#training and testing set
set.seed(5)
btrain=sample(1:nrow(data1), nrow(data1)/2)
bostontrain = data1[btrain,]
btest = (-btrain)
bostontest = data1[btest,]
nrow(data1)
nrow(bostontrain)
nrow(bostontest)
#ridge
bmodel <- model.matrix(crim ~ ., data = data1)
bcrim <- data1$crim
boslambda <- cv.glmnet(bmodel, bcrim, alpha = 0)
blambdamin <- boslambda$lambda.min
blambdamin
bridge = glmnet(bmodel, bcrim, alpha=0, lambda = blambdamin) 
coef(bridge)

btrainmatrix <- model.matrix(crim ~ ., data = bostontrain)
btestmatrix <- model.matrix(crim ~ ., data = bostontest)

bpred = predict(bridge, newx = btestmatrix, s = blambdamin) 
mean((bostontest[, "crim"] - bpred)^2)
#lasso
set.seed(23)
boslambda1 <- cv.glmnet(bmodel, bcrim, alpha = 1)
blambdamin1 <- boslambda1$lambda.min
blambdamin1
blasso = glmnet(bmodel, bcrim, alpha=1, lambda = blambdamin1) 
coef(blasso)
blpred = predict(blasso, newx = btestmatrix, s = blambdamin1) 
mean((bostontest[, "crim"] - blpred)^2)
#pcr
set.seed(2)
bpcr <- pcr(crim ~ ., data = bostontrain, scale = TRUE, validation = "CV")
summary(bpcr)
bpcrpred <- predict(bpcr, bostontest, ncomp=13) 
mean((bostontest[, "crim"] - bpcrpred)^2)

#2b
dim(data1)
blassopred = predict(blasso, type = 'coefficient', s = blambdamin1)[1:14,]
blassopred
#The lasso model had the lowest mean squared error, therefore it performed the 
#best among models on this data set

#2c
#the lasso model included all of the features in the dataset except for age. Every
#other variable is larger than 0, so they are included in the model. 

