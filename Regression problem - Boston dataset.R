library(MASS)
library(glmnet)
data(Boston)
set.seed(45678912)
str(Boston)
sapply(Boston, class)



#linear model
Boston <- as.data.frame(Boston)
index <- sample(nrow(Boston), nrow(Boston)*0.75)
boston.train <- Boston[index,]
boston.test <- Boston[-index,]

linear.boston <- lm(medv~crim+zn+indus+nox+rm+age+dis+rad+tax+ptratio+black, data=boston.train)
summary(linear.boston)
MSE.original.linear.train <- (summary(linear.boston)$sigma)^2
adjR.original.linear <- summary(linear.boston)$adj.r.squared
AIC(linear.boston)
BIC(linear.boston)

adjR.original.linear
pi <- predict(linear.boston, newdata=boston.test)
MSPE.linear.test.original <- mean((pi-boston.test$medv)^2)
MSPE.linear.test.original #test
MSE.original.linear.train #train

par(mfrow = c(2,2))
plot(linear.boston)
#Best subset regression
#install.packages('leaps')
library(leaps)
subset <- regsubsets(medv~., data=boston.train)
summary(subset)
plot(subset, scale = "bic")

model.best.subset <- lm(medv~chas+nox+rm+dis+ptratio+black+lstat, data=boston.train)
summary(model.best.subset)
MSE.bestsubset.linear.train <- (summary(model.best.subset)$sigma)^2
adjR.bestsubset.linear <- summary(model.best.subset)$adj.r.squared
AIC(model.best.subset)
BIC(model.best.subset)
adjR.bestsubset.linear

pi <- predict(model.best.subset, newdata=boston.test)
MSPE.linear.test.bestsub <- mean((pi-boston.test$medv)^2)
par(mfrow = c(2,2))
plot(model.best.subset)
MSE.bestsubset.linear.train #train
MSPE.linear.test.bestsub  #test

#stepwise variable selection

nullmodel=lm(medv~1, data=boston.train)
fullmodel=lm(medv~., data=boston.train)
?step
model.step <- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "both")
summary(model.step)
MSE.model.step.train <- (summary(model.step)$sigma)^2
adjR.model.step.linear <- summary(model.step)$adj.r.squared
AIC(model.step)
BIC(model.step)
adjR.model.step.linear
par(mfrow=c(2,2))
plot(model.step)
#final model = model.step
#Out-of-sample prediction - test error 
pi <- predict(model.step, newdata=boston.test)
MSE.linear.test.step <- mean((pi-boston.test$medv)^2)
MSE.linear.test.step #test
MSE.model.step.train #train




# TREE --------------------------------------------------------------------
install.packages('rpart')
install.packages('rpart.plot') 
library(rpart)
library(rpart.plot)
library(MASS)
data(Boston)
set.seed(45678912)
index <- sample(nrow(Boston), nrow(Boston)*0.75)
boston.train <- Boston[index,]
boston.test <- Boston[-index,]


original.tree <- rpart(formula = medv ~ ., data = boston.train)
par(mfrow=c(1,2))
prp(original.tree, extra = 1, main = "Original")
insample.prediction.original.tree <- predict(boston.rpart, boston.train)

outsample.prediction.original.tree <- predict(boston.rpart, boston.test)

summary(original.tree)
MSE.original.tree <- mean((boston.train$medv - insample.prediction.original.tree)^2)
MSPE.original.tree <- mean((outsample.prediction.original.tree - boston.test$medv)^2)

plotcp(original.tree)
prune.original.tree <- prune(original.tree, cp = 0.03)
prp(prune.original.tree, extra = 1, main = "Pruned")

MSPE.original.tree   #test
MSE.original.tree   #train


insample.prediction.prune.tree <- predict(prune.original.tree, boston.train)
outsample.prediction.prune.tree <- predict(prune.original.tree, boston.test)

MSE.prune.tree <- mean((boston.train$medv - insample.prediction.prune.tree)^2)
MSPE.prune.tree <- mean((outsample.prediction.prune.tree - boston.test$medv)^2)
# Bagging -----------------------------------------------------------------


install.packages("ipred")
library(ipred)
boston.bag.tree<- bagging(medv~., data = boston.train, nbagg=100)
boston.bag.tree

#calculation number of bags
nbag <- seq(1,200,3)
MSPE.nbag <- rep(0,length(nbag))
for(i in 1:length(nbag)){
  boston.nbag <- bagging(medv~., data=boston.train, nbagg = nbag[i])
  boston.nbag.pred <- predict(boston.nbag, newdata=boston.test)
  MSPE.nbag[i] <- mean((boston.nbag.pred-boston.test$medv)^2)
}
plot(nbag, MSPE.nbag, type = 'l', col='blue', lwd=2, main = "MSPE vs number of trees")


#Prediction on testing sample.
bag.pred.train <- predict(boston.bag.tree, newdata = boston.train)
boston.bag.pred.test<- predict(boston.bag.tree, newdata = boston.test)
MSPE.bag <- mean((boston.test$medv -boston.bag.pred.test)^2)
MSE.bag <- mean((boston.train$medv -bag.pred.train)^2)

MSE.bag     #train
MSPE.bag     #test

#out of bag sample
boston.bag.oob<- bagging(medv~., data = boston.train, coob=T, nbagg=110)
boston.bag.oob
#root mean squared error:  4.0735, MSE = 16 


# Random forest -----------------------------------------------------------
install.packages("randomForest")
library(randomForest)

rand.forest.tree <- randomForest(medv~., data = boston.train, importance = TRUE)
rand.forest.tree$importance
plot(rand.forest.tree$mse, type='l', col='blue', lwd=2, main = "MSE vs number of trees")
rand.forest.tree.pred <- predict(rand.forest.tree, newdata = boston.test)
MSPE.rand.error <- mean((boston.test$medv - rand.forest.tree.pred)^2)
rand.train.forest.tree.pred <- predict(rand.forest.tree, newdata = boston.train)
MSPE.rand.error <- mean((boston.test$medv - rand.forest.tree.pred)^2)
MSPE.rand.error
MSE.forest.tree.pred <- mean((boston.train$medv - rand.train.forest.tree.pred)^2)
MSE.forest.tree.pred  #train
MSPE.rand.error #test
# Boosting ----------------------------------------------------------------
install.packages("gbm")
library(gbm)
boston.boosting.tree <- gbm(medv~., data = boston.train, distribution = "gaussian", n.trees = 1000)
summary(boston.boosting.tree)

par(mfrow = c(2,2))
plot(boston.boosting.tree, i="lstat", main ="Most influential predictor", lwd=2)
plot(boston.boosting.tree, i="zn", main ="Least influential predictor", lwd=2)

boston.boosting.tree.pred <- predict(boston.boosting.tree, newdata = boston.test, n.trees = 1000)
MSPE.boston.boosting.tree <- mean((boston.test$medv - boston.boosting.tree.pred)^2)

boston.train.boosting.tree.pred <- predict(boston.boosting.tree, newdata = boston.train, n.trees = 1000)
MSE.train.boston.boosting.tree <- mean((boston.train$medv - boston.train.boosting.tree.pred)^2)

ntree.boost <- seq(100,1000,100)
error <- rep(0,length(ntree.boost))
for (i in 1:length(ntree.boost)){
  pred.ntree.boost <- predict(boston.boosting.tree, newdata = boston.test, n.trees = ntree.boost[i])
  error[i] <- mean((boston.test$medv - pred.ntree.boost)^2)
}
plot(ntree.boost,error,type='l',lwd = 2, col='green')

MSE.train.boston.boosting.tree  #train
MSPE.boston.boosting.tree   #test

# GAM ---------------------------------------------------------------------
data(Boston)
set.seed(45678912)
index <- sample(nrow(Boston), nrow(Boston)*0.75)
boston.train <- Boston[index,]
boston.test <- Boston[-index,]

install.packages(mgcv)
library(mgcv)
summary(boston.train)
gam_formula <- as.formula(paste("medv~s(crim)+s(zn)+s(indus)+s(nox)+s(rm)+s(age)+s(dis)+s(tax)+s(ptratio)+s(black)+s(lstat)+chas+rad"))
#s(X) - spline basis flexible function
#degree of freedom close to 1 - linear term
boston.gam <- gam(gam_formula, data=boston.train, knots=NULL)
summary(boston.gam)
plot(boston.gam, shade=TRUE,seWithMean=TRUE,scale=0, pages = 1)
#zn , age, ptratio linear because estimated degree of freedom is 1
#make table
gam_formula_revised <- as.formula(paste("medv~s(crim)+s(indus)+s(nox)+s(rm)+s(dis)+s(tax)+s(black)+s(lstat)+chas+rad+age+zn+ptratio"))
boston.gam_revised <- gam(gam_formula_revised, data=boston.train, knots=NULL)
summary(boston.gam_revised)
plot(boston.gam_revised, shade=TRUE,seWithMean=TRUE,scale=0, pages = 1)

AIC(boston.gam_revised)
BIC(boston.gam_revised)
boston.gam_revised$deviance

AIC(boston.gam)
BIC(boston.gam)
boston.gam$deviance


#not a lot of difference in AIC, BIC and deviance
#in-sample fit performance
pred.gam.in<-predict(boston.gam_revised,boston.train,type="response")
insample.pred.error.gam <- mean((pred.gam.in-boston.train$medv)^2)
insample.pred.error.gam
#out-sample fit performance
pred.gam.in<-predict(boston.gam_revised,boston.test,type="response")
outsample.pred.error.gam <- mean((pred.gam.in-boston.test$medv)^2)
outsample.pred.error.gam


pred.gam.in<-predict(boston.gam,boston.train,type="response")
insample.pred.error.gam <- mean((pred.gam.in-boston.train$medv)^2)
insample.pred.error.gam
#out-sample fit performance
pred.gam.in<-predict(boston.gam,boston.test,type="response")
outsample.pred.error.gam <- mean((pred.gam.in-boston.test$medv)^2)
outsample.pred.error.gam



# Neural Network ----------------------------------------------------------
library(MASS)
maxs <- apply(Boston, 2, max) 
mins <- apply(Boston, 2, min)
scaled <- as.data.frame(scale(Boston, center = mins, scale = maxs - mins))

index <- sample(1:nrow(Boston),round(0.75*nrow(Boston)))
train_ <- scaled[index,]
test_ <- scaled[-index,]
install.packages("neuralnet")
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
plot(nn)
#out of sample MSE
pr.nn <- compute(nn,test_)

pr.nn_ <- pr.nn$net.result*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)
test.r <- (test_$medv)*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)
MSE.nn.test <- sum((test.r - pr.nn_)^2)/nrow(test_)
MSE.nn.test

#in sample MSE
pr.nn <- compute(nn,train_)

pr.nn_ <- pr.nn$net.result*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)
train.r <- (train_$medv)*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)
MSE.nn.train <- sum((train.r - pr.nn_)^2)/nrow(train_)
MSE.nn.train
