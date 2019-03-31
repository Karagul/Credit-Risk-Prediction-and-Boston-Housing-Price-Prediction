library(psych)
library(knitr)
library(dplyr)

library(ggplot2)
set.seed(12825368)
german_credit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
#http://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)
install.packages('ggplot')
library(ggplot)

colnames(german_credit)=c("chk_acct","duration","credit_his","purpose","amount","saving_acct","present_emp","installment_rate","sex","other_debtor","present_resid","property","age","other_install","housing","n_credits","job","n_people","telephone","foreign","response")
if(min(german_credit$response>0)){
german_credit$response = german_credit$response-1}
#orginal response coding 1= good, 2 = bad
#we need 0 = good, 1 = bad

# Exploratory Data Analysis -----------------------------------------------

dim(german_credit)
names(german_credit)
head(german_credit)
summary(german_credit)
sapply(german_credit,class)
attach(german_credit)

par(mfrow=c(3,1))
hist(duration, col='cyan')
hist(amount, col='cyan')
hist(installment_rate, col='cyan')
par(mfrow=c(2,2))
hist(age, col='cyan')
hist(n_credits, col='cyan')
hist(n_people, col='cyan')


# generalised Linear models -----------------------------------------------------------

index = sample(nrow(german_credit), nrow(german_credit)*0.75)
train = german_credit[index,]
test = german_credit[-index,]
dim(test)
dim(train)
    train = as.data.frame(train)
    test = as.data.frame(test)

#full model
fullmodel = glm(train$response~., data = train)
summary(fullmodel)
par(mfrow=c(2,2))

plot(fullmodel)

#prediction
pred.glm0.fullmodel.train<- predict(fullmodel, data = train, type="response")
MSE.train.fullmodel <- mean((pred.glm0.fullmodel.train-train$response)^2)

pred.glm0.fullmodel.test <- predict(fullmodel, data = test, type="response")
MSPE.test.fullmodel <- mean((pred.glm0.fullmodel.test-test$response)^2)
MSE.train.fullmodel  #train
MSPE.test.fullmodel  #test
AIC(fullmodel)
#R^2 30.0%, adj R^2 25.5%, MSE: 0.14
attach(train)

#stepwise regression - AIC
nullmodel=lm(train$response~1, data=train)
model_step_s <- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='both')
summary(model_step_s)
mse_model_step_s = mean(model_step_s$residuals^2)
plot(model_step_s)
AIC(model_step_s)

#prediction
pred.glm0.step.train<- predict(model_step_s, data = train, type="response")
MSE.train.step <- mean((pred.glm0.step.train-train$response)^2)

pred.glm0.step.test <- predict(model_step_s, data = test, type="response")
MSPE.test.step <- mean((pred.glm0.step.test-test$response)^2)
MSE.train.step  #train
MSPE.test.step  #test

#BIC - best subset
#Which subset of variables should you include in order to minimize BIC?


#install.packages('leaps')
library(leaps)
best_sub <- regsubsets(response~., train)
summary(best_sub)
plot(best_sub, scale="bic")
colnames(train)
#If there are n independent variables, the number of possible nonempty subsets is 2^n - 1


#LASSO
library(glmnet)
#glmnet does not take data frame as input, so we have to specify the x matrix and y vector.
is.na(train)
class(train$duration)

class(colnames(train))
for(i in colnames(train)){
  if((class(train[[i]])) == 'integer')
  {
    train[[i]] <- as.numeric(train[[i]])
  }
}
str(train)

dummy <- model.matrix(~., data=train)
head(dummy)
data.lasso <- data.frame(dummy[,-1])
head(data.lasso)
dummy.test <- model.matrix(~., data=test)
head(dummy.test)
test.data.lasso <- data.frame(dummy.test[,-1])
head(test.data.lasso)

#Perform cross-validation to determine the shrinkage parameter.
lasso <- glmnet(x = as.matrix(data.lasso[,-c(which(colnames(train)=='response'))]), data.lasso$response, alpha=1, family='binomial')
lasso.cv <- cv.glmnet(x = as.matrix(data.lasso[,-c(which(colnames(train)=='response'))]), data.lasso$response,family='binomial', type.measure = "class")
lasso.cv
plot(lasso.cv)
#For logistc regression, we can specify type.measure="class" so that the CV error will be misclassification error.

#Get the coefficient with optimal ??

coef(lasso, s=lasso.cv$lambda.min)
# in-sample prediction
pred.lasso.train <- predict(lasso, newx = as.matrix(data.lasso[,-c(which(colnames(train)=='response'))]), lasso.cv$lambda.1se, type = "response")
# out-of-sample prediction
pred.lasso.test<- predict(lasso, newx=as.matrix(test.data.lasso[,-c(which(colnames(train)=='response'))]), s=lasso.cv$lambda.1se, type = "response")
#You have to specify type="response" in order to get probability outcome
#Otherwise, what it produces is the linear predictor term ??0+??1X1+??2X2+.. 

MSE.lasso.train <- mean((pred.lasso.train-train$response)^2)
MSE.lasso.test <- mean((pred.lasso.test-test$response)^2)
MSE.lasso.train   #train
MSE.lasso.test    #test

# ROC curve ---------------------------------------------------------------
install.packages('ROCR')
library(ROCR)
credit.glm0.logit<- glm(response~., family=binomial, data=train)
summary(credit.glm0.logit)
credit.glm0.probit<- glm(response~., family=binomial(probit), data=train)
summary(credit.glm0.probit)
credit.glm0.CLL<- glm(response~., family=binomial(cloglog), data=train)
summary(credit.glm0.CLL)

#almost same results, usually coef(logit) = 1.6(probit) but comparing coef is not a good measure

pred.glm0.train.logit<- predict(credit.glm0.logit, type="response")

pred.logit <- prediction(pred.glm0.train.logit, train$response)
perf.logit <- performance(pred.logit, "tpr", "fpr")
plot(perf.logit, colorize=TRUE, main = 'ROC curve')

pred.glm0.train.probit<- predict(credit.glm0.probit, type="response")

pred.probit <- prediction(pred.glm0.train.probit, train$response)
perf.probit <- performance(pred.probit, "tpr", "fpr")
plot(perf.probit, colorize=TRUE, main = 'Probit ROC curve')

pred.glm0.train.CLL<- predict(credit.glm0.CLL, type="response")

pred.CLL <- prediction(pred.glm0.train.CLL, train$response)
perf.CLL <- performance(pred.CLL, "tpr", "fpr")
plot(perf.CLL, colorize=TRUE, main = 'Complimentary loglog ROC curve')    #100%


# AUC ---------------------------------------------------------------------


AUC_logit<- unlist(slot(performance(pred.logit, "auc"), "y.values"))

AUC_probit <- unlist(slot(performance(pred.probit, "auc"), "y.values"))
AUC_CLL <- unlist(slot(performance(pred.CLL, "auc"), "y.values"))       
AUC_logit
AUC_probit
AUC_CLL
#almost same AUC



# Cross Validation --------------------------------------------------------

library(boot)

#Choosing a large cut-off probability will result in few cases being predicted as 1, and chossing a small cut-off probability will result in many cases being predicted as 1.
costfunc = function(obs, pred.p, pcut){
  weight1 = 6   # define the weight for "true=1 but pred=0" (FN)
  weight0 = 1    # define the weight for "true=0 but pred=1" (FP)
  c1 = (obs==1)&(pred.p<pcut)    # count for "true=1 but pred=0"   (FN)
  c0 = (obs==0)&(pred.p>=pcut)   # count for "true=0 but pred=1"   (FP)
  cost = mean(weight1*c1 + weight0*c0)  # misclassification with weight
  return(cost) # you have to return to a value when you write R functions
}
pred.glm0.train<- predict(fullmodel, data = train, type="response")
pred.glm0.test<- predict(fullmodel, data = test,  type="response")


p.seq = seq(0.01, 1, 0.01) 
cost = rep(0, length(p.seq))  

for(i in 1:length(p.seq)){ 
  print(cost[i])
  cost[i] = costfunc(obs = train$response, pred.p = pred.glm0.train, pcut = p.seq[i])  
} 
plot(p.seq, cost)

optimal.pcut.glm0 = p.seq[which(cost==min(cost))]   #0.27
class.glm0.train.opt<- (pred.glm0.train>optimal.pcut.glm0)*1

class.glm0.test.opt<- (pred.glm0.test>0.27)*1

# step 2. get confusion matrix, MR, FPR, FNR
table(train$response, class.glm0.train.opt, dnn = c("True", "Predicted"))
MR.train<- mean(train$response!=class.glm0.train.opt)
MR.train

  table(test$response, class.glm0.test.opt, dnn = c("True", "Predicted"))
MR.test<- mean(test$response!=class.glm0.test.opt)
MR.test


# Classification tree ---------------------------------------------------------
library(rpart)
sapply(german_credit,class)
library(rpart.plot)
german_credit$response <- as.factor(german_credit$response)
index.tree <- sample(nrow(german_credit), 0.75*nrow(german_credit))
tree.train <- german_credit[index.tree,]
tree.test <- german_credit[-index.tree,]
credit.rpart <- rpart(formula = tree.train$response ~ . , data = tree.train, method = "class", parms = list(loss=matrix(c(0,5,1,0), nrow = 2)))
#The method = "class" is required if the response is not declared as factors.
prp(credit.rpart, extra = 1, main = "Classification tree")
#in sample prediction
pred.tree.train<- predict(credit.rpart, type="class")
table(tree.train$response, pred.tree.train, dnn = c("True", "Pred"))
mean(ifelse(tree.train$response != pred.tree.train, 1, 0))

#out of sample prediction
pred.tree.test<- predict(credit.rpart, newdata = tree.test, type="class")
table(tree.test$response, pred.tree.test, dnn = c("True", "Pred"))
mean(ifelse(tree.test$response != pred.tree.test, 1, 0))

#GAM

credit.data = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
#http://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)


colnames(credit.data)=c("chk_acct","duration","credit_his","purpose","amount","saving_acct","present_emp","installment_rate","sex","other_debtor","present_resid","property","age","other_install","housing","n_credits","job","n_people","telephone","foreign","response")
if(min(credit.data$response)>0){
credit.data$response = credit.data$response-1}
#orginal response coding 1= good, 2 = bad
#we need 0 = good, 1 = bad

id_train <- sample(nrow(credit.data),nrow(credit.data)*0.75)
credit.train = credit.data[id_train,]
credit.test = credit.data[-id_train,]

creditcost <- function(observed, predicted){
  weight1 = 6
  weight0 = 1
  c1 = (observed==1)&(predicted == 0) #logical vector - true if actual 1 but predict 0
  c0 = (observed==0)&(predicted == 1) #logical vector - true if actual 0 but predict 1
  return(mean(weight1*c1+weight0*c0))
}


library(mgcv)
colnames(credit.data)
sapply(credit.data, class)

credit.gam <- gam(as.factor(response)~s(duration)+s(amount)+(installment_rate)+(present_resid)+(age)+(n_credits)+(n_people)+chk_acct+credit_his+purpose+saving_acct+present_emp+sex+other_debtor+property+other_install+housing+job+telephone+foreign, family=binomial,data=credit.data)
summary(credit.gam)
par(mfrow=c(1,2))
plot(credit.gam, shade=TRUE,seWithMean=TRUE,scale=0, pages = 1)

prob.gam.in<-predict(credit.gam,credit.train,type="response")
pred.gam.in<-(prob.gam.in>=0.27)*1
table(credit.train$response,pred.gam.in,dnn=c("Observed","Predicted"))
MSE_gam_in <- mean((pred.gam.in-credit.train$response)^2)
MSE_gam_in

prob.gam.out<-predict(credit.gam,credit.test,type="response")
pred.gam.out<-(prob.gam.out>=0.27)*1
table(credit.test$response,pred.gam.out,dnn=c("Observed","Predicted"))
MSE_gam_out <- mean((pred.gam.out-credit.test$response)^2)
MSE_gam_out


##neural network
install.packages("nnet")
library(nnet)
credit.nnet <- nnet(as.factor(response)~., data=credit.train,size=13, maxit=500)
prob.nnet.test= predict(credit.nnet,credit.test)
prob.nnet.train= predict(credit.nnet,credit.train)
pred.nnet.test = as.numeric(prob.nnet.test > 0.27)
table(credit.test$response,pred.nnet.test, dnn=c("Observed","Predicted"))
mean(ifelse(credit.test$response != pred.nnet.test, 1, 0))

pred.nnet.train = as.numeric(prob.nnet.train > 0.27)
table(credit.train$response,pred.nnet.train, dnn=c("Observed","Predicted"))
mean(ifelse(credit.train$response != pred.nnet.train, 1, 0))

