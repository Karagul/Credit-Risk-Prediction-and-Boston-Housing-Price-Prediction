library(psych)
library(knitr)
library(dplyr)
library(ggplot2)
set.seed(12824704)
german_credit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
#http://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)
#install.packages('ggplot')
library(ggplot)
head(german_credit)
ncol(german_credit)
str(german_credit)
colnames(german_credit)=c("chk_acct","duration","credit_his","purpose","amount","saving_acct","present_emp","installment_rate","sex","other_debtor","present_resid","property","age","other_install","housing","n_credits","job","n_people","telephone","foreign","response")
str(german_credit)
colSums(is.na(german_credit))
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
library(ggplot2)

h1 <- ggplot(german_credit, aes(duration))+geom_histogram()
h2 <- ggplot(german_credit, aes(amount))+geom_histogram()
h3 <- ggplot(german_credit, aes(age))+geom_histogram()
h4 <- ggplot(german_credit, aes(installment_rate))+geom_histogram()
h5 <- ggplot(german_credit, aes(n_credits))+geom_histogram()
h6 <- ggplot(german_credit, aes(n_people))+geom_histogram()
h7 <- ggplot(german_credit, aes(response)+geom_histogram(binwidth = 1))
ggplot(german_credit, aes(response))+geom_histogram(binwidth = 0.2)
grid.arrange(h1,h2,h3,h4,h5,h6, ncol=3)
# counts
require(gridExtra)

p1 <- ggplot(german_credit, aes(x=chk_acct)) +
  geom_bar()
p2 <- ggplot(german_credit, aes(x=credit_his)) +
  geom_bar()
p3 <- ggplot(german_credit, aes(x=purpose)) +
  geom_bar()
p4 <- ggplot(german_credit, aes(x=saving_acct)) +
  geom_bar()
p5 <- ggplot(german_credit, aes(x=present_emp)) +
  geom_bar()
p6 <- ggplot(german_credit, aes(x=sex)) +
  geom_bar()
p7 <- ggplot(german_credit, aes(x=other_debtor)) +
  geom_bar()
p8 <- ggplot(german_credit, aes(x=property)) +
  geom_bar()
p9 <- ggplot(german_credit, aes(x=other_install)) +
  geom_bar()



p10 <- ggplot(german_credit, aes(x=housing)) +
  geom_bar()
p11 <- ggplot(german_credit, aes(x=job)) +
  geom_bar()
p12 <- ggplot(german_credit, aes(x=telephone)) +
  geom_bar()
p13 <- ggplot(german_credit, aes(x=foreign)) +
  geom_bar()
grid.arrange(p1, p2, p3, p4, p5, p6,p7, p8, p9, ncol=3)
grid.arrange(p10,p11,p12,p13, ncol=2)

#multicollinearity
library(car)
str(german_credit)
lm <- lm(as.numeric(response)~duration+amount+installment_rate+present_resid+age+n_credits+n_people, data=german_credit)
1/(1-0.06289)
vif(lm)
#outliers
outlier_values_duration <- boxplot.stats(german_credit$duration)$out  # outlier values.
outlier_values_amount <- boxplot.stats(german_credit$amount)$out  # outlier values.

par(mfrow=c(2,3))
boxplot(german_credit$duration, main = "duration")
boxplot(german_credit$amount, main = "amount")
boxplot(german_credit$installment_rate, main = "installment rate")
boxplot(german_credit$present_resid, main = "present resident since")
boxplot(german_credit$age, main="age")
boxplot(german_credit$n_credits, main = "number of credits")

mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)


# generalised Linear models -----------------------------------------------------------
set.seed(12825704)
index = sample(nrow(german_credit), nrow(german_credit)*0.75)
train = german_credit[index,]
test = german_credit[-index,]
dim(test)
dim(train)
    train = as.data.frame(train)
    test = as.data.frame(test)

#full model
fullmodel = glm(train$response~., data = train, family=binomial)
summary(fullmodel)

costfunc = function(obs, pred.p, pcut){
  weight1 = 1   # define the weight for "true=1 but pred=0" (FN)
  weight0 = 5    # define the weight for "true=0 but pred=1" (FP)
  c1 = (obs==1)&(pred.p<pcut)    # count for "true=1 but pred=0"   (FN)
  c0 = (obs==0)&(pred.p>=pcut)   # count for "true=0 but pred=1"   (FP)
  cost = mean(weight0*c1 + weight1*c0)  # misclassification with weight
  return(cost) # you have to return to a value when you write R functions
} 
p.seq = seq(0.01, 0.5, 0.01)
cost = rep(0, length(p.seq))  
for(i in 1:length(p.seq)){ 
  cost[i] = costfunc(obs = train$response, pred.p = pred.glm0.fullmodel.train, pcut = p.seq[i])  
}
plot(p.seq, cost)
optimal.pcut= p.seq[which(cost==min(cost))]
#prediction
pred.glm0.fullmodel.train<- predict(fullmodel, data = train, type="response")
class.glm0.train<- (pred.glm0.fullmodel.train>0.5)*1
mean(train$response!=class.glm0.train)
table(train$response, class.glm0.train, dnn = c("True", "Pred"))

pred.glm0.fullmodel.test<- predict(fullmodel, newdata = test, type="response")
class.glm0.test<- (pred.glm0.fullmodel.test>0.5)*1
mean(test$response!=class.glm0.test)
table(test$response, class.glm0.test, dnn = c("True", "Pred"))


AIC(fullmodel)
attach(train)

#stepwise regression - AIC
nullmodel=glm(train$response~1, data=train, family=binomial)
model_step_s <- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='both', family=binomial)
summary(model_step_s)
mse_model_step_s = mean(model_step_s$residuals^2)
AIC(model_step_s)


pred.step.test<- predict(model_step_s, newdata = test, type="response")

library(ROCR)


pred.test.step = prediction(pred.step.test, test$response)
perf.test.step = performance(pred.test.step, "tpr", "fpr")
plot(perf.test.step, colorize=TRUE, main = "Logistic Regression")
unlist(slot(performance(pred.test.step, "auc"), "y.values"))


#prediction

pred.glm0.step.test<- predict(model_step_s, newdata = test, type="response")
class.glm0.test.step<- (pred.glm0.step.test>0.5)*1
mean(test$response!=class.glm0.test.step)
table(test$response, class.glm0.test.step, dnn = c("True", "Pred"))


pred.glm0.step.train<- predict(model_step_s, newdata = train, type="response")
class.glm0.train.step<- (pred.glm0.step.train>0.5)*1
mean(train$response!=class.glm0.train.step)

#BIC - best subset
#Which subset of variables should you include in order to minimize BIC?


#install.packages('leaps')
par(mfrow=c(1,1))
library(leaps)
best_sub <- regsubsets(response~., train)
summary(best_sub)
plot(best_sub, scale="bic")


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
data.lasso <- data.frame(dummy)
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
set.seed(12825708)
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

pred.tree.test.prob<- predict(credit.rpart, newdata = tree.test, type="prob")
pred.test.orig = prediction(pred.tree.test.prob[,2], tree.test$response)
perf.test.orig = performance(pred.test.orig, "tpr", "fpr")
p_orig <- plot(perf.test.orig, colorize=TRUE, main = "Original")
unlist(slot(performance(pred.test.orig, "auc"), "y.values"))


#prune tree

plotcp(credit.rpart)
prune.original.tree <- prune(credit.rpart, cp = 0.015)
prp(prune.original.tree, extra = 1)
#in sample prediction prune
pred.tree.prune<- predict(prune.original.tree, type="class")
table(tree.train$response, pred.tree.prune, dnn = c("True", "Pred"))
mean(ifelse(tree.train$response != pred.tree.prune, 1, 0))

#out of sample prediction prune
pred.tree.test<- predict(prune.original.tree, newdata = tree.test, type="class")
table(tree.test$response, pred.tree.test, dnn = c("True", "Pred"))
mean(ifelse(tree.test$response != pred.tree.test, 1, 0))
count_correct = 0;
count_error = 0;
pred.tree.test_df <- as.data.frame(pred.tree.test)
for (i in 1:nrow(tree.test)){
if(tree.test$response[i] != pred.tree.test_df[i,1]){
  count_error = 1+count_error
}else{
  count_correct = 1 + count_correct
  }
  }
count_error
count_correct
error_rate_test <- count_error/(count_error+count_correct)
error_rate_test

prune.bag.pred.test<- predict(prune.original.tree, newdata = tree.test, type="prob")
pred.test.prune = prediction(prune.bag.pred.test[,2], tree.test$response)
perf.test.prune = performance(pred.test.prune, "tpr", "fpr")
p_prune <- plot(perf.test.prune, colorize=TRUE, main = "Prune")
unlist(slot(performance(pred.test.prune, "auc"), "y.values"))


#bagging
library(ipred)

credit.bag<- bagging(as.factor(tree.train$response)~., data = tree.train, nbagg=100)
credit.bag
credit.bag.pred.test<- predict(credit.bag, newdata = tree.test, type="prob")[,2]

bag.oob<- bagging(response~., data = german_credit, coob=T, nbagg=100)
bag.oob

pred.tree.bag.train<- predict(credit.bag, data = tree.train, type="class")
table(tree.train$response, pred.tree.bag.train, dnn = c("True", "Pred"))
mean(ifelse(tree.train$response != pred.tree.bag.train, 1, 0))

pred.tree.bag.test<- predict(credit.bag, newdata = tree.test, type="class")
table(tree.test$response, pred.tree.bag.test, dnn = c("True", "Pred"))
mean(ifelse(tree.test$response != pred.tree.bag.test, 1, 0))
library(ROCR)


pred.test.bag = prediction(credit.bag.pred.test, tree.test$response)
perf.test.bag = performance(pred.test.bag, "tpr", "fpr")
p_bag <- plot(perf.test.bag, colorize=TRUE, main = "Bagging")
unlist(slot(performance(pred.test.bag, "auc"), "y.values"))

#Random forest
library(randomForest)
credit.rf <- randomForest(as.factor(response)~., data = tree.train)
credit.rf.pred<- predict(credit.rf, newdata = tree.train, type = "prob")[,2]

costfunc = function(obs, pred.p, pcut){
  weight1 = 1   # define the weight for "true=1 but pred=0" (FN)
  weight0 = 5    # define the weight for "true=0 but pred=1" (FP)
  c1 = (obs==1)&(pred.p<pcut)    # count for "true=1 but pred=0"   (FN)
  c0 = (obs==0)&(pred.p>=pcut)   # count for "true=0 but pred=1"   (FP)
  cost = mean(weight0*c1 + weight1*c0)  # misclassification with weight
  return(cost) # you have to return to a value when you write R functions
} 
p.seq = seq(0.01, 0.5, 0.01)
cost = rep(0, length(p.seq))  
for(i in 1:length(p.seq)){ 
  cost[i] = costfunc(obs = tree.train$response, pred.p = credit.rf.pred, pcut = p.seq[i])  
}
plot(p.seq, cost)
optimal.pcut= p.seq[which(cost==min(cost))]
credit.rf.pred.test<- predict(credit.rf, newdata=tree.test, type = "prob")[,2]
credit.rf.class.test<- (credit.rf.pred.test>0.29)*1
table(tree.test$response, credit.rf.class.test, dnn = c("True", "Pred"))
mean(ifelse(tree.test$response != credit.rf.class.test, 1, 0))

credit.rf.pred.train<- predict(credit.rf, newdata=tree.train, type = "prob")[,2]
credit.rf.class.train<- (credit.rf.pred.train>0.29)*1
table(tree.train$response, credit.rf.class.train, dnn = c("True", "Pred"))
mean(ifelse(tree.test$response != credit.rf.class.test, 1, 0))


library(ROCR)
pred.rf <- prediction(credit.rf.pred.test, tree.test$response)
perf.rf <- performance(pred.rf, "tpr", "fpr")
unlist(slot(performance(pred.rf, "auc"), "y.values"))
p_rf <- plot(perf.rf, colorize=TRUE, main = "Random Forest")


pred.tree.rf.train<- predict(credit.rf, newdata = tree.train, type="class")
table(tree.train$response, pred.tree.rf.train, dnn = c("True", "Pred"))
mean(ifelse(tree.train$response != pred.tree.rf.train, 1, 0))

#boosting
#install.packages("adabag")
library(adabag)

credit.boost= boosting(response~., data = tree.train, boos = T)
save(credit.boost, file = "credit.boost.Rdata")
# Testing AUC
pred.credit.boost= predict(credit.boost, newdata = tree.test, type="prob")
pred.boost<- prediction(pred.credit.boost$prob[,2], tree.test$response)
perf.boost <- performance(pred.boost, "tpr", "fpr")
p_boost <- plot(perf.boost, colorize=TRUE, main = "Boosting")



pred.tree.boost.test.class <- predict(credit.boost, newdata=tree.test, type="class")
table(tree.test$response, pred.tree.boost.test.class$class, dnn = c("True", "Pred"))
mean(ifelse(tree.test$response != pred.tree.boost.test.class$class, 1, 0))

pred.tree.boost.train.class <- predict(credit.boost, newdata=tree.train, type="class")
table(tree.train$response, pred.tree.boost.train.class$class, dnn = c("True", "Pred"))
mean(ifelse(tree.train$response != pred.tree.boost.train.class$class, 1, 0))


par(mfrow=c(2,2))
plot(perf.test.orig, colorize=TRUE, main = "Original")
plot(perf.test.bag, colorize=TRUE, main = "Bagging")
plot(perf.rf, colorize=TRUE, main = "Random Forest")
plot(perf.boost, colorize=TRUE, main = "Boosting")
unlist(slot(performance(pred.test.orig, "auc"), "y.values"))
unlist(slot(performance(pred.rf, "auc"), "y.values"))
unlist(slot(performance(pred.test.bag, "auc"), "y.values"))
unlist(slot(performance(pred.boost, "auc"), "y.values"))



#Get the AUC
unlist(slot(performance(pred.boost, "auc"), "y.values"))

#GAM
credit.data = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
#http://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)

colnames(credit.data)=c("chk_acct","duration","credit_his","purpose","amount","saving_acct","present_emp","installment_rate","sex","other_debtor","present_resid","property","age","other_install","housing","n_credits","job","n_people","telephone","foreign","response")
if(min(credit.data$response)>0){
credit.data$response = credit.data$response-1}
#orginal response coding 1= good, 2 = bad
#we need 0 = good, 1 = bad
set.seed(12825604)
id_train <- sample(nrow(credit.data),nrow(credit.data)*0.75)
credit.train = credit.data[id_train,]
credit.test = credit.data[-id_train,]

costfunc = function(obs, pred.p, pcut){
  weight1 = 1   # define the weight for "true=1 but pred=0" (FN)
  weight0 = 5    # define the weight for "true=0 but pred=1" (FP)
  c1 = (obs==1)&(pred.p<pcut)    # count for "true=1 but pred=0"   (FP)
  c0 = (obs==0)&(pred.p>=pcut)   # count for "true=0 but pred=1"   (FN)
  cost = mean(weight0*c1 + weight1*c0)  # misclassification with weight
  return(cost) # you have to return to a value when you write R functions
}
p.seq = seq(0.01, 0.5, 0.01)
cost = rep(0, length(p.seq))  
for(i in 1:length(p.seq)){ 
  cost[i] = costfunc(obs = credit.train$response, pred.p = prob.gam.in, pcut = p.seq[i])  
}
plot(p.seq, cost)
optimal.pcut= p.seq[which(cost==min(cost))]

library(mgcv)
colnames(credit.data)
sapply(credit.data, class)

credit.gam <- gam(as.factor(response)~s(duration)+s(amount)+installment_rate+present_resid+age+n_credits+n_people+chk_acct+credit_his+purpose+saving_acct+present_emp+sex+other_debtor+property+other_install+housing+job+telephone+foreign, family=binomial,data=credit.train)
summary(credit.gam)
par(mfrow=c(1,2))
plot(credit.gam, shade=TRUE,seWithMean=TRUE,scale=0, pages = 1)

prob.gam.in<-predict(credit.gam,credit.train,type="response")
pred.gam.in<-(prob.gam.in>=optimal.pcut)*1
table(credit.train$response,pred.gam.in,dnn=c("Observed","Predicted"))
MSE_gam_in <- mean((pred.gam.in-credit.train$response)^2)
MSE_gam_in

prob.gam.out<-predict(credit.gam,credit.test,type="response")
pred.gam.out<-(prob.gam.out>=optimal.pcut)*1
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
plot.nnet(credit.nnet)
pred.nnet.train = as.numeric(prob.nnet.train > 0.27)
table(credit.train$response,pred.nnet.train, dnn=c("Observed","Predicted"))
mean(ifelse(credit.train$response != pred.nnet.train, 1, 0))

library(neuralnet)
#if variables are quantitative
n <- names(credit.train)
f <- as.formula(paste("response ~", paste(n[!n %in% "response"], collapse = " + ")))

credit.nnet <- neuralnet(f, data=credit.train,act.fct = "logistic",
                    linear.output = FALSE, lifesign = "minimal")
plot(credit.nnet)

#if variables are categorical, make dummy variables first
install.packages("fastDummies")
library(knitr)
library(fastDummies)
results <- fastDummies::dummy_cols(credit.train)
#knitr::kable(results)

results[,] <- lapply(results[,], as.numeric)

f <- as.formula(paste("response ~", paste(n[!n %in% "response"], collapse = " + ")))

credit.nnet <- neuralnet(f, data=results,act.fct = "logistic", hidden = c(3, 14, 3),

                         linear.output = FALSE)
plot(credit.nnet)
