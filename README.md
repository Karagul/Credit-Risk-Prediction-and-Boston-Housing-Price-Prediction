# Linear-Non-Linear-Decision-Trees-Neural-Network-Logistic-regression

Goal and Background:
The problem at hand is to fit generalized linear model, tree model (Random forest, Bagging, Boosting), Generalised Additive models and Neural Network for Boston Housing dataset and German Credit Scoring dataset. We need to do these for regression and classification problems and the response variable of Boston Housing dataset is quantitative and of German Credit Scoring dataset is qualitative.  
Approach:

For Boston Dataset:
•	Divided the dataset into 75% train and 25% test dataset.
•	Created a linear model using all the variables, with stepwise and best subset variable selection methods on training dataset.
•	Calculated in sample and out of sample prediction error for all the models.
•	Created tree model using all the variables, bagging, boosting and random forest.
•	Calculated in sample and out of sample prediction error for all the tree models.
•	Built GAM model using all the variables and revised the model using only non – linear variables.
•	Calculated in sample and out of sample prediction error for GAM models.
•	Designed neural network after scaling the data.
•	Calculated in sample and out of sample prediction error for Neural Network model.

For Credit Scoring Dataset:
•	Divided the dataset into 75% train and 25% test dataset.
•	Created a logistic model using all the variables, with stepwise, LASSO and best subset variable selection methods on training dataset.
•	Calculated optimal cutoff probability using 5:1 asymmetric cost function.
•	Calculated in sample and out of sample confusion matrix.
•	Created a classification tree to calculate in sample and out of sample confusion matrix and error.
•	Built GAM model using all the variables to calculate in sample and out sample prediction error.
•	Designed neural network and calculated in sample and out of sample prediction error.


