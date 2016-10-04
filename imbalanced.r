setwd("C:/Users/abc/Downloads/DataSets/imb_class")
train_data <- read.csv("train.csv", na.strings = c("", " ", ",","NA",NA))
test_data <- read.csv("test.csv", na.strings = c("", " ", ",","NA",NA))

#Data Exploration
str(train_data)
unique(train_data$income_level)
install.packages("data.table")
library(data.table)
#Recoding the income level variable
train_data$income_level <- ifelse(train_data$income_level==-50000,0,1)
table(train_data$income_level) #Highly biased
test_data$income_level <- ifelse(test_data$income_level==-50000,0,1)
table(test_data$income_level)

#set column classes

factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols)

for(i in factcols){
  train_data[,i] <- as.factor(train_data[,i])
}

for(i in numcols){
  train_data[,i] <- as.numeric(train_data[,i])
}

for(i in factcols){
  test_data[,i] <- as.factor(test_data[,i])
}

for(i in numcols){
  test_data[,i] <- as.numeric(test_data[,i])
}

#subset categorical variables
cat_train <- train_data[,factcols]
cat_test <- test_data[,factcols]

#subset numerical variables
num_train <- train_data[,numcols]
num_test <- test_data[,numcols] 

#Writing to save the changes
write.csv(train_data,"train_data_en")
write.csv(test_data, "test_data_en")
rm(train_data,test_data) #save memory

library(ggplot2)
install.packages("plotly")
library(plotly)

#Plot function for hist plot alongwith density
tr <- function(a){
  ggplot(data = num_train, aes(x= a, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
  ggplotly()
}

tr(num_train$age)
tr(num_train$capital_losses) # Highly right skewed can be normalized
# if unique values for above are less, it can be tabulated
# Good to check with response var

num_train$income_level <- cat_train$income_level

#create a scatter plot
ggplot(data=num_train,aes(x = age, y=wage_per_hour))+geom_point(aes(colour=income_level))+scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))
# this show that a lot of people in age range 25-65 are the one having significant wage/hr hence it supports
# the fact that age<20 might not belong to >50k category hence this variable can be binned!

tr(num_train$capital_gains) #Righ skewed
tr(num_train$dividend_from_Stocks)# Right Skewed
tr(num_train$num_person_Worked_employer) # Discreet
tr(num_train$weeks_worked_in_year)

#dodged bar chart
all_bar <- function(i){
  ggplot(cat_train,aes(x=i,fill=income_level))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}

all_bar(cat_train$class_of_worker)
# here only two categories seem to dominate hence others with frequency lesser than 5% can be combined
all_bar(cat_train$industry_code)
# All categories are distributed evenly
all_bar(cat_train$occupation_code)
# A particular code response is dominating the distribution
all_bar(cat_train$education)
# All the Children belong to income level 0 while for 1 participants with bachelors degree dominate
all_bar(cat_train$enrolled_in_edu_inst_lastwk)
#others can be clubbed
all_bar(cat_train$marital_status)

# Checking for missing values
colSums(is.na(num_train)) #NO missing values
colSums(is.na(num_test)) # No missing values

#checking for correlation among numerical variables
library(caret)
num_train$income_level <- NULL
ax <-findCorrelation(x = cor(num_train), cutoff = 0.7)
num_train <- num_train[,-ax]
#Variable weeks worked gets removed, so will remove from testdata as well
num_test$weeks_worked_in_year <- NULL

# Checking for missing values in categorical var
colSums(is.na(cat_train))
#6 vars have missing infor: country_self, country_father, country_mother, migrationbelt, migration reg, migration_msa, state of prev resd

mvtr <- sapply(cat_train, function(x){sum(is.na(x))/length(x)})*100
mvte <- sapply(cat_test, function(x){sum(is.na(x)/length(x))}*100)
mvtr #Some have around 50% missing values hence can be deleted
mvte # None here!

cat_train <- subset(cat_train, select = mvtr < 5 )
cat_test <- subset(cat_test, select = mvte < 5)

#set NA as Unavailable - train data
#convert to characters
for(i in 1:length(cat_train)){
  cat_train[,i] <- as.character(cat_train[,i])
}
cat_train[is.na(cat_train)] <- "Unavailable"
#convert back to factors
for(i in 1:length(cat_train)){
  cat_train[,i] <- as.factor(cat_train[,i])
}

#set NA as Unavailable - test data
#convert to characters
for(i in 1:length(cat_test)){
  cat_test[,i] <- as.character(cat_test[,i])
}
cat_test[is.na(cat_test)] <- "Unavailable"
#convert back to factors
for(i in 1:length(cat_test)){
  cat_test[,i] <- as.factor(cat_test[,i])
}

#combine factor levels with less than 5% values
#train
for(i in names(cat_train)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_train[[i]])) < p))
  levels(cat_train[[i]])[levels(cat_train[[i]]) %in% ld] <- "Other"
}

#test
for(i in names(cat_test)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_test[[i]])) < p))
  levels(cat_test[[i]])[levels(cat_test[[i]]) %in% ld] <- "Other"
}

install.packages("mlr")
library(mlr)

summarizeColumns(cat_train)[,"nlevs"]
summarizeColumns(cat_test)[,"nlevs"]
#Both have equal levels


cat_test$migration_msa <- NULL
cat_test$migration_reg <- NULL
cat_test$migration_within_reg <- NULL
cat_test$migration_sunbelt <- NULL

#create simple tables representing counts of unique values 

#Bin age for train
num_train$age <- cut(x = num_train$age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))
num_train$age <- as.factor(num_train$age)

#Bin age for test
num_test$age <- cut(x = num_test$age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))
num_test$age <- as.factor(num_test$age)

#Bin wage/hr variable, cap gain, losses, divfromstocks train
num_train$wage_per_hour <- ifelse(num_train$wage_per_hour == 0, "Zero", "MoreThanZero")
num_train$wage_per_hour <- as.factor(num_train$wage_per_hour)
num_train$capital_gains <- ifelse(num_train$capital_gains == 0, "Zero", "MoreThanZero")
num_train$capital_gains <- as.factor(num_train$capital_gains)
num_train$dividend_from_Stocks <- ifelse(num_train$dividend_from_Stocks == 0, "Zero", "MoreThanZero")
num_train$dividend_from_Stocks <- as.factor(num_train$dividend_from_Stocks)
num_train$capital_losses <- ifelse(num_train$capital_losses == 0, "Zero", "MoreThanZero")
num_train$capital_losses <- as.factor(num_train$capital_losses)

#Bin wage/hr variable, cap gain, losses, divfromstocks test
num_test$wage_per_hour <- ifelse(num_test$wage_per_hour == 0, "Zero", "MoreThanZero")
num_test$wage_per_hour <- as.factor(num_test$wage_per_hour)
num_test$capital_gains <- ifelse(num_test$capital_gains == 0, "Zero", "MoreThanZero")
num_test$capital_gains <- as.factor(num_test$capital_gains)
num_test$dividend_from_Stocks <- ifelse(num_test$dividend_from_Stocks == 0, "Zero", "MoreThanZero")
num_test$dividend_from_Stocks <- as.factor(num_test$dividend_from_Stocks)
num_test$capital_losses <- ifelse(num_test$capital_losses == 0, "Zero", "MoreThanZero")
num_test$capital_losses <- as.factor(num_test$capital_losses)

#Combine num an dcat data sets
d_train <- cbind(num_train,cat_train)
d_test <- cbind(num_test, cat_test) 

#Remove other vars
rm(num_train,num_test,cat_test,cat_train)

library(mlr)

#create task
train.task <- makeClassifTask(data = d_train,target = "income_level")
test.task <- makeClassifTask(data=d_test,target = "income_level")

#remove zero variance features as they are uninfrmative and might break the model
train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)

#Package for feature selection
install.packages("FSelector")
library(FSelector)

#get variable importance chart
var_imp <- generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)

# We'll be using all the methods of treating imbalanced datasets

# UnderSampling
train.under <- undersample(train.task,rate = 0.1) #keep only 10% of majority class
table(getTaskTargets(train.under))

# OverSampling
train.over <- oversample(train.task, rate = 15) # Minority enhances by 15 times
table(getTaskTargets(train.over))

#SMOTE- it will consume a lot of memory
train.smote <- smote(train.task,rate = 10,nn = 3) 
system.time(train.smote <- smote(train.task,rate = 10,nn = 3) )

#lets see which algorithms are available
listLearners("classif","twoclass")[c("class","package")]

#naive Bayes
naive_learner <- makeLearner("classif.naiveBayes",predict.type = "response")
naive_learner$par.vals <- list(laplace = 1)

#10 fold CV - stratified
folds <- makeResampleDesc("CV",iters=10,stratify = TRUE)

#cross validation function
fun_cv <- function(a){
  crv_val <- resample(naive_learner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
  crv_val$aggr
}

fun_cv (train.task)
fun_cv (train.under)
fun_cv (train.over)

# Since smote will not work on the machine among these oversampling is doing best hence we will train our model

#train and predict
nB_model <- train(naive_learner, train.over)
nB_predict <- predict(nB_model,test.task)

#evaluate
nB_prediction <- nB_predict$data$response
dCM <- confusionMatrix(d_test$income_level,nB_prediction)
dCM


#calculate F measure
precision <- dCM$byClass['Pos Pred Value']
recall <- dCM$byClass['Sensitivity']

f_measure <- 2*((precision*recall)/(precision+recall))
f_measure

# XGBoost
install.packages("xgboost")
library(xgboost)
set.seed(2002)
xgb_learner <- makeLearner("classif.xgboost",predict.type = "response")
xgb_learner$par.vals <- list(  objective = "binary:logistic",  eval_metric = "error",  nrounds = 150, print.every.n = 50)

#define hyperparameters for tuning
xg_ps <- makeParamSet( 
  makeIntegerParam("max_depth",lower=3,upper=10),
  makeNumericParam("lambda",lower=0.05,upper=0.5),
  makeNumericParam("eta", lower = 0.01, upper = 0.5),
  makeNumericParam("subsample", lower = 0.50, upper = 1),
  makeNumericParam("min_child_weight",lower=2,upper=10),
  makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80)
)

#define search function
rancontrol <- makeTuneControlRandom(maxit = 5L) #do 5 iterations

#5 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 5L,stratify = TRUE)

#tune parameters
xgb_tune <- tuneParams(learner = xgb_learner, task = train.task, resampling = set_cv, measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = xg_ps, control = rancontrol)

#set optimal parameters
xgb_new <- setHyperPars(learner = xgb_learner, par.vals = xgb_tune$x)

#train model
xgmodel <- train(xgb_new, train.task)

#test model
predict.xg <- predict(xgmodel, test.task)

#make prediction
xg_prediction <- predict.xg$data$response

#make confusion matrix
xg_confused <- confusionMatrix(d_test$income_level,xg_prediction)

precision <- xg_confused$byClass['Pos Pred Value']
recall <- xg_confused$byClass['Sensitivity']

f_measure <- 2*((precision*recall)/(precision+recall))
f_measure