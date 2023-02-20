library(lattice)
library(caret)
library(dplyr)
library(party)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(pROC)
library(e1071)
library(ranger)
library(pROC)
#install.packages("ROSE")
library(ROSE)
rm(list = ls(all.names = TRUE));  gc();


dataset <- read.csv("D:/....... UOTTWA/Fundemental of applied sci neger/Assignment/4/customer_churn.csv",header=TRUE)
cols.num <- c('gender'
              ,'SeniorCitizen'
              ,'Partner'
              ,'Dependents'
              ,'PhoneService'
              ,'MultipleLines'
              ,'OnlineSecurity'
              ,'OnlineBackup'
              ,'DeviceProtection'
              ,'TechSupport'
              ,'StreamingTV'
              ,'StreamingMovies'
              ,'PaperlessBilling'
              ,'Contract'
              ,'PaymentMethod'
              ,'InternetService'
              ,'Churn')

dataset[cols.num] <- lapply(dataset[cols.num], factor)

##library(ggplot2)

##ggplot(stack(dataset), aes(x = ind, y = values)) +
##  geom_boxplot()

glimpse(dataset)
str(dataset)
summary(dataset)
#table(dataset)
#boxplot(dataset)


dataset <- na.omit(dataset)
dataset=dataset[,2:21]
###############################################################################################################
#a) Partition the data set using the holdout method,
#so that 67% of the records are included in the training data set and 33% are included in the test data set.
#Use a bar graph to confirm your proportions.

# Holdout method
no.m_Of_Records=nrow(dataset)

# Holdout method
# using caret function
in_train <- createDataPartition(dataset$Churn, p = 0.67, list = FALSE)
train <- dataset[in_train, ]
test <- dataset[-in_train, ]

train_ <- nrow(train)
test_ <- nrow(test)
total<- c(nrow(train),nrow(test))
total_names=c('Train','Test')
barplot(total,names.arg=total_names,xlab="Dataset Types",ylab="Record numbers",col="blue",
        main="Records",border="red")
percentage=train_/(train_+test_)*100
percentage

total1<- c(train_/(train_+test_)*100,test_/(train_+test_)*100)
barplot(total1,names.arg=total_names,xlab="Dataset Types",ylab="Record numbers",col="red",
        main="Records",border="red")

#####################################################################################################################
#Identify the total number of records in the training data set and how many records in the
#training data set have a churn value of true (or 1). 
#Calculate how many true churn recordsyou need to resample in order to have 20% of the rebalanced data set have true churn values.
train_ <- nrow(train)
total_records_train=c(nrow(train))
total_records_train
true_train=train[train$Churn == "Yes", ] 
no.m_Of_Records_true=nrow(true_train)
no.m_Of_Records_true
rebalance_needed=no.m_Of_Records_true/total_records_train*100
rebalance_needed
rebalance_needed=rebalance_needed-30
rebalance_needed
total_records_train
rebalance_needed=rebalance_needed*total_records_train/100
rebalance_needed=abs(rebalance_needed)
rebalance_needed
(rebalance_needed+no.m_Of_Records_true)/total_records_train
(no.m_Of_Records_true-rebalance_needed)/total_records_train*100
######################################################################################################################
#c) Perform the rebalancing described in (b) 
#and confirm that 20% of the records in the rebalanced data set have true churn values.
train<- ovun.sample(Churn~., data = train, method = "over", p = 0.3)$data
#######################################################################################################################
#d) Create a decision tree model that can predict Churn using the data set given. Use 
#predictors you think are appropriate and obtain the predicted value.

ctrl <- trainControl(selectionFunction='best',classProbs = TRUE,savePredictions = TRUE,
                     summaryFunction = twoClassSummary)
#specify method as class since we are dealing with classification
model <- rpart(Churn ~ ., data = train, method = "class") 
# summarize importance
# estimate variable importance
importance <- varImp(model, scale=FALSE)
print(importance)
# plot importance
plot(importance)


drop <- c('gender','SeniorCitizen','Partner',
          'Dependents','PhoneService','MultipleLines'
          ,'DeviceProtection','StreamingTV','Churn')

######selecting Features
train_features_selected = train[,(names(train) %in% drop)]
test_features_selected = test[,(names(test) %in% drop)]
model <- train(Churn ~ ., data = train_features_selected,
               method = "rpart", metric = "ROC",trControl = ctrl)
#plot the model
rpart.plot(model$finalModel)

# summarize importance
# estimate variable importance
importance <- varImp(model, scale=FALSE)
print(importance)
# plot importance
plot(importance)
#Make predictions
predsTree <- predict(model, newdata = test_features_selected) #use the predict() function and pass in the testing subset
predsTree

#Print the confusion Matrix
confusionMatrix(test_features_selected$Churn, predsTree)

######################################################################################################################

#e) Use an ensemble method (e.g., Random Forest, Adaboost) to obtain the predicted value 
#of Churn. Tune the hyper-parameters (e.g., node size, max depth, max terminal nodes, 
#                                     etc.) of the ensemble model and compare against the initial model

## Random Forests ---- improving performance
# random forest with default settings

set.seed(300)
str(dataset)
glimpse(dataset)
#m_rf <- randomForest(Churn ~ ., data = train)
#m_rf <- train(Churn ~ ., data = train, method = "ranger")
#rfpreds <- predict(m_rf, data = test) #use the predict() function and pass in the testing subset



rfpreds <- train(Churn ~ ., data = train , metric = "ROC" , method = "ranger",trControl = ctrl)
fpreds <- predict(rfpreds, newdata = test)
#Print the confusion Matrix
confusionMatrix(test$Churn, fpreds)


#tune


grid_rf <- expand.grid(
  mtry = 2:7,
  splitrule="gini",
  min.node.size=2:5
  
  )
m_rftune <- train(Churn ~ ., data = train, method = "ranger",
              tuneGrid = grid_rf,trControl = ctrl )

rfpredstune <- predict(m_rftune, newdata = test) 

m_rftune$finalModel$min.node.size
m_rftune$finalModel$mtry

#Print the confusion Matrix
confusionMatrix(test$Churn, rfpredstune)


roc_rf_tune <- roc(m_rftune$pred$obs, m_rftune$pred$Yes)
roc_rf <- roc(rfpreds$pred$obs, rfpreds$pred$Yes)
tree <- roc(model$pred$obs, model$pred$Yes)
plot(roc_rf_tune, col = "red", legacy.axes = TRUE)
plot(roc_rf, col = "green", add = TRUE)
plot(tree, col = "blue", add = TRUE)
 
