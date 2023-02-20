#install.packages("zoo")                                    # Install & load zoo package
#install.packages("neuralnet")
#install.packages("imputeTS") # for replacing na with mean
#install.packages("nortest")
#normal mean gluc skinthi bmi
#median blp inc
library(caTools)
library(tidyr)
library(ggplot2)
library(neuralnet)
library("zoo")
library(imputeTS)
library(nortest)


dataset <-read.csv("C:/Users/Reyad/Desktop/Fundemental of applied sci neger/Assignment/3/diabetes.csv")
dataset[dataset == '?'] <- NA
cols.num <- c("Glucose","BloodPressure","SkinThickness","Insulin","BMI")
dataset[cols.num] <- sapply(dataset[cols.num],as.numeric)
sapply(dataset, class)
str(dataset)
########################################################################################
ggplot(gather(dataset), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')


ad.test(dataset$BloodPressure)


dataset[ , 1][is.na(dataset[ , 1])] <- median(dataset[ , i], na.rm = TRUE)
dataset[ , 4][is.na(dataset[ , 4])] <- median(dataset[ , i], na.rm = TRUE)
dataset[ , 8][is.na(dataset[ , 8])] <- median(dataset[ , i], na.rm = TRUE)
dataset=na_mean(dataset)



ggplot(gather(dataset), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')  

########################################################################################






##############################################################################################
max = apply(dataset , 2 , max)
min = apply(dataset, 2 , min)
dataset = as.data.frame(scale(dataset, center = min, scale = max - min))
dataset
##############################################################################################




set.seed(42)
sample_split <- sample.split(Y = dataset, SplitRatio = 0.75)
train_set <- subset(x = dataset, sample_split == TRUE)
test_set <- subset(x = dataset, sample_split == FALSE)

###############################################################################################2&&&&&&&&&&&&&&&&&&&&&&&&&4
NN2 = neuralnet(Outcome 
               ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, 
               data=train_set, hidden = c(5,5),linear.output = TRUE,stepmax = 9999999999)

predict_testNN2 = compute(NN2, test_set[,c(1:8)])
predict_testNN2 =round(predict_testNN2$net.result)
predict_testNN2 <- sapply(predict_testNN2,as.numeric)

table(test_set$Outcome,predict_testNN2)
accuracy <- mean(test_set$Outcome == predict_testNN2)
accuracy

plot(NN2, rep = 'best')







NN = neuralnet(Outcome 
               ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, 
               train_set, hidden = 2,linear.output = FALSE, err.fct = 'ce',likelihood = TRUE,stepmax = 9999999999)

plot(NN, rep = 'best')

predict_testNN = compute(NN, test_set[,c(1:8)])
predict_testNN = round(predict_testNN$net.result)
predict_testNN <- sapply(predict_testNN,as.numeric)


table(test_set$Outcome,predict_testNN)
accuracy <- mean(test_set$Outcome == predict_testNN)
accuracy




############################################################################################# activation function tanh
NNtanh = neuralnet(Outcome 
               ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, 
               train_set, hidden = 2,linear.output = TRUE, likelihood = TRUE,stepmax = 9999999999,act.fct="tanh")



plot(NNtanh, rep = 'best')

predict_testNN = compute(NNtanh, test_set[,c(1:8)])
predict_testNN = round(predict_testNN$net.result)
predict_testNN <- sapply(predict_testNN,as.numeric)

table(test_set$Outcome,predict_testNN)
accuracy <- mean(test_set$Outcome == predict_testNN)
accuracy


NNtanh = neuralnet(Outcome 
                   ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, 
                   train_set, hidden = c(5,5),linear.output = TRUE, likelihood = TRUE,stepmax = 9999999999,act.fct="tanh")



plot(NNtanh, rep = 'best')

predict_testNN = compute(NNtanh, test_set[,c(1:8)])


predict_testNN = round(predict_testNN$net.result)


predict_testNN <- sapply(predict_testNN,as.numeric)

table(test_set$Outcome,predict_testNN)


accuracy <- mean(test_set$Outcome == predict_testNN)
accuracy

############################################################################################# activation function
############################################################################################################softplus
softplus <- function(x) log(1 + exp(x))
NNsoftplus = neuralnet(Outcome 
                   ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, 
                   train_set, hidden = 2,linear.output = FALSE, likelihood = TRUE,stepmax = 9999999999,act.fct=softplus)

plot(NNsoftplus, rep = 'best')


predict_testNN = compute(NNtanh, test_set[,c(1:8)])
predict_testNN = round(predict_testNN$net.result)
predict_testNN <- sapply(predict_testNN,as.numeric)

table(test_set$Outcome,predict_testNN)
accuracy <- mean(test_set$Outcome == predict_testNN)
accuracy



NNsoftplus = neuralnet(Outcome 
                       ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, 
                       train_set, hidden = c(5,5),linear.output = FALSE, likelihood = TRUE,stepmax = 9999999999,act.fct=softplus)

plot(NNsoftplus, rep = 'best')


predict_testNN = compute(NNtanh, test_set[,c(1:8)])
predict_testNN = round(predict_testNN$net.result)
predict_testNN <- sapply(predict_testNN,as.numeric)

table(test_set$Outcome,predict_testNN)
accuracy <- mean(test_set$Outcome == predict_testNN)
accuracy



###################################################################################################################
#learning rate
NNlr = neuralnet(Outcome 
                   ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, 
                   train_set, hidden = 2,linear.output = FALSE, likelihood = TRUE,stepmax = 9999999999,learningrate = 0.99)

plot(NNlr, rep = 'best')

predict_testNN = compute(NNlr, test_set[,c(1:8)])
predict_testNN = round(predict_testNN$net.result)
predict_testNN <- sapply(predict_testNN,as.numeric)

table(test_set$Outcome,predict_testNN)
accuracy <- mean(test_set$Outcome == predict_testNN)
accuracy
######################################################################################################################
#epoch
NNepoch = neuralnet(Outcome 
                 ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, 
                 train_set, hidden = 2,linear.output = FALSE, likelihood = TRUE,stepmax = 9999999999,rep = 100)
predict_testNN = compute(NNepoch, test_set[,c(1:8)])
predict_testNN = round(predict_testNN$net.result)


predict_testNN <- sapply(predict_testNN,as.numeric)

table(test_set$Outcome,predict_testNN)



accuracy <- mean(test_set$Outcome == predict_testNN)
accuracy
plot(NNepoch, rep = 'best')
##########################################################################################################################
#Execluding bias
NN2 = neuralnet(Outcome 
                ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, 
                data=train_set, hidden = 2,linear.output = TRUE,stepmax = 9999999999)

NN2$weights[[c(1, 1)]][1,]=0
NN2$weights[[c(1, 2)]][1,]=0
NN2$weights

predict_testNN2 = compute(NN2, test_set[,c(1:8)])
predict_testNN2 =round(predict_testNN2$net.result)
predict_testNN2 <- sapply(predict_testNN2,as.numeric)

table(test_set$Outcome,predict_testNN2)
accuracy <- mean(test_set$Outcome == predict_testNN2)
accuracy

plot(NN2, rep = 'best')






#Execluding bias
NN2 = neuralnet(Outcome 
                ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, 
                data=train_set, hidden = c(5,5),linear.output = TRUE,stepmax = 9999999999)

NN2$weights[[c(1, 1)]][1,]=0
NN2$weights[[c(1, 2)]][1,]=0
NN2$weights

predict_testNN2 = compute(NN2, test_set[,c(1:8)])
predict_testNN2 =round(predict_testNN2$net.result)
predict_testNN2 <- sapply(predict_testNN2,as.numeric)

table(test_set$Outcome,predict_testNN2)
accuracy <- mean(test_set$Outcome == predict_testNN2)
accuracy

plot(NN2, rep = 'best')

##########################################################################################################################
#BEst Model
NNtanh = neuralnet(Outcome 
                   ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, 
                   train_set, hidden = 2,linear.output = TRUE, likelihood = TRUE,stepmax = 9999999999,act.fct="tanh",rep = 40,learningrate = 0.1)



plot(NNtanh, rep = 'best')

predict_testNN = compute(NNtanh, test_set[,c(1:8)])


predict_testNN = round(predict_testNN$net.result)


predict_testNN <- sapply(predict_testNN,as.numeric)

table(test_set$Outcome,predict_testNN)


accuracy <- mean(test_set$Outcome == predict_testNN)
accuracy
