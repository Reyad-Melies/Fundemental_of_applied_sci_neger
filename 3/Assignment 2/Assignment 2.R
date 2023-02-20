install.packages("tidyverse")
install.packages("tidyr")
install.packages("caret")
install.packages('rattle')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('RColorBrewer')

library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(tidyverse) #perform data manipulation & visualization 
library(tidyr)
library(caret) # cross - validation methods
library(rpart)
library(rpart.plot)
library(ggplot2)
library(corrplot)
library(reshape2)
library(ggplot2)

dataset <-read.csv("C:/Users/Reyad/Desktop/Fundemental of applied sci neger/Assignment/3/hypothyroid.csv")
cols.num <- c("age","TSH","T3","TT4","T4U","FTI")
dataset[cols.num] <- sapply(dataset[cols.num],as.numeric)
cols.num <- c("sex","on_thyroxine","query_on_thyroxine","on_antithyroid_medication","sick","pregnant","thyroid_surgery","I131_treatment","query_hypothyroid","query_hyperthyroid","lithium","goitre","tumor","psych","referral_source","Class","hypopituitary","TSH_measured","T3_measured","TT4_measured","T4U_measured","FTI_measured","TBG_measured")
dataset[cols.num] <-lapply(dataset[cols.num],as.factor)

#####################################################################?=na
dataset[dataset == '?'] <- NA
dataset = dataset[,!(names(dataset) %in% c("TBG"))]
dataset=dataset %>% drop_na()



#####################################################################

cont = subset(dataset, select = c(age, TSH, T3, TT4, T4U, FTI))
corrplot(cor(cont),        # Correlation matrix
         method = "circle",                # Correlation plot method (method = number, circle, pie, or color)
         type = "full",                   # Correlation plot style (also "upper" and "lower")
         diag = TRUE,                     # If TRUE (default), adds the diagonal
         tl.col = "black",                # Labels color
         bg = "white",                    # Background color
         title = "",                      # Main title
         col = NULL,                      # Color palette
         tl.cex =0.7,
         cl.ratio =0.2)                            



corrplot(cor(cont,method = "pearson"),diag = FALSE,
         method = "ellipse",
         tl.cex = 0.7, tl.col = "black", cl.ratio = 0.2
)

str(dataset)
summary(dataset)

#remove outliers
dataset=dataset[dataset$age != 455, ]
dataset=dataset[dataset$hypopituitary != "t", ]

####################dataset=dataset[dataset$Class != "secondary_hypothyroid", ]




#drop these columns
dataset = dataset[,!(names(dataset) %in%
                       c('hypopituitary',"TSH_measured","T3_measured",
                         "TT4_measured","T4U_measured","FTI_measured","TBG_measured"))]

#################################################################Splitting
set.seed(123) #generates a reproducible random sampling
ctrl <- trainControl(method = "cv", number = 10)
#fit a decision tree model and use k-fold CV to evaluate performance
dtree_fit_gini <- train(Class~., data = dataset, method = "rpart", parms = list(split = "gini"), trControl = ctrl, tuneLength = 5)
####################################################

#dtree_fit_gini111 <- rpart(Class~., data = dataset, method = "class", parms = list(split = "chi"), control = rpart.control(cp=0))
dtree_fit_gini111 <- rpart(Class~., data = dataset, method = "class", parms = list(split = "gini"), control = rpart.control(cp=0,maxdepth = 5,minsplit = 10))
dtree_fit_gini111
####################################################


#Step 5: Evaluate - view summary of k-fold CV               
print(dtree_fit_gini) #metrics give us an idea of how well the model performed on previously unseen data
plot(dtree_fit_gini)

#view final model
dtree_fit_gini$finalModel
prp(dtree_fit_gini$finalModel, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function

#view predictions for each fold
dtree_fit_gini$resample

#############################################################################       Reduction
dtree_fit_reduction <- train(Class~., data = dataset, method = "rpart", parms = list(split = 'reduction'), trControl = ctrl, tuneLength = 10)
#Step 5: Evaluate - view summary of k-fold CV               
print(dtree_fit_reduction) #metrics give us an idea of how well the model performed on previously unseen data

#view final model
dtree_fit_reduction$finalModel
prp(dtree_fit_reduction$finalModel, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function

#view predictions for each fold
dtree_fit_reduction$resample
################################################################################      CHI
dtree_fit_chi <- train(Class~., data = dataset, method = "rpart", parms = list(split = 'reduction'), trControl = ctrl, tuneLength = 10)
#Step 5: Evaluate - view summary of k-fold CV               
print(dtree_fit_chi) #metrics give us an idea of how well the model performed on previously unseen data

#view final model
dtree_fit_chi$finalModel
prp(dtree_fit_chi$finalModel, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function

#view predictions for each fold
dtree_fit_chi$resample
##############################################################################Puring
prune=prune.rpart(dtree_fit_gini$finalModel,0.1)
prp(prune, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function
###################################################################################

