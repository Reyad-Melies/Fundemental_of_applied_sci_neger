install.packages("lsa")
install.packages("matlib")
library(lsa)
library(matlib)
library(stringi)
library(reshape2)
library(recommenderlab)
dataset=read.csv("D:/....... UOTTWA/Fundemental of applied sci neger/Assignment/4-5/PART2.csv")
rownames(dataset)=dataset$X
dataset = as.matrix(dataset[,-1])
coss=t(dataset)
coss[is.na(coss)]=0
temp=cosine(coss)



#Convert ratings matrix to real rating matrx which makes it dense
datsetmatrix = as(dataset, "realRatingMatrix")
model = Recommender(datsetmatrix, method = "UBCF", param=list(method="Cosine",normalize=NULL,nn=5)) 
Top_pred = predict(model, datsetmatrix[4], n=1)
#Convert the recommendations to a list
Top_List = as(Top_pred, "list")
Top_List


#Apply item-based collaborative filtering to this dataset
model = Recommender(datsetmatrix, method = "IBCF", param=list(method="pearson")) 
Top_pred = predict(model, datsetmatrix[4], n=1)
#Convert the recommendations to a list
Top_List = as(Top_pred, "list")
Top_List




#We convert the list to a dataframe and change the column name to student intials
Top_df=data.frame(Top_List)
colnames(Top_5_df)="movieId"
