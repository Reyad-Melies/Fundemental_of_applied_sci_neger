library(arules)
library(arulesViz)
dataset=read.transactions('D:/....... UOTTWA/Fundemental of applied sci neger/Assignment/4-5/transactions.csv',header = FALSE,sep = ",",format = "basket",cols = NULL)
# look at the first five transactions
inspect(head(dataset,10))


inspect(dataset[1:5])
# examine the frequency of items
itemFrequency(dataset[, 1:10])
#a) Generate a plot of the top 10 transaction
itemFrequencyPlot(dataset, topN = 10)


apriori(dataset)

model <- apriori(dataset, parameter = list(support =
                                                      0.002, confidence = 0.20, maxlen = 3))
model
# summary of grocery association rules
summary(model)

# look at the first three rules
inspect(model[1:3])

# sorting model rules by lift to determine actionable rules
inspect(sort(model, by = "lift"))


#i) Which rule has the greatest lift?
inspect(sort(model, by = "lift")[1])

#i) Which rule has the greater support?
#inspect(sort(model, by = "support")[1])



model1 <- apriori(dataset, parameter = list(support =
                                             0.002, confidence = 0.20, maxlen = 2))
model1
summary(model1)
inspect(sort(model, by = "lift")[1])#3

inspect(sort(model1, by = "lift")[1])#2



#iii) If you were a marketing manager, and could fund only one of these rules, which would it be, and why?

