install.packages('rpart')
dataset=read.csv('data.csv')
dataset=dataset[,-1]

library(caTools)
splits=sample.split(dataset$Mood, SplitRatio = 0.75)
train_set=subset(dataset,splits==TRUE)
test_set=subset(dataset,splits==FALSE)

library(rpart)
classifier=rpart(formula=Mood~., data=train_set)
Y_pred=predict(classifier, newdata = test_set[-9], type = 'class')
cm=table(test_set[,8],Y_pred)