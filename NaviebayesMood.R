install.packages('caTools')
install.packages('e1071')

dataset=read.csv('data.csv')

library(caTools)
splits=sample.split(dataset$Mood, SplitRatio = 0.75)
train_set=subset(dataset,splits==TRUE)
test_set=subset(dataset,splits==FALSE)

library(e1071)
classifier=naiveBayes(x=train_set[,-9],
                      y=train_set$Mood,
                      formula=Mood~.)

Y_pred=predict(classifier, newdata=test_set[,-9])
cm=table(test_set[, 9],Y_pred)