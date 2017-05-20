install.packages('caTools')

dataset=read.csv('data.csv')
dataset=dataset[-1]

library(caTools)
splits=sample.split(dataset$Mood, SplitRatio = 0.75)
train_set=subset(dataset,splits==TRUE)
test_set=subset(dataset,splits==FALSE)

library(e1071)
classifier=svm(formula=Mood~.,
               data= train_set,
               type='C-classification', 
               kernel='linear')

Y_pred=predict(classifier, type='response', newdata=test_set[,-9])

#Making the Confusion Matrix
cm=table(test_set[, 9],Y_pred)
