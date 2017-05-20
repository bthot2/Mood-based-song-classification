dataset=read.csv('data.csv')
dataset=dataset[-1]

library(caTools)
set.seed(123)
splits=sample.split(dataset$Mood, SplitRatio = 0.8)
train_set=subset(dataset,splits==TRUE)
test_set=subset(dataset,splits==FALSE)

#Feature Scaling
train_set[,-8]=scale(train_set[,-8])
test_set[,-8]=scale(test_set[,-8])

#Applying the PCA
library(caret)
library(e1071)
pca=preProcess(x=train_set[-8], method='pca', pcaComp = 3)
train_set=predict(pca, train_set)
train_set= train_set[c(2,3,4,1)]

test_set=predict(pca, test_set)
test_set=test_set[c(2,3,4,1)]

#Fitting a model
library(e1071)
classifier=svm(formula=Mood~.,
               data= train_set,
               type='C-classification', 
               kernel='linear')

#Probability
Y_pred=predict(classifier, type='response', newdata=test_set[,-4])

#Making the Confusion Matrix
cm=table(test_set[, 4],Y_pred)
