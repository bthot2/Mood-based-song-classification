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

#Applying the LDA
library(caret)
library(MASS)
lda=lda(formula=Mood~., data=train_set)
train_set=as.data.frame(predict(lda, train_set))
train_set= train_set[c(6,7,8,1)]

test_set=as.data.frame(predict(lda, test_set))
test_set=test_set[c(6,7,8,1)]

#Fitting a model
library(e1071)
classifier=svm(formula=class~.,
               data= train_set,
               type='C-classification', 
               kernel='linear')

#Probability
Y_pred=predict(classifier, type='response', newdata=test_set[,-4])

#Making the Confusion Matrix
cm=table(test_set[, 4],Y_pred)

#Visualizing the training set
#install.packages('ElemStatLearn')
library(ElemStatLearn)
set = train_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'SVM(Training set)',
     xlab = 'X.LD1', ylab = 'X.LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid==2,'deepskyblue', 
                                         ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[,2]==2,'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid==2,'deepskyblue', 
                                         ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[,2]==2,'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))
