###監督式學習
library(e1071)

np=ceiling(0.1*nrow(iris))

test.index=sample(1:nrow(iris),np)
test.data=iris[test.index,]
train.data=iris[-test.index,]

svm.model=svm(Species~.,data=train.data,type='C-classification',cost=10, gamma=10)

svm_predict=predict(svm.model,test.data[,-5])

confusion_matrix_test=table(pred=svm_predict,true=test.data[,5])
confusion_matrix_test

correct.svm=sum(diag(confusion_matrix_test))/sum(confusion_matrix_test)
correct.svm


######搜尋最佳的cost與gamma(可設定搜巡範圍)
tuned=tune.svm(Species~., data=train.data,gamma=10^(-3:-1), cost=10^(-1:1))
summary(tuned)
# cost=0.1, gamma=0.1誤差最小

model=svm(Species~.,data=train.data,kernel="radial",gamma=0.1, cost=0.1)
summary(model)

 svm_pred=predict(model, test.data[,-5])

 confusion_matrix_test=table(pred=svm_pred,true=test.data[,5])
 confusion_matrix_test

 correct.svm=sum(diag(confusion_matrix_test))/sum(confusion_matrix_test)
 correct.svm



