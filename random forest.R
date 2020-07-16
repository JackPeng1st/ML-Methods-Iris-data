#清除R記憶體資料
rm(list=ls())
gc()

library(randomForest)

ind=sample(2,nrow(iris),replace = T,prob=c(0.8,0.2))
train_data=iris[ind==1,]
test_data=iris[ind==2,]

rf_model=randomForest(Species~.,data=train_data,ntree=100)

pred_test=predict(rf_model,test_data)

confusion_matrix=table(pred_test,test_data$Species)
confusion_matrix
correct_rate=sum(diag(confusion_matrix))/sum(confusion_matrix)
correct_rate
