#清除R記憶體資料
rm(list=ls())
gc()

library(adabag)

ind=sample(2,nrow(iris),replace=T,prob=c(0.8,0.2))
train_data=iris[ind==1,]
test_data=iris[ind==2,]

model=boosting(Species~., data=train_data,boos=T,mfinal = 5)

pred_test=predict.boosting(model,newdata=test_data)
pred_test$confusion
pred_test$error
correct_rate=1-pred_test$error
correct_rate
