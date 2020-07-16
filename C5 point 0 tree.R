###監督式學習

library(C50)

np=ceiling(0.1*nrow(iris))
np

test.index=sample(1:nrow(iris),np)
test.data=iris[test.index,]
train.data=iris[-test.index,]

c=C5.0Control(subset=F,bands=0,winnow=F,noGlobalPruning = F,CF=0.25,minCase=2, fuzzyThreshold = F,sample=0,
              seed=sample.int(4096,size=1)-1L, earlyStopping = T)

iris_treeModel=C5.0(x=train.data[,-5], y=train.data$Species,control = c)

summary(iris_treeModel)

test_predict=predict(iris_treeModel,test.data[,-5], type="class")
# 計算正確率
n=length(test_predict)
number=0
for(i in 1:n){
  if(test_predict[i]==test.data[i,5]){
    number=number+1
  }
}
test_accuracy=number/n
test_accuracy

########使用sample引數
cc=C5.0Control(subset=F,bands=0,winnow=F,noGlobalPruning = F,CF=0.25,minCase=2, fuzzyThreshold = F,sample=0.9,
              seed=sample.int(4096,size=1)-1L, earlyStopping = T,label = "Species")
iris_treeModel2=C5.0(x=train.data[,-5], y=train.data$Species,control = cc)
summary(iris_treeModel2)

test_predict2=predict(iris_treeModel2,test.data[,-5], type="class")
# 計算正確率
n=length(test_predict2)
number=0
for(i in 1:n){
  if(test_predict2[i]==test.data[i,5]){
    number=number+1
  }
}
test_accuracy=number/n
test_accuracy
