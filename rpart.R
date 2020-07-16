###監督式學習
library(rpart)
library(rpart.plot)
library(partykit)
iris
np=ceiling(0.1*nrow(iris))
np

test.index=sample(1:nrow(iris),np) #隨機抽取10%資料當testing data
test.index

test.data=iris[test.index,]
train.data=iris[-test.index,]

iris.tree=rpart(Species~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, method = "class", data=train.data)
iris.tree
summary(iris.tree)
#######################  https://rstudio-pubs-static.s3.amazonaws.com/275285_90aaf9a2a64d43a5846a86dbcde8eba9.html
# 法一繪製
plot(iris.tree)
text(iris.tree)
# 法二繪製
prp(iris.tree
    ,faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)
# 法三繪製
rparty.tree <- as.party(iris.tree)# 轉換cart決策樹
rparty.tree
plot(rparty.tree)
##################計算訓練資料的正確率
train.ans=iris$Species[-test.index]
train.predict=factor(predict(iris.tree,train.data,type='class'),levels=levels(train.ans))
confus.matrix_train <-table(train.ans,train.predict)
sum(diag(confus.matrix_train))/sum(confus.matrix_train) # 對角線的數量/總數量

#################計算測試資料的正確率
test.ans=iris$Species[test.index]
test.predict=factor(predict(iris.tree,test.data,type='class'),levels=levels(test.ans))
confus.matrix_test=table(test.ans,test.predict)
sum(diag(confus.matrix_test))/sum(confus.matrix_test)

########## 改善決策樹分類結果 rpart.control()
iris.tree=rpart(Species~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, method = "class", data=train.data,
                control=rpart.control(minsplit = 5,cp=0.0001, maxdepth = 30))

################## 改善後   計算訓練資料的正確率
train.ans=iris$Species[-test.index]
train.predict=factor(predict(iris.tree,train.data,type='class'),levels=levels(train.ans))
confus.matrix_train <-table(train.ans,train.predict)
sum(diag(confus.matrix_train))/sum(confus.matrix_train) # 對角線的數量/總數量

################# 改善後 計算測試資料的正確率
test.ans=iris$Species[test.index]
test.predict=factor(predict(iris.tree,test.data,type='class'),levels=levels(test.ans))
confus.matrix_test=table(test.ans,test.predict)
sum(diag(confus.matrix_test))/sum(confus.matrix_test)

