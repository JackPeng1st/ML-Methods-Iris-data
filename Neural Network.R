library("neuralnet")
library(DMwR)

var1=runif(100,min=0,max=100)

sqrt.data=data.frame(var1,Sqrt=sqrt(var1))

net.sqrt=neuralnet(Sqrt~var1, sqrt.data,hidden=10, threshold = 0.01)

net.sqrt

plot(net.sqrt)


test_data=as.data.frame((1:10)^2)
nn.result=compute(net.sqrt,test_data)
nn.result

regr.eval(sqrt(test_data[,1]),nn.result$net.result[,1],stats=c('mae','rmse'))
