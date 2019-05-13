data=read.csv("C:\\Users\\Abhinav Raj\\Downloads\\Data sets\\cereals.csv",header = T)
data1=sapply(data,as.numeric)
data=data.frame(data1)
samplesize=0.6*nrow(data)
index=sample(seq_len(nrow(data)),size = samplesize)
datatrain=data[index,]
datatest=data[-index,]
max = apply(data , 2 , max)
min = apply(data , 2 , min)
scaled=as.data.frame(scale(data,center = min,scale=max-min))
#--------------------------------------------------------------------------------------
library(neuralnet)
trainNN=scaled[index,]
testNN=scaled[-index,]
NN=neuralnet(rating~.,trainNN,hidden=3,linear.output = T)
plot(NN)
predict_testNN = compute(NN, testNN[,c(1:5)])
predict_testNN=(predict_testNN$net.result*(max(data$rating)-min(data$rating)))+min(data$rating)
plot(datatest$rating,predict_testNN,col='blue',pch=16,ylab="predicted rating NN", xlab="real rating")
abline(0,1)
RMSE.NN = (sum((datatest$rating - predict_testNN)^2) / nrow(datatest)) ^ 0.5
