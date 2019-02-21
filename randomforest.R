
data=read.csv("C:\\Users\\Abhinav Raj\\Downloads\\Data sets\\breast-cancer.csv",header = T)
#--------------------data cleaning--------------------------------
cdata=data[,1:32]
fdata= cdata[,-1]#final data
v=1: floor(nrow(fdata)*0.8) 
train_data=fdata[v,]
test_data=fdata[-v,]
#---------------------random forest--------------------------------
library(randomForest)
cancer.rf=randomForest(diagnosis~.,data=fdata)
p=predict(cancer.rf,test_data)
accuracy=mean(test_data$diagnosis==p)