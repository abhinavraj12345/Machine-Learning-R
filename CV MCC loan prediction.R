library(randomForest)
library(caret)
Data=read.csv("C:\\Users\\Abhinav Raj\\Downloads\\Data sets\\loan prediction.csv",
              header = T,na.string=c(""))
Da=na.roughfix(Data)
Da$ApplicantIncome=as.numeric(Da$ApplicantIncome)
Da$CoapplicantIncome=as.numeric(Da$CoapplicantIncome)
Da$LoanAmount=as.numeric(Da$LoanAmount)
Da$Loan_Amount_Term=as.numeric(Da$Loan_Amount_Term)
Da$Credit_History=as.factor(Da$Credit_History)
trainData=Da[,-1]
up_train=upSample(x=trainData[,-ncol(trainData)],y=trainData$Loan_Status,yname ="Loan_Status" )
up_train=up_train[sample(nrow(up_train)),]
#------------------------test data---------------------------------------------------
test_data=read.csv("C:\\Users\\Abhinav Raj\\Downloads\\Data sets\\loan test.csv",
                   header = T,na.string=c(""))
Ta=na.roughfix(test_data)
Ta$ApplicantIncome=as.numeric(Ta$ApplicantIncome)
Ta$CoapplicantIncome=as.numeric(Ta$CoapplicantIncome)
Ta$LoanAmount=as.numeric(Ta$LoanAmount)
Ta$Loan_Amount_Term=as.numeric(Ta$Loan_Amount_Term)
Ta$Credit_History=as.factor(Ta$Credit_History)
testData=Ta[,-1]

#----------------------------CV.RF.ntree.mtry function---------------------------------
CV.RF.ntree.mtry<-function(data,fold,a,b){ #other function parameters){
  d=floor(nrow(data)/fold)
  k=1:d
  train<-NULL
  test<-NULL
  MCC=NULL
  for(i in 1:fold){
    test=data[k,]
    train=data[-k,]
    cancer.rf=randomForest(formula=Loan_Status~.,data=train,mtry=a,ntree=b)#include function parameters 
    p=predict(cancer.rf,test)
    confusion_matrix=as.matrix(table(test$Loan_Status,p))
    if(sum(dim(confusion_matrix)==2)==2){
      TN=confusion_matrix[1,1]
      FP=confusion_matrix[1,2]
      FN=confusion_matrix[2,1]
      TP=confusion_matrix[2,2]
      MCC[i]= ((TP*TN)-(FP*FN))/(sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))
    }else{
      MCC[i]=1
    }
    k=d+k
  }#END:for
  return(mean(MCC))
}
#---------------------------------CV function-----------------------------------------
x<-seq(1,11,by=1)
y<-seq(50,500,by=50)
aa=matrix(NA,nrow=length(x),length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    aa[i,j]=CV.RF.ntree.mtry(up_train,5,x[i],y[j])
  }#END:j
}#END:i
accuracy.matrix=t(aa)
l=which(accuracy.matrix == max(accuracy.matrix), arr.ind = TRUE)
cat(sprintf("\n Maximum MCC is %f for mtry = %d and ntree = %d in cross-validation. \n \n",
            max(accuracy.matrix),x[l[1,2]],y[l[1,1]]))
#----------------------------------------------------------------------------------------
finaltree=randomForest(formula=Loan_Status~.,data=up_train,mtry=x[l[1,2]],ntree=y[l[1,1]])
prediction=data.frame(Ta$Loan_ID,predict(finaltree,testData))
colnames(prediction)=c("Loan_ID","Loan_Status")
write.csv(prediction, file = "loan_rf.csv",row.names=FALSE)






































