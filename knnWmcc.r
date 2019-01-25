#data1 = read.csv("C:\\Users\\Abhinav Raj\\Downloads\\Data sets\\breast-cancer-wisconsin.data",header = TRUE)
#-------------------------Reading data--------------------------------------------------------------------
x=data.matrix(iris[-c(1:50),])
y=x[sample(100,replace=FALSE),]
train=y[1:80,]
test=y[81:100,]
knn<-function(x,y,k){
  test_label=c()
  #--------------------- function will take train data,test data & k and will give the accuracy-----------
  for (i in 1:dim(y)[1]) {
    z=matrix( rep(y[i,],dim(x)[1]),dim(x)[1],byrow=T )
    d_vec=sqrt(rowSums((x[,-dim(x)[2]]-z[,-dim(z)[2]])^2))	#distance calculated
    n_vec=order(d_vec)[1:k]	#row index of nearest k training data sets
    
    #-----------------------if k=1 then we will get a vector which needs to converted into matrix---------
    if (k==1){
      compare_matrix=t(as.matrix(x[n_vec,]))
    }#END:if 
    
    else{
      compare_matrix= x[n_vec,]#the marix of k neareast training set data
    }#END:else
    #--------------------------------- got compare_matrix ---------------------------------------------- 
    tbl=table(compare_matrix[,dim(x)[2]])#making table of the species/labels 
    sort_table=sort(tbl)#sorted table
    final_label=names(sort_table[dim(tbl)])#final label for test data
    test_label[i]=as.numeric(final_label)
  }#END:for
  original_label= y[,dim(y)[2]]
  accuracy= (sum(original_label == test_label)/dim(y)[1])
#------------------------------------confusion matrix-----------------------------------------------------
  confusion_matrix=as.matrix(table(original_label,test_label))
  TP=confusion_matrix[1,1]
  FN=confusion_matrix[1,2]
  FP=confusion_matrix[2,1]
  TN=confusion_matrix[2,2]
  MCC= ((TP*TN)-(FP*FN))/(sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))
  return(MCC)
  #return(confusion_matrix)
}#END:function
#--------------------plot between MCC and various K--------------------------------------------------------
aa=seq(1,19,by=2)
bb=c()
for(i in 1:length(aa)){
  bb[i]=knn(train,test,aa[i])
}
plot(aa,bb,type='b',pch=20)
