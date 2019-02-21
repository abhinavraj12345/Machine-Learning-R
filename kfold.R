data<-iris
fold=16
#-------------------------------------------------------------
test=NULL
train=NULL
d=floor(nrow(data)/fold)
k=1:d
for(i in 1:fold){
  test[[i]]=data[k,]
  train[[i]]=data[-k,]
  k=d+k
}
#-------------------------------------------------------------
