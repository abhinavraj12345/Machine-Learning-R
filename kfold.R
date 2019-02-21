data<-iris
fold=16
#-------------------------------------------------------------
test=NULL
train=NULL
d=floor(nrow(iris)/fold)
k=1:d
for(i in 1:fold){
  test[[i]]=iris[k,]
  train[[i]]=iris[-k,]
  k=d+k
}
#-------------------------------------------------------------