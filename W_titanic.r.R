#---------------------------Titanic---------------------------------------------------------
my<-read.csv("C:\\Users\\Abhinav Raj\\Downloads\\titanic.csv",header=T)
library(arules)
rules11<-apriori(my)
inspect(rules11)
#-------------------------------------------------------------------------------------------
rules22<-apriori(my,parameter=list(minlen=2,supp=0.005,conf=0.8),
                 appearance = list(rhs=c("Survived=No","Survived=Yes"),default="lhs"))
inspect(rules22)
#-------------------------------------------------------------------------------------------
rules33<-apriori(my,parameter=list(minlen=2,supp=0.005,conf=0.8),
                 appearance = list(rhs=c("Survived=Yes"),default="lhs"))
inspect(rules33)

