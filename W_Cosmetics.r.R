
#-----------------------------Finding association rules---------------------------------

mydata<-read.csv("C:\\Users\\Abhinav Raj\\Downloads\\Cosmetics.csv",header=T)
summary(mydata)
library(arules)
rules<-apriori(mydata)
summary(rules)
#----------------------------------------------------------------------------------------
rules<-apriori(mydata,parameter=list(minlen=2,maxlen=3,supp=.7))
inspect(rules)
#----------------------------------------------------------------------------------------
rules<-apriori(mydata,parameter=list(minlen=2,maxlen=3,conf=.7),
               appearance=list(rhs=c("Foundation=Yes"),default="lhs"))
inspect(rules)
#-------------------------graphs & charts------------------------------------------------
library(arulesViz)
plot(rules)
plot(rules,method="grouped")
#-------------------finding intresting rule2---------------------------------------------
rules<-apriori(mydata,parameter=list(minlen=2,maxlen=5,supp=.1,conf=.5),
               appearance=list(rhs=c("Foundation=Yes"),lhs=c("Bag=Yes","Blush=Yes",
                "Nail.Polish=Yes","Brushes=Yes","Concealer=Yes","Eyebrow.Pencils=Yes",
                "Bronzer=Yes","Lip.liner=Yes","Mascara=Yes","Eye.shadow=Yes",
                "Lip.Gloss=Yes","Lipstick=Yes","Eyeliner=Yes"),default="none"))
inspect(rules)
