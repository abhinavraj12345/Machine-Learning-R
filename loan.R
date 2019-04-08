library(randomForest)
data=read.csv("C:\\Users\\Abhinav Raj\\Downloads\\Data sets\\loan prediction.csv",
              header = T,na.string=c(""))
Da=na.roughfix(data)
colnames(Da)[2]=c("Gender_1");Da$Gender_2= Da$Gender_1
colnames(Da)[3]=c("Married_1");Da$Married_2= Da$Married_1
colnames(Da)[5]=c("Education_1"); Da$Education_2=Da$Education_1
colnames(Da)[6]=c("Self_Employed_1");Da$Self_Employed_2=Da$Self_Employed_1
colnames(Da)[12]=c("Property_Area_1");Da$Property_Area_2=Da$Property_Area_3=Da$Property_Area_1
Da$Dependents=as.numeric(Da$Dependents)-1
fdata=data.frame(Da$Loan_ID,Da$Gender_1,Da$Gender_2,Da$Married_1,Da$Married_2,Da$Dependents,
                 Da$Education_1,Da$Education_2,Da$Self_Employed_1,Da$Self_Employed_2,
                 Da$ApplicantIncome,Da$CoapplicantIncome,Da$LoanAmount,Da$Loan_Amount_Term,
                 Da$Credit_History,Da$Property_Area_1,Da$Property_Area_2,Da$Property_Area_3,
                 Da$Loan_Status)
fdata$Da.Gender_1=as.factor(as.numeric(fdata$Da.Gender_1=="Male"))
fdata$Da.Gender_2=as.factor(as.numeric(fdata$Da.Gender_2=="Female"))
fdata$Da.Married_1=as.factor(as.numeric(fdata$Da.Married_1=="Yes"))
fdata$Da.Married_2=as.factor(as.numeric(fdata$Da.Married_2=="No"))
fdata$Da.Education_1=as.factor(as.numeric(fdata$Da.Education_1=="Graduate"))
fdata$Da.Education_2=as.factor(as.numeric(fdata$Da.Education_2=="Not Graduate"))
fdata$Da.Self_Employed_1=as.factor(as.numeric(fdata$Da.Self_Employed_1=="Yes"))
fdata$Da.Self_Employed_2=as.factor(as.numeric(fdata$Da.Self_Employed_2=="No"))
fdata$Da.Property_Area_1=as.factor(as.numeric(fdata$Da.Property_Area_1=="Rural"))
fdata$Da.Property_Area_2=as.factor(as.numeric(fdata$Da.Property_Area_2=="Semiurban"))
fdata$Da.Property_Area_3=as.factor(as.numeric(fdata$Da.Property_Area_3=="Urban"))
fdata$Da.ApplicantIncome=as.numeric(fdata$Da.ApplicantIncome)
colnames(fdata)[ncol(fdata)]=c("Loan_Status")
#--------------------------------training data = fdata ------------------------------------------
test_data=read.csv("C:\\Users\\Abhinav Raj\\Downloads\\Data sets\\loan test.csv",
                   header = T,na.string=c(""))
Ta=na.roughfix(test_data)
colnames(Ta)[2]=c("Gender_1");Ta$Gender_2= Ta$Gender_1
colnames(Ta)[3]=c("Married_1");Ta$Married_2= Ta$Married_1
colnames(Ta)[5]=c("Education_1"); Ta$Education_2=Ta$Education_1
colnames(Ta)[6]=c("Self_Employed_1");Ta$Self_Employed_2=Ta$Self_Employed_1
colnames(Ta)[12]=c("Property_Area_1");Ta$Property_Area_2=Ta$Property_Area_3=Ta$Property_Area_1
Ta$Dependents=as.numeric(Ta$Dependents)-1
tdata=data.frame(Ta$Loan_ID,Ta$Gender_1,Ta$Gender_2,Ta$Married_1,Ta$Married_2,Ta$Dependents,
                 Ta$Education_1,Ta$Education_2,Ta$Self_Employed_1,Ta$Self_Employed_2,
                 Ta$ApplicantIncome,Ta$CoapplicantIncome,Ta$LoanAmount,Ta$Loan_Amount_Term,
                 Ta$Credit_History,Ta$Property_Area_1,Ta$Property_Area_2,Ta$Property_Area_3)
tdata$Ta.Gender_1=as.factor(as.numeric(tdata$Ta.Gender_1=="Male"))
tdata$Ta.Gender_2=as.factor(as.numeric(tdata$Ta.Gender_2=="Female"))
tdata$Ta.Married_1=as.factor(as.numeric(tdata$Ta.Married_1=="Yes"))
tdata$Ta.Married_2=as.factor(as.numeric(tdata$Ta.Married_2=="No"))
tdata$Ta.Education_1=as.factor(as.numeric(tdata$Ta.Education_1=="Graduate"))
tdata$Ta.Education_2=as.factor(as.numeric(tdata$Ta.Education_2=="Not Graduate"))
tdata$Ta.Self_Employed_1=as.factor(as.numeric(tdata$Ta.Self_Employed_1=="Yes"))
tdata$Ta.Self_Employed_2=as.factor(as.numeric(tdata$Ta.Self_Employed_2=="No"))
tdata$Ta.Property_Area_1=as.factor(as.numeric(tdata$Ta.Property_Area_1=="Rural"))
tdata$Ta.Property_Area_2=as.factor(as.numeric(tdata$Ta.Property_Area_2=="Semiurban"))
tdata$Ta.Property_Area_3=as.factor(as.numeric(tdata$Ta.Property_Area_3=="Urban"))
tdata$Ta.ApplicantIncome=as.numeric(tdata$Ta.ApplicantIncome)
colnames(tdata)=colnames(fdata)[-length(colnames(fdata))]
#-------------------------------test data = tdata ---------------------------------------------
K.rf=randomForest(formula=Loan_Status~.,data=fdata[,-1],ntree=250,mtry=15)
prediction=data.frame(tdata$Da.Loan_ID,predict(K.rf,tdata[,-1]))
colnames(prediction)=c("Loan_ID","Loan_Status")
prediction$Loan_ID=as.factor(prediction$Loan_ID)
#prediction$Loan_Status=as.integer(prediction$Loan_Status)
write.csv(prediction, file = "loan.csv",row.names=FALSE)







