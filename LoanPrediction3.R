loanData<-read.csv("train_u6lujuX_CVtuZ9i.csv",header = TRUE)

loanData$Gender<-ifelse(loanData$Gender=="",NA,loanData$Gender)
loanData$Gender<-ifelse(loanData$Gender==2,"Female",ifelse(loanData$Gender==3,"Male",NA))
loanData$Gender<-as.factor(loanData$Gender)

loanData$Self_Employed<-ifelse(loanData$Self_Employed=="",NA,loanData$Self_Employed)
loanData$Self_Employed<-ifelse(loanData$Self_Employed==2,"No",ifelse(loanData$Self_Employed==3,"Yes",NA))
loanData$Self_Employed<-as.factor(loanData$Self_Employed)

loanData$Dependents<-ifelse(loanData$Dependents=="",NA,loanData$Dependents)
loanData$Dependents<-as.factor(loanData$Dependents)

loanData$Married<-ifelse(loanData$Married=="",NA,loanData$Married)
loanData$Married<-ifelse(loanData$Married==2,"No",ifelse(loanData$Married==3,"Yes",NA))
loanData$Married<-as.factor(loanData$Married)

loanData1<-loanData[-13]
loanData1<-loanData1[-1]

library(C50)
model<-C5.0(loanData1,loanData$Loan_Status)
test<-read.csv("test_Y3wMUE5_7gLdaTN.csv")
str(test$Gender)

test$Gender<-ifelse(test$Gender=="",NA,test$Gender)
test$Gender<-ifelse(test$Gender==2,"Female",ifelse(test$Gender==3,"Male",NA))
test$Gender<-as.factor(test$Gender)

test$Dependents<-ifelse(test$Dependents=="",NA,test$Dependents)
test$Dependents<-as.factor(test$Dependents)

test$Self_Employed<-ifelse(test$Self_Employed=="",NA,test$Self_Employed)
test$Self_Employed<-ifelse(test$Self_Employed==2,"No",ifelse(test$Self_Employed==3,"Yes",NA))
test$Self_Employed<-as.factor(test$Self_Employed)

pred<-predict(model,test[-1])

submisson<-cbind(test[1],pred)
names(submisson)<-c("Loan_ID","Loan_Status")
write.csv(submisson,file = "submission.csv",sep=",",row.names=FALSE)
