#test_LPIII <- read.csv("D:/Hack/Loan_Prediction 3/test_LPIII.csv")
#train_LPIII <- read.csv("D:/Hack/Loan_Prediction 3/train_LPIII.csv")

test_LPIII <- read.csv("/media/jibesh/5AD20696D2067693/Hack/Loan_Prediction 3/test_LPIII.csv")
train_LPIII <- read.csv("/media/jibesh/5AD20696D2067693/Hack/Loan_Prediction 3/train_LPIII.csv")


#load dataset
test=test_LPIII
train=train_LPIII
#exploratory analysis
str(train)
summary(train)

str(test)
summary(test)

#checking missing value
table(is.na(test))
table(is.na(train))

colSums(is.na(train))
colSums(is.na(test))

#replacing space into NA
train[train==""] = NA
test[test==""] = NA

summary(train)
summary(test)

#Replacing NA value to median mode
train[,c(2)=="NA"]= "Male"

#chnage into factor
train$Credit_History=as.factor(train$Credit_History)

#missing value for categorical value
library(mlr)
imputed_data=impute(train, classes = list(factor=imputeMode()))
train=imputed_data$data
is.na(train$Gender)

summary(train)# checking missing value
str(train)

#Imput loanAmount and Loan amount term
train[is.na(train$LoanAmount),9]=median(train$LoanAmount, na.rm = T)
train[is.na(train$Loan_Amount_Term),10]=median(train$Loan_Amount_Term,na.rm = T)


#Now missing value treatment with Test data
is.na(test)
table(is.na(test))
summary(test)
str(test)

#change crdit_History into factor
test$Credit_History=as.factor(test$Credit_History)

#imputing for discrete value
library(mlr)
imputed_data_1=impute(test, classes = list(factor=imputeMode()))
test=imputed_data_1$data

#imputing continuous value
test[is.na(test$LoanAmount),9]=median(test$LoanAmount,na.rm = T)
test[is.na(test$Loan_Amount_Term),10]=median(test$Loan_Amount_Term, na.rm = T)

#now I get clean data now i could put any predictive model

table(is.na(test))
table(is.na(train))

#Add loan status in Test
#test$Loan_Status=test$

#cor(train,use = "everything",method = c("pearson"))

train=train[,c(-1)]
#The variable importance plot is obtained by growing some trees,
require(randomForest)
fit=randomForest(factor(Loan_Status)~., data=train)
#Then we can use simple functions

VI_F=importance(fit) 
VI_F


#Model Applying
library(caret)
cntrl_1=trainControl(method = "cv",number=10)
model7 = train(Loan_Status~Credit_History+LoanAmount+ApplicantIncome+CoapplicantIncome+Dependents, data = train,
               method="C5.0", trControl = cntrl_1, metric = "Kappa")
pred_model7=predict(model7,newdata=test)

submission = data.frame("Loan_ID" = test$Loan_ID,"Loan_Status" = pred_model7)

# Write CSV in R
write.csv(submission, file = "submission.csv")

prop.table(table(train$Education,train$Loan_Status))
