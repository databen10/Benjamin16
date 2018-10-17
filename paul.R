data<-read.csv("C:/Users/wise/Documents/medical1.csv")
View(data)
library(e1071)
library(caret)
Intrain<-sample(2,nrow(data),p=c(0.6,0.4),replace=TRUE)
Training<-data[Intrain==1,]
Testing<-data[Intrain==2,]
model1<-svm(TYPE~CLINICAL+MANAGEMENT+PROGNOSIS,Training, type="C-classification",kernel="linear",cost=0.021)
pred<-predict(model1,Testing)
cm<-confusionMatrix(table(pred,Testing$TYPE))
cm
