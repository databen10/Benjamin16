#######NEURAL NETWORK IN R FOR STOCK EXCHANGE PREDICTION################
library(neuralnet)
library(caret)
Access<-read.csv("C:/Users/wise/Documents/access.csv")
Intrain<-sample(2,nrow(Firstbank),p=c(0.6,0.4), replace=TRUE)
View(Firstbank)
####Normlize the dataset
normalize<-function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}
maxmif<-as.data.frame(lapply(Access, normalize))
#################Training and Testing datasets
Trainset<-maxmif[1:237,]
Testset<-maxmif[238:395,]
#####NEURAL NETWORK MODELLING########
allvars<-colnames(Access)
predictorvars<-allvars[!allvars%in%"Close"]
predictorvars<-paste(predictorvars,collapse="+")
form<-as.formula(paste("Close~",predictorvars,collapse="+"))
nnmodel<-neuralnet(formula=form,hidden=c(4,2),linear.output=T,data=Trainset)
nnmodel$result.matrix
plot(nnmodel)
#####Testing the model on the test dataset
Test_set<-subset(Testset, select = c("Low","High","Open","Last","TotalTradeQuantity","Turnover"))
nntest<-compute(nnmodel,Test_set)
head(Test_set)
#####ACCURACY#######
result<-data.frame(actual = Testset$Close, prediction = nntest$net.result)
table(actual,prediction)
roundresult<-sapply(result.round.digits=0)
roundf<-data.frame(roundresult)