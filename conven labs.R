#######QUADRATIC EQUATION#############
w<-c(a=1,b=5,c=3)
d = b^2-(4*a*c)
e = sqrt(d)
y = -b-e
y1= -b+e


a<-1
b<-5
c<-3
d = b^2-(4*a*c)
e = sqrt(d)
y1 = -b-e
y2= -b+e
x1 = y1/2*a
x2 = y2/2*a
###################IF ELSE STATEMENT#########################
x=5
if(x>5){
  y<-20
} else {y<-0

}
print(y)
#######LOOP STATEMENT##########
for(a in 1:500){
     print (a) 

  }
x<-c("a","b","c","d")
  for(y in 1:4){
    print(x[y])
  }
####MATRIX
x<-matrix(4:9,2,3)
 for(i in seq_len(nrow(x))){
   for (j in seq_len(ncol(x))){
     print(x[i,j])
   }
 }
print(x)
#####WHILE FUNCTION#######
ade<-0
while(ade < 100){
  print(ade)
  ade = ade + 100
}
####Correlation Analysis#######
data<-read.csv("C:/Users/wise/Desktop/Coven.csv")
y<-cor(data$Income,data$Car.price.category, method="kendall")
cor(data$Income,data$Age,method= "kendall")
w<-cor(data,"all.obs")

#####HOME STUDY########
###FOR LOOP
x=3
if(x>5){
  (y>15)++++
    }else {y<-0 
    
} 
print(y)
#####17/07/2018 CONVEN CLASS###########
w<-shapiro.test(data$Age)
G<-log (data$Age, base=exp(1))
View(G)
######WEB SCRAPING########
####READING FILES FROM R BLOGGERS INTO R, MAIN SOURCE https://www.wvgazette.com/
data<-readLines("https://www.r-bloggers.com/wp-content/uploads/2016/01/vent.txt")
df<-data.frame(data)
textdata<-df[df$data,]
text1<-gsub("[[:punct:]]", "",textdata)
text2<-gsub("[[:digit:]]", "",text1)
text3<-gsub("http\\wt", "",text2)
text4<-gsub("[  \t]{2,}", "",text3)
text5<-sapply(text4, try.error)
names(text5)=NULL
try.error=function(x)
 {
  y=NA
  try_error=tryCatch(tolower(x),error=function(e) e)
  if(!inherits(try.error, "error"))
    y=tolower(x)
  return(y)
}
####SENTIMENT ANALYSIS
write.xlsx(text5, "C:/Users/hashbang/Documents/text5.xlsx",colnames=TRUE,row.names=TRUE,sheetName="text5")
write.csv(text5,"text5.csv")
data1<-read.csv("C:Users/wise/Documents/APC.csv")
#################POPULATION RATE###############
data<-read.csv("C:/Users/wise/Documents/nigpo.csv")
train<-creatDataPartition(y=data$Pop.Rate, p=0.65,list=FALSE)
test<-nigpo[-data,]
data(data)
##########
data1<-read.csv("C:/Users/wise/Documents/text5.csv")
data<-read.csv("C:/Users/wise/Documents/medical1.csv")
library(e1071)
Intrain<-sample(2,nrow(data),p=c(0.6,0.4),replace=TRUE)
Training<-data[Intrain==1,]
Testing<-data[Intrain==2,]
model1<-svm(TYPE~CLINICAL+MANAGEMENT+PROGNOSIS+GENDER,Training, type="C-classification",kernel="linear",gamma=0.005,cost=0.025)
pred<-predict(model1,Training)
pred1<-predict(model1,newdata = Testing,type="class")
predict(model1,Testing,type = "prob")
cm<-table(predict(model1), Training$TYPE)
cm<-confusionMatrix(table(pred,Training$TYPE))
cm
View(pred)
points(Training$TYPE, pred, col = "blue", pch=4)
Testing
#######
model1<-svm(TYPE~CLINICAL+MANAGEMENT+PROGNOSIS+GENDER,Testing,kernel="linear")
cm<-table(predict(model1), Testing$TYPE)
cm<-table(predict(model1), Testing$TYPE)
cm
###TUNING

svmtune<-tune(svm,TYPE~CLINICAL+MANAGEMENT+PROGNOSIS+GENDER, data = data, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(seq(0.5,8,.5)))
plot(model1,data=data,TYPE~CLINICAL+MANAGEMENT+PROGNOSIS+GENDER, slice=list(Width=2,Lenght=2))

pkgs <- c('reshape2', 'plyr', 'ggplot2', 'dplyr', 'data.table', 'Lahman')
install.packages(pkgs)
plot(wt, mpg, main="Scatterplot Example",
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19) 
