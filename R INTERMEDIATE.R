ella<-data(mtcars)
View(mtcars)
hist(mtcars)
hist(mtcars$mpg)
hist(mtcars$cyl)
head(mtcars)
tail(mtcars)
piechart(mtcars)
pie(mtcars$mpg)
pie(mtcars$mpg,edges = 300,radius=0.9, angle=90)
pie(mtcars$mpg,edges = 300,radius = 1.5,angle = 90)
is.na()
as.factor(mpg)
mtrand<-data(mtcars)
write.csv(mtrend, "mpg.csv")
mtcars
mtrend <- mtcars
view(mtrend)
write.csv(mtrend,"mpg.csv")
#to install a package use install.packages("e1071")
library(corrplot)
Fav<-cor(mtcars)
corrplot(Fav,method="number")

#linear regression model day2 (new topic)
View(mtrend)

mymodel1<-lm(mpg~hp+cyl,data=mtrend)
summary(mymodel1)
summary(mymodel)
mymodel<-lm(mpg~disp+hp+qsec+am, data=mtrend)
summary(mymodel)

#Day 3 logisticical regression model

Diabetes<-read.csv("C:/Users/pc/Desktop/New folder (4)/DATA diabetes.csv")
View(Diabetes)
str(Diabetes)
#Lets find the influence of all the variables on a patients diabetic status
#glm(means generalised linear model) is the fuction for finding a yes or no model
mymodel<-glm(Outcome~.,data=Diabetes,family=binomial)
summary(mymodel)
#Remove the variables without asteriks and check outcome
mymodel<-glm(Outcome~.-Glucose,data=Diabetes, family=binomial)
summary(mymodel)
mymodel<-glm(Outcome~.-SkinThickness,data=Diabetes, family=binomial)
summary(mymodel)
mymodel1<-glm(Outcome~.-Age,data=Diabetes, family=binomial)
summary(mymodel1)

#Lets train our model to predict
set.seed(3241)
valid<-sample(2,nrow(Diabetes),p=c(0.75,0.25),replace =TRUE)
Ellamodel<-glm(Outcome~.,data=Diabetes,family=binomial)
Training<-Diabetes[valid==1,]
Testing<-Diabetes[valid==2,]
nrow(Diabetes)
nrow(Training)
nrow(Testing)
res<-predict(Ellamodel,newdata=Testing)
Tab<-table(Testing$Outcome, predicted=res>0.5)
Tab
View(Testing)
res<-predict(Ellamodel,newdata=Testing, type = "response")
res
