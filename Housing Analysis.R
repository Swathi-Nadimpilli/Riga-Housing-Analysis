install.packages("dplyr")
library(dplyr) 
install.packages("tidyr") 
library(tidyr) 
install.packages("dplyr")
library(dplyr) 
install.packages("ggplot2")
library(ggplot2) 
install.packages("psych") 
library(psych) 

#Reading the csv file

data<-read.csv("riga_re.csv") head(data)
#Summary of data set summary(data) 
library(vtable)
st(data)
#Checking the null values sum(is.na(data))
#Replacing the null values in data set install.packages("imputeTS") library(imputeTS)
data$area<-round(na_mean(data$area,option ="median" )) data$floor<-round(na_mean(data$floor,option ="median" ))
data$total_floors<-round(na_mean(data$total_floors,option="median")) data$price<-round(na_mean(data$price,option="median")) summary(data)


floor1 <-boxplot(data$floor,main="Boxplot for floor",col="red") floor_out<-data$floor
floor_out[which(data$floor %in% floor1$out)]<-mean(data$floor) boxplot(floor_out,main="Boxplot for floor after removing outliers",col="red") boxplot.stats(floor_out)$out


total_floors1<-boxplot(data$total_floors,main="Boxplot for total areas",col="dark green")

total_floors1$out total_floors_out<-data$total_floors
total_floors_out[which(data$total_floors %in% total_floors1$out)] <- mean(data$total_floors)

boxplot(total_floors_out,main="Boxplot for total floors after removing outliers",col="dark green")

boxplot.stats(total_floors_out)$out



total_floors1<-boxplot(data$price,main="Boxplot for price",col="dark green") total_floors1$out
total_floors_out<-data$price
total_floors_out[which(data$price %in% total_floors1$out)] <- median(data$price) boxplot(total_floors_out,main="Boxplot for total floors after removing outliers",col="dark
green") boxplot.stats(total_floors_out)$out


View(data)

t <- table(data$op_type) t
plot <- barplot(t, main = "Plot : Count of Offer Type", ylab = "Counts", ylim = c(0,3000), col=brewer.pal(5,"Set3"),las = 3,  cex.lab = 0.8)

text(x = plot, y = as.numeric(data$op_type), label = as.numeric(t), pos = 3, cex = 0.9, col = "Black")



plot(data$area, data$price, main = "Area Vs Price", xlab = "area", ylab = "Price", col = "darkgreen")

hist(data$floor, main = "Frequency of Floors", col = "orange", xlim = c(0,20), xlab = "Floors")



#Creating Train and Test sets set.seed(11)
TrainIndex <-createDataPartition(data$condition,times=1,p=0.8,list=FALSE) Train_set<-data[TrainIndex,]
Test_set<-data[-TrainIndex,] head(Train_set) dim(Train_set) head(Test_set) dim(Test_set)

#Fitting a logistic regression model to the training set Log_model<-
glm(condition~price+rooms+floor+area,data=Train_set,family=binomial(link="logit"))
Log_model 
summary(Log_model)

#Making predictions on test data
probabilities<-predict(Log_model,newdata = Test_set,type="response") Predicted.Classes.Min<-as.factor(ifelse(probabilities>=0.5,"Yes","No")) Predicted.Classes.Min1<-as.factor(ifelse(Predicted.Classes.Min=="Yes",1,0))
Predicted.Classes.Min1
m<-as.factor(Test_set$condition) length(m)

#Confusion matrix and accuracy for logistic model

confusionMatrix(Predicted.Classes.Min1,m)


#Decision tree
model1<-rpart(condition~price+rooms+floor+area,data=Train_set,method="class",control	= c(maxdepth = 4,cp = 0.005))
rpart.plot(model1)
predict_unseen <-predict(model1,Test_set, type = 'class') tab <- table(Test_set$condition, predict_unseen)
tab
accuracy_Test <- sum(diag(tab)) / sum(tab) print(paste('Accuracy for test', accuracy_Test)) confusionMatrix(predict_unseen,m)

#Random forest model2<-
randomForest(condition~price+rooms+floor+area,data=Train_set,proximity=TRUE,ntree=5000)
model2
pr_m2<-predict(model2,Test_set) Predicted.Classes.Min2<-as.factor(ifelse(pr_m2>=0.5,1,0)) length(pr_m2) confusionMatrix(Predicted.Classes.Min2,m)
plot(model2) importance(model2)
