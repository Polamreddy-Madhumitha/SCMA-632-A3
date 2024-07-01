
#PART A: Logistic regression analysis the dataset. Validate assumptions, evaluate with a confusion matrix and ROC curve, and interpret the results. 

heart <- read.csv("C:\\Users\\aravi\\Downloads\\heart.csv")

library(dplyr)
library(ggplot2)
library(DataExplorer)

heart
plot_missing(heart)
summary(heart)
sum(is.na(heart))
names(heart)
str(heart)

# Replace missing values with the mean for numeric columns
heart <- heart %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#Checking missing values after filling it with mean values of the column 
missing_info <- colSums(is.na(heart))
cat("Missing Values Information:\n")
print(missing_info)


#performing logistic regression , validate assumptions , and evaluating the performance with a confusion matrix
#and ROC curve and interpreting the results 
names(heart)

cor_matrix<-cor(heart)
print(cor_matrix)
heatmap(cor_matrix)
boxplot(cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal ~ target,data=heart)

library("caTools")
library("MLmetrics")

library(dplyr)
library(caTools)
library(pROC)
library(rpart)
library(rpart.plot)
library(MLmetrics)

# Logistic Regression
set.seed(123)
split<-sample.split(heart$target,SplitRatio = 0.7)
train<-subset(heart,split==TRUE)
test<-subset(heart,split==FALSE)
model<-glm(target~., data = train , family = binomial)
pred_prob<-predict(model,newdata=test,type="response")
pred_class<-ifelse(pred_prob >= 0.5,1,0)

# Confusion Matrix 
library(MLmetrics)
confusion <- ConfusionMatrix(factor(pred_class),factor(test$target))
print(confusion)
roc_obj<-roc(test$target,pred_prob)
auc<-auc(roc_obj)
print(paste("AUC-ROC:",auc))
plot(roc_obj,main="ROC Curve",print.auc=TRUE)

#decision tree analysis for the data in part A and compare the results of the 
#Logistic regression and Decision tree
library(stats)
#install.packages("rpart")
library(rpart)
#install.packages("caTools")
library(caTools)

set.seed(123)
split<-sample.split(heart$target,SplitRatio = 0.7)
train<-subset(heart,split == TRUE)
test<-subset(heart,split == FALSE)
model<-rpart(target~.,data=train,method="class")
pred_prob<-predict(model,newdata=test,type="prob")
pred_class<-ifelse(pred_prob[,2]>=0.5,1,0)

confusion <- ConfusionMatrix(factor(pred_class),factor(test$target))
print(confusion)
roc_obj<-roc(test$target,pred_prob[,2])
auc<-auc(roc_obj)
print(paste("AUC-ROC:",auc))
plot(roc_obj,main="ROC Curve",print.auc=TRUE)

