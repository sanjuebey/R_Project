
# To check the directory and to read the file

setwd("C:/sje_project")
employee<-read.csv("employee.csv")
str(employee)
summary(employee)

# Let's check the correlation values of the attributes

employee_cor <- employee[,-c(6:10)]
cr <- cor(employee_cor)
library(corrplot)
corrplot(cr, type='lower')
cor(employee_cor)

# Let's visualize the datasets for the employees who left the company

employee_left <- employee[employee$left==1,]

library("ggplot2")
qplot(employee_left$satisfaction_level)
qplot(employee_left$last_evaluation)
qplot(employee_left$number_project)
qplot(employee_left$average_montly_hours)
qplot(employee_left$department)
qplot(employee_left$salary)

# Let's visualize the relationship between attributes for the employees who left the organization

qplot(employee_left$satisfaction_level,employee_left$last_evaluation)
qplot(employee_left$number_project,employee_left$average_montly_hours)
qplot(employee_left$satisfaction_level,employee_left$average_montly_hours)

# Let's visualize the datasets for the employees who didn't leave the company.

employee_stay <- employee[employee$left==0,]

library("ggplot2")
qplot(employee_stay$satisfaction_level)
qplot(employee_stay$last_evaluation)
qplot(employee_stay$number_project)
qplot(employee_stay$average_montly_hours)
qplot(employee_stay$department)
qplot(employee_stay$salary)

# Let's visualize the relationship between attributes for the employees who didn't leave the company

qplot(employee_stay$satisfaction_level,employee_stay$last_evaluation)
qplot(employee_stay$number_project,employee_stay$average_montly_hours)
qplot(employee_stay$satisfaction_level,employee_stay$average_montly_hours)

# Let's visualize the datasets for overall employees in the company.

library("ggplot2")
qplot(employee$satisfaction_level)
qplot(employee$last_evaluation)
qplot(employee$number_project)
qplot(employee$average_montly_hours)
qplot(employee$department)
qplot(employee$salary)

# Let's visualize the relationship between attributes for overall employees

qplot(employee$satisfaction_level,employee$last_evaluation)
qplot(employee$number_project,employee$average_montly_hours)
qplot(employee$satisfaction_level,employee$average_montly_hours)

# Let's evaluate the department wise turnouts

employee_left$department <- as.factor(employee_left$department)
qplot(employee_left$department,ylim=c(0,4000))

employee_stay$department <- as.factor(employee_stay$department)
qplot(employee_stay$department, ylim=c(0,4000))

# Let's evaluate percentage of employees leaving from each department

a <- table(employee$department)
b <- table(employee_left$department)
c <- as.data.frame(cbind(a,b))
colnames(c) <- c("Total_employees","Left_employees")
c$left_percentage <- (c$Left_employees/c$Total_employees)*100
c

# Let's start building the first model - Decision tree for this dataset and to check the accuracy

employee$Work_accident <- as.factor(employee$Work_accident)
employee$left <- as.factor(employee$left)
employee$promotion_last_5years <- as.factor(employee$promotion_last_5years)
employee$department <- as.factor(employee$department)
employee$salary <- as.factor(employee$salary)
str(employee)

library(rpart)
#Build a training and testing set
set.seed(4)
id<-sample(2,nrow(employee),prob = c(0.7,0.3),replace = T)
trainset<-employee[id==1,]
testset<-employee[id==2,]

model<-rpart(trainset$left~. ,data = trainset)
model$variable.importance
plot(model, margin=0.1)
text(model, use.n = TRUE,pretty = TRUE, cex=0.8)

predvalues<-predict(model,newdata = testset,type = "class")
library(caret)
library(e1071)
confusionMatrix(table(predvalues, testset$left))

# Let's start building the second model - Random Forest for this dataset and check the accuracy

library(randomForest)
model<-randomForest(trainset$left~. ,data = trainset, ntree=500)
model

predvalues<-predict(model,newdata = testset,type = "class")
confusionMatrix(table(predvalues, testset$left))

varImp(model)
varImpPlot(model,sort=TRUE, type=2)

# Let's start building the third model - Naive Bayes for this dataset and check the accuracy

library(e1071)
model<-naiveBayes(trainset$left~. ,data = trainset)
model

predvalues<-predict(model,newdata = testset,type = "class")
confusionMatrix(table(predvalues, testset$left))

# Let's start building the fourth model - SVM for this dataset with linear hyperplane and check the accuracy

model<-svm(trainset$left~. ,data = trainset, kernel = "linear", cost = 1.0)
summary(model)

predvalues<-predict(model,newdata = testset,type = "class")
confusionMatrix(table(predvalues, testset$left))

# Let's start building the fourth model - SVM for this dataset with polynomial hyperplane and check the accuracy

model<-svm(trainset$left~. ,data = trainset, kernel = "polynomial", cost = 1.0)
summary(model)

predvalues<-predict(model,newdata = testset,type = "class")
confusionMatrix(table(predvalues, testset$left))


