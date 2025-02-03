# Load the necessary libraries
rm(list = ls()) 
#install.packages('corrplot')
library(class)
library(dplyr)
library(caret)
library(cvms)
library(e1071)
library(rpart)
library(caTools)
library(readr)
library(tidyverse)
library(MASS)

#import .csv data
library(readxl)
data_pt <- read_excel("data_pt.xlsx")
View(data_pt)   
summary(data_pt)
str(data_pt)

#names of attributes; get the data we need
attributes <- names(data_pt)
print(attributes)
data <- subset(data_pt, select = c('译者编号', '译者性别', '译文句编号' ,
                                   
                                   '对应原文句数', '词汇复杂性','ttr',        
                                   '平均句长', '平均依存深度', 
                                   '输出速度', '原文长度（词）', '原文长度（秒）',
                                   
                                   '是否有逻辑增补'))
#check missing value
missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)
summary(data)
str(data)
head(data)


######data preprocessing######
#Scale the numerical data
data$对应原文句数 <- as.numeric(scale(data$对应原文句数))
data$词汇复杂性 <- as.numeric(scale(data$词汇复杂性))
data$ttr <- as.numeric(scale(data$ttr))

data$平均句长<-as.numeric(scale(data$平均句长))
data$平均依存深度<-as.numeric(scale(data$平均依存深度))
data$输出速度<-as.numeric(scale(data$输出速度))
data$`原文长度（词）`<-as.numeric(scale(data$`原文长度（词）`))
data$`原文长度（秒）`<-as.numeric(scale(data$`原文长度（秒）`))

#rename the columns in English
colnames(data) <- c('translater_ID','sex','sent_ID','sent_ct','complexity','ttr',
                    'sent_leng','dept','spm','orin_wd','orin_sec','logic')

#check corplot between numerical variables
dt<-data[,-c(1,2,3,12)]
M<-cor(dt)
head(round(M,2))
library(corrplot)
corrplot(M, method="circle")
corrplot(M, method = 'number') 

'结论1：Strong correlation between: dept-complexity; ttr-spm; dept-sent_leng;
these variables need to be fixed if they are strongly significant in p-value in the stepwise logit model'


'----------1.Explore data-------------'
#Convert to factors
str(data) 
data$sex <- as.factor(data$sex)
data$logic <- as.factor(data$logic)
str(data) # Good to go!

counts<-table(data$logic)
barplot(counts, main="Distribution of logic addiction",xlab="Freq")


'----------2.Logistic models: null model/ full model/ stepwised model for feature selection--------'
#Split data
#Get a 70% training data and 20% test data
data<-data[,-c(1,3)]
set.seed(1)
index <- sample(1:nrow(data), 0.7*nrow(data))
train <- data[index,]
test <- data[-index,]

Logistic_regression_full <- glm(logic ~ ., data = train, family = binomial)
Logistic_regression_null <- glm(logic ~ 0, data = train, family = binomial)
summary(Logistic_regression_full)
summary(Logistic_regression_null)
anova(Logistic_regression_full, Logistic_regression_null, test ="Chisq") 
#model_full is sig better than model null!!
'结论2：negatively related to y: orin_wd, sent_leng, ttr, comlexity; 
other variables are positively related to y (y和所有因素的正负相关性)'

#Perform stepwise logistic regression, for feature selection
#perform forward and backward stepwise regression
bothstepwise <- step(Logistic_regression_null, direction='both', scope=formula(Logistic_regression_full), trace=0)
bothstepwise$coefficients
'(rli28@hotmail for future cooperation if you and your coworkers need)
结论3: stepwise helps to find and keep important features in the model,
selected features are : sex, orin_sec, sent_leng. from output.
结论4: from the coefficients we can interpret that:
sexF: Being female is associated with increased odds of lo being True compared to being male.
orin_sec: Higher values of orin_sec are associated with decreased odds of lo being True.
sent_leng: Higher values of sent_leng are also associated with decreased odds of lo being True.

'

anova(Logistic_regression_full, bothstepwise, test ="Chisq") #sig p-vale
BIC(Logistic_regression_full)
BIC(Logistic_regression_null)
BIC(bothstepwise) #best logit model because lowest BIC

'----------3.Check model performance--------'
#Get predictions for all observations 
predictions_model1 <- predict(bothstepwise, type = "response", newdata= test)

# Fit criteria ------------------------------------------------------------
test$logic<- ifelse(test$logic == "T", 1, 0)
# Predict the test
pred1 <- predict(bothstepwise, test)
# Get performance
conf_mat1 <- confusion_matrix(targets = pred1,
                              predictions = test$logic, 
                              metrics = list("Accuracy" = TRUE))
##Accuracy=0.9166667
conf_mat1$Accuracy 
'结论5: 最优logistic回归模型的预测准确度=0.9166667‘
