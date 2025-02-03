# Load the necessary libraries
rm(list = ls()) 
#install.packages('corrplot')
library(class)
library(dplyr)
library(caret)
library(cvms)
library(randomForest)
library(e1071)
library(rpart)
library(caTools)

#import .csv data
library(readxl)
data_pt <- read_excel("data_pt.xlsx")
View(data_pt)   
summary(data_pt)
str(data_pt)

#names of attributes
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

colnames(data) <- c('translater_ID','sex','sent_ID','sent_ct','complexity','ttr',
                    'sent_leng','dept','spm','orin_wd','orin_sec','logic')

#check corplot between num variables
dt<-data[,-c(1,2,3,12)]
M<-cor(dt)
head(round(M,2))
library(corrplot)
corrplot(M, method="circle")
corrplot(M, method = 'number') 


'----------1.Explore data-------------'
#Convert to factors
str(data) 
data$sex <- as.factor(data$sex)
data$logic <- as.factor(data$logic)
str(data) # Good to go!

# Splitting data into train and test data 
data<-data[,-c(1,3)]
set.seed(1)
index <- sample(1:nrow(data), 0.7*nrow(data))
train <- data[index,]
test <- data[-index,]


'----------2.Basic Tree Model----------'

#A base Tree - one tree, apply on our scaled data.
cart_model <- rpart(logic ~., data=data, method = "class")
par(xpd=NA)
plot(cart_model)
text(cart_model, digits =3)

#A base Tree - one tree, but apply on the ORINGAL SCALE data 原始数据.
data_pt <- read_excel("data_pt.xlsx")
data_0 <- subset(data_pt, select = c('译者编号', '译者性别', '译文句编号' ,
                                   '对应原文句数', '词汇复杂性','ttr',        
                                   '平均句长', '平均依存深度', 
                                   '输出速度', '原文长度（词）', '原文长度（秒）', '是否有逻辑增补'))
colnames(data_0) <- c('translater_ID','sex','sent_ID','sent_ct','complexity','ttr',
                    'sent_leng','dept','spm','orin_wd','orin_sec','logic')
data_0$sex <- as.factor(data_0$sex)
data_0$logic <- as.factor(data_0$logic)
data_0<-data_0[,-c(1,3)]
cart_model <- rpart(logic ~., data=data_0, method = "class")
par(xpd=NA)
plot(cart_model)
text(cart_model, digits =3)

'结论1：we can see from the tree plots that sent_ct, orin_sec, orin_wd seems important;
sent_ct, orin sec, orin_wd有阶段性影响的零界数值，比如当orin_sec>=172秒,逻辑增补为true.
rli28@hotmail.com for future need, reasonale price and exellent service. thanks'


'----2.Random forest ----'
library(randomForest)
library(ggplot2)

'------2.1 rank important features------'
#Random forest - many trees
control <- trainControl(method="repeatedcv", number=5, repeats=2)
metric = "Accuracy"
tuneGrid = expand.grid(.mtry = c(1:10))
rf_model<- train(logic~., 
                 data=train, method="rf",
                 metric=metric,tuneGrid=tuneGrid, 
                 trControl=control)
rf_model

v<-varImp(rf_model)

library(ggplot2)
ggplot(data= varImp(rf_model), aes(x=rownames(varImp(rf_model)),y=Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + geom_point(color='skyblue') + xlab(" Importance Score")+
  ggtitle("Variable Importance") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))
plot(varImp(rf_model))

'结论2：we can see from the plots that the most important features are
sent_ct, orin_sec, sex, sent_leng.
note:Variable importance scores are typically scaled between 0 and 100, 
where a higher value signifies a greater contribution of that variable to the models 
predictions.
'


'-----3.Accuracy of model------'
# Predict the test
pred1 <- predict(rf_model, test)
# Get performance
conf_mat1 <- confusion_matrix(targets = pred1,
                              predictions = test$logic, 
                              metrics = list("Accuracy" = TRUE))
##Accuracy
conf_mat1$Accuracy 

'结论3：Random forest模型的预测准确度为0.6151079; logistic回归模型的预测准确度为0.9166667；
我们选择logistic regression model，因为它的准确度以及用于二分因变量的普遍性。
logistic回归模型推论重要的特征为 orin_sec, sex, sent_leng，
rf模型用于和logistic回归模型做对比,结果推论出重要的特征为sent_ct, orin_sec, sex, sent_leng.
1.这依旧包含上述三个重要自变量(orin_sec, sex, sent_leng),并且决策树展示了不同区间的数值对y的影响。'


