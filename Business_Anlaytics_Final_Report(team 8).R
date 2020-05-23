setwd("C:/Desktop/ba")
getwd()

# Load data
bankdata= read.csv("bank-additional-full.csv",sep=";")

# Quickly preview data structure with HEAD(), STR(), and SUMMARY().
str(bankdata)
head(bankdata,10)
summary(bankdata)

library(ggplot2)
library(tidyverse)
library(gmodels)
library(ggmosaic)
library(corrplot)


# Checking missing values
colSums(is.na(bank))
colSums(bank == "")
colSums(bank == "unknown")

bank %>% 
  summarise_all(list(~sum(. == "unknown"))) %>% 
  gather(key = "variable", value = "num_unknown") %>% 
  arrange(-num_unknown)



#Exploratory Data Analysis

## 1-1. age
summary(bankdata$age)

#### distribution with barplot

bankdata%>%
ggplot()+
aes(age)+
geom_bar(aes(fill=y))+
 scale_x_continuous(breaks = seq(0, 100, 5))

bankdata%>%
ggplot()+
aes(age)+
geom_bar(aes(fill=y),position='fill')+
scale_x_continuous(breaks = seq(0, 100, 5))
  

### distribution with Probability of Conversion
par(mar = c(5,5,4,4))    
hist(bankdata$age,main="Distribution of Customers by Age",xlab="Age in Years",col="turquoise3")
par(new=T)
plot(prop.table(table(bankdata$age,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F, col="orangered", pch = 16)
par(new=T)
mtext("Probability of Conversion", side=4, line=3)
axis(4, ylim=c(0,1), col="black",col.axis="black",las=1)

### chiq.test
chisq.test(bankdata$age, bankdata$y)



##1-2. job

bankdata%>%
ggplot()+
aes(job)+
geom_bar(aes(fill=y))+
coord_flip()

bankdata%>%
ggplot()+
aes(job)+
geom_bar(aes(fill=y),position='fill')+
coord_flip()


par(mar = c(7,5,4,4))
plot(bankdata$job, main="Distribution of Customers by Job", ylab = "Frequency", las = 2, col = "turquoise3", xlim = c(0,15))
par(new=T)
plot(prop.table(table(bankdata$job,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F, col = "orangered3", pch = 16, xlim = c(0.5,13))
mtext("Probability of Conversion",side=4, line = 3)
axis(4, ylim=c(0,1), col="black",col.axis="black",las=1)


### chiq.test
chisq.test(bankdata$job, bankdata$y)



##1-3. marital
  
bankdata%>%
ggplot()+
aes(marital)+
geom_bar(aes(fill=y))


bankdata%>%
ggplot()+
aes(marital)+
geom_bar(aes(fill=y),position='fill')


par(mar = c(5,5,5,5))
plot(bankdata$marital, main="Distribution of Customers by marital", ylab = "Frequency", las = 2, col = "turquoise3", xlim = c(0,5))
par(new=T)
plot(prop.table(table(bankdata$marital,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F, col = "orangered3", pch = 16, xlim = c(0.5,4.5))
mtext("Probability of Conversion",side=4, line = 3)
axis(4, ylim=c(0,1), col="black",col.axis="black",las=1)


## chiq.test
chisq.test(bankdata$marital, bankdata$y)


#1-4. education
  
bankdata%>%
ggplot()+
aes(education)+
geom_bar(aes(fill=y))

bankdata%>%
ggplot()+
aes(education)+
geom_bar(aes(fill=y),position='fill')

par(mar = c(5,5,5,5))
plot(bankdata$education, main="Distribution of Customers by education", ylab = "Frequency", las = 2, col = "turquoise3", xlim = c(0,10))
par(new=T)
plot(prop.table(table(bankdata$education,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F, col = "orangered3", pch = 16, xlim = c(0.38,8.6))
mtext("Probability of Conversion",side=4, line = 3)
axis(4, ylim=c(0,1), col="black",col.axis="black",las=1)

## chiq.test
chisq.test(bankdata$education, bankdata$y)



#1-5. default

bankdata%>%
ggplot()+
aes(default)+
geom_bar(aes(fill=y))


str(bankdata)

#1-6. housing
bankdata%>%
ggplot()+
aes(housing)+
geom_bar(aes(fill=y))

  
bankdata%>%
ggplot()+
aes(housing)+
geom_bar(aes(fill=y),position='fill')

## chiq.test
chisq.test(bankdata$housing, bankdata$y)


#1-7. loan
bankdata%>%
ggplot()+
aes(loan)+
geom_bar(aes(fill=y))

  
bankdata%>%
ggplot()+
aes(loan)+
geom_bar(aes(fill=y),position='fill')

## chiq.test
chisq.test(bankdata$loan, bankdata$y)



#1-8. contact
bankdata%>%
ggplot()+
aes(contact)+
geom_bar(aes(fill=y))

  
bankdata%>%
ggplot()+
aes(contact)+
geom_bar(aes(fill=y),position='fill')


par(mar = c(5,5,5,5))
plot(bankdata$contact, main="Distribution of Customers by contact", ylab = "Frequency", las = 2, col = "turquoise3", xlim = c(0,2.5))
par(new=T)
plot(prop.table(table(bankdata$contact,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F, col = "orangered3", pch = 16, xlim = c(0.38,2.5))
mtext("Probability of Conversion",side=4, line = 3)
axis(4, ylim=c(0,1), col="black",col.axis="black",las=1)

## chiq.test
chisq.test(bankdata$contact, bankdata$y)




#1-9. month
bankdata%>%
ggplot()+
aes(month)+
geom_bar(aes(fill=y))

  
bankdata%>%
ggplot()+
aes(month)+
geom_bar(aes(fill=y),position='fill')

par(mar = c(5,5,5,5))
plot(bankdata$month, main="Distribution of Customers by month", ylab = "Frequency", las = 2, col = "turquoise3", xlim = c(0,12))
par(new=T)
plot(prop.table(table(bankdata$month,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F, col = "orangered3", pch = 16, xlim = c(0.63,10.3))
mtext("Probability of Conversion",side=4, line = 3)
axis(4, ylim=c(0,1), col="black",col.axis="black",las=1)


## chiq.test
chisq.test(bankdata$month, bankdata$y)



#1-10. day_of_week 
bankdata%>%
ggplot()+
aes(day_of_week )+
geom_bar(aes(fill=y))

  
bankdata%>%
ggplot()+
aes(day_of_week )+
geom_bar(aes(fill=y),position='fill')

par(mar = c(5,5,5,5))
plot(bankdata$day_of_week, main="Distribution of Customers by day_of_week", ylab = "Frequency", las = 2, col = "turquoise3", xlim = c(0,6))
par(new=T)
plot(prop.table(table(bankdata$day_of_week,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F, col = "orangered3", pch = 16, xlim = c(0.3,5.5))
mtext("Probability of Conversion",side=4, line = 3)
axis(4, ylim=c(0,1), col="black",col.axis="black",las=1)

## chiq.test
chisq.test(bankdata$day_of_week, bankdata$y)



# 1-11. duration
#Since the goal is to seek best candidates who will have the best odds to subscribe to a term deposit, the call duration can¡¯t be known before. So we recommend this feature be removed.
bankdata%>%
ggplot()+
aes(duration )+
geom_bar(aes(fill=y))


# 1-12. campaign
bankdata %>% 
  ggplot() +
  aes(campaign) +
  geom_bar(aes(fill=y)) +
   scale_x_continuous(breaks = seq(0, 50, 5))
#Calling the same person more than ten times during a single marketing campaign seems excessive. We¡¯ll consider those as outliers, even if marketing harrassment a real thing. However, we can see that on the chart above that harassment doesn¡¯t work at all.

bankdata <- bankdata %>%   
    filter(campaign <= 10) 

bankdata %>% 
  ggplot() +
  aes(campaign) +
  geom_bar(aes(fill=y)) +
   scale_x_continuous(breaks = seq(0, 10, 1))


par(mar = c(5,5,5,5))
barplot(table(bankdata$campaign), main="Distribution of Customers by campaign", ylab = "Frequency", las = 2, col = "turquoise3", xlim = c(0,12))
par(new=T)
plot(prop.table(table(bankdata$campaign,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F, col = "orangered3", pch = 16, xlim = c(0.2,10.4))
mtext("Probability of Conversion",side=4, line = 3)
axis(4, ylim=c(0,1), col="black",col.axis="black",las=1)

## chiq.test
chisq.test(bankdata$campaign, bankdata$y)


# 1-13. pdays
table(bankdata$pdays)
#The idea of contact with clients, in general, seems more important than days passed. 999 value means the client wasn¡¯t previously contacted. Let¡¯s create a dummy out of it.
Clients who haven¡¯t been contacted in a previous campaign will be labeled ¡°0¡± in the pdays_dummy variable

bankdata <- bankdata %>% 
  mutate(pdays_dummy = if_else(pdays == 999, 0, 1))

bankdata %>% 
  ggplot() +
  aes(pdays_dummy) +
  geom_bar(aes(fill=y))

 bankdata %>% 
  ggplot() +
  aes(pdays_dummy) +
  geom_bar(aes(fill=y),position='fill')

par(mar = c(5,5,5,5))
barplot(table(bankdata$pdays_dummy), main="Distribution of Customers by pdays_dummy", ylab="Frequency", las = 2, col = "turquoise3", xlim = c(0,2.5))
par(new=T)
plot(prop.table(table(bankdata$pdays_dummy,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F, col = "orangered3", pch = 16, xlim = c(0.38,2.5))
mtext("Probability of Conversion",side=4, line = 3)
axis(4, ylim=c(0,1), col="black",col.axis="black",las=1)


## chiq.test
chisq.test(bankdata$pdays_dummy, bankdata$y)


#1-14. previous 
bankdata %>% 
  ggplot() +
  aes(previous) +
  geom_bar(aes(fill=y))

 bankdata %>% 
  ggplot() +
  aes(previous) +
  geom_bar(aes(fill=y),position='fill')


par(mar = c(5,5,5,5))
barplot(table(bankdata$previous), main="Distribution of Customers by previous", ylab="Frequency", las = 2, col = "turquoise3", xlim = c(0,9.7))
par(new=T)
plot(prop.table(table(bankdata$previous,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F, col = "orangered3", pch = 16, xlim = c(0.38,8.5))
mtext("Probability of Conversion",side=4, line = 3)
axis(4, ylim=c(0,1), col="black",col.axis="black",las=1)

## chiq.test
chisq.test(bankdata$previous, bankdata$y)



#1-15.poutcome
table(bankdata$poutcome)

bankdata %>% 
  ggplot() +
  aes(poutcome) +
  geom_bar(aes(fill=y))

 bankdata %>% 
  ggplot() +
  aes(poutcome) +
  geom_bar(aes(fill=y),position='fill')

par(mar = c(5,5,5,5))
plot(bankdata$poutcome, main="Distribution of Customers by poutcome", ylab = "Frequency", las = 2, col = "turquoise3", xlim = c(0,3.7))
par(new=T)
plot(prop.table(table(bankdata$poutcome,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F, col = "orangered3", pch = 16, xlim = c(0.38,3.6))
mtext("Probability of Conversion",side=4, line = 3)
axis(4, ylim=c(0,1), col="black",col.axis="black",las=1)

## chiq.test
chisq.test(bankdata$poutcome, bankdata$y)



## economic index data

bankdata %>% 
  select(emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed) %>% 
  cor() %>% 
  corrplot(method = "number",
           type = "upper",
           tl.cex = 0.8,
           tl.srt = 45,
           tl.col = "black")


# anova
anova(aov(emp.var.rate~y,data=bankdata))
anova(aov(cons.price.idx~y,data=bankdata))
anova(aov(cons.conf.idx~y,data=bankdata))
anova(aov(euribor3m~y,data=bankdata))
anova(aov(nr.employed~y,data=bankdata))





# prediction model 

rm(list=ls())

#library list
library(dplyr)
library(C50)
library(e1071)
library(gmodels) # Cross Tables [CrossTable()]
library(ggmosaic) # Mosaic plot with ggplot [geom_mosaic()]
library(corrplot) # Correlation plot [corrplot()]

library(ggpubr) # Arranging ggplots together [ggarrange()]
library(cowplot) # Arranging ggplots together [plot_grid()]
library(caret) # ML [train(), confusionMatrix(), createDataPartition(), varImp(), trainControl()]

library(ROCR) # Model performance [performance(), prediction()]
library(plotROC) # ROC Curve with ggplot [geom_roc()]
library(pROC) # AUC computation [auc()]
library(rpart) # Decision trees [rpart(), plotcp(), prune()]
library("tidyr")
library(ranger)
library(tidyverse)
library(rpart.plot) # Decision trees plotting [rpart.plot()]
library(party)
library("DMwR")
library("mice")



# load the library and the data

setwd("C:\\Users\\Next investment\\Desktop\\business (2)")

bank_data = read.csv(file = "finishh.csv")

bank_data1 = read.csv(file = "11.csv")

bank_data2 = read.csv(file = "22.csv")

bank_data3 = read.csv(file = "33.csv")




# explore the data structure
str(bank_data)
head(bank_data)
dim(bank_data)

CrossTable(bank_data$y)




    
    
# remove default which is not relevant for classification
bank_data = bank_data[, !names(bank_data) %in% c("default")]
bank_data1= bank_data1[, !names(bank_data1) %in% c("default")]
bank_data2= bank_data2[, !names(bank_data2) %in% c("default")]
bank_data3= bank_data3[, !names(bank_data3) %in% c("default")]



# split 70 percent of the data into the training dataset and 30 percent into the testing dataset
set.seed(1234)
index = sample(2, nrow(bank_data), replace = TRUE, prob = c(0.7, 0.3))

trainset = bank_data[index==1,]
testset = bank_data[index==2,]

#for bank_data1 split 70 percent 
set.seed(1234)
index= sample(2,nrow(bank_data1[-1]),replace = TRUE, prob=c(0.7,0.3))

trainset1=bank_data1[index==1,]
testset1=bank_data1[index==2,]

#for bank data2
set.seed(1234)
index= sample(2,nrow(bank_data2[-1]),replace = TRUE, prob=c(0.7,0.3))

trainset2=bank_data2[index==1,]
testset2=bank_data2[index==2,]

#for bank data3
set.seed(1234)
index= sample(2,nrow(bank_data3[-1]),replace = TRUE, prob=c(0.7,0.3))

trainset3=bank_data3[index==1,]
testset3=bank_data3[index==2,]



# check the data size
str(trainset)
str(trainset1)

trainset=trainset[-1]
trainset1=trainset1[-1]
trainset2=trainset2[-1]
trainset3=trainset3[-1]

testset=testset[-1]
testset1=testset1[-1]
testset2=testset2[-1]
testset3=testset3[-1]

#################################################################
# Tree Model and Random Forest Model
#################################################################

# recursive partitioning tree
library(rpart)

y.rp = rpart(y ~ ., data=trainset)
y.rp

##for testset1,2,3

y1.rp=rpart(y~.,data=trainset1)
y2.rp=rpart(y~.,data=trainset2)
y3.rp=rpart(y~.,data=trainset3)


# visualize the tree
plot(y.rp)
text(y.rp)

plot(y1.rp)
text(y1.rp)

plot(y2.rp)
text(y2.rp)

plot(y3.rp)
text(y3.rp)


# use parameters to adjust the layout
plot(y.rp, uniform=TRUE, branch=0.1, margin=0.1)
text(y.rp, all=TRUE, use.n=TRUE)

predictions = predict(y.rp, testset, type="class")
table(predictions, testset$y)

predictions1=predict(y1.rp, testset1, type="class")
table(predictions1, testset1$y)


predictions2=predict(y2.rp, testset2, type="class")
table(predictions2, testset2$y)


predictions3=predict(y3.rp, testset3, type="class")
table(predictions3, testset3$y)




# Performance: Confusion matrix
library(caret)
confusionMatrix(table(predictions, testset$y))
confusionMatrix(table(predictions1, testset1$y))
confusionMatrix(table(predictions2, testset2$y))
confusionMatrix(table(predictions3, testset3$y))



#CI tree

library(party)

y.ci = ctree(y ~ ., data=trainset)
y1.ci=ctree(y~.,data=trainset1)
y2.ci=ctree(y~.,data=trainset2)
y3.ci=ctree(y~.,data=trainset3)

# visualize the tree
plot(y.ci) 
# export the tree graph
jpeg(file="c:/tmp/y.ci.jpg",width=2000,height=1000)
plot(y.ci, main="CI Tree") 
dev.off()



# performance
predictions = predict(y.ci, testset)
predcitions1=predict(y1.ci, testset1)
predictions2=predict(y2.ci, testset2)
predictions3=predict(y3.ci, testset3)

table(predictions, testset$y)
table(predictions1, testset1$y)
table(predictions2, testset2$y)
table(predictions3, testset3$y)

# performance: Confusion matrix
library(caret)
confusionMatrix(table(predictions, testset$y))
confusionMatrix(table(predictions1, testset1$y))
confusionMatrix(table(predictions2, testset2$y))
confusionMatrix(table(predictions3, testset3$y))

# random forest
library(randomForest)
y.rf = randomForest(y ~ ., data=trainset, importance=T)
y1.rf = randomForest(y ~ ., data=trainset1, importance=T)
y2.rf = randomForest(y ~ ., data=trainset2, importance=T)
y3.rf = randomForest(y ~ ., data=trainset3, importance=T)


hist((importance(y.rf)))

importance(y1.rf)
importance(y2.rf)
importance(y3.rf)


# performance
predictions = predict(y.rf, testset)
predictions1 = predict(y1.rf, testset1)
predictions2 = predict(y2.rf, testset2)
predictions3 = predict(y3.rf, testset3)


table(predictions, testset$y)
table(predictions1, testset1$y)
table(predictions2, testset2$y)
table(predictions3, testset3$y)

# Performance: Confusion matrix

confusionMatrix(table(predictions, testset$y))
confusionMatrix(table(predictions1, testset1$y))
confusionMatrix(table(predictions2, testset2$y))
confusionMatrix(table(predictions3, testset3$y))


#################################################################
# Naive Bayes
#################################################################

# Naive Bayes
library(e1071)
y.NB = naiveBayes(trainset[,!names(trainset) %in% c("y")], trainset$y)
y1.NB= naiveBayes(trainset1[,!names(trainset1) %in% c("y")], trainset1$y)

y2.NB= naiveBayes(trainset2[,!names(trainset2) %in% c("y")], trainset2$y)

y3.NB= naiveBayes(trainset3[,!names(trainset3) %in% c("y")], trainset3$y)



# performance
predictions = predict(y.NB, testset[,!names(testset) %in% c("y")])
predictions1 = predict(y1.NB, testset1[,!names(testset1) %in% c("y")])

predictions2 = predict(y2.NB, testset2[,!names(testset2) %in% c("y")])

predictions3 = predict(y3.NB, testset3[,!names(testset3) %in% c("y")])




table(predictions,testset$y)

table(predictions1, testset1$y)

table(predictions2, testset2$y)

table(predictions3, testset3$y)




# performance: confusion matrix


confusionMatrix(table(predictions, testset$y))
confusionMatrix(table(predictions1, testset1$y))
confusionMatrix(table(predictions2, testset2$y))
confusionMatrix(table(predictions3, testset3$y))

#################################################################
# GBM
#################################################################

# GBM

library(gbm)

##trainset

head(trainset)
trainset=trainset[,!names(trainset) %in% c("yes", "no")]
testset=testset[,!names(testset) %in% c("yes", "no")]

trainset$y = ifelse(trainset$y=="yes", 1, 0)
testset$y = ifelse(testset$y=="yes", 1, 0)

set.seed(1234)
y.GBM=gbm(y~., distribution="bernoulli", data=trainset, n.trees=1000, 
              interaction.depth=7, shrinkage=0.01, cv.folds = 3)
summary(y.GBM)



##trainset1

trainset1=trainset1[,!names(trainset1) %in% c("yes", "no")]
testset1=testset1[,!names(testset1) %in% c("yes", "no")]

trainset1$y = ifelse(trainset1$y=="yes", 1, 0)
testset1$y = ifelse(testset1$y=="yes", 1, 0)

set.seed(1234)
y1.GBM=gbm(y~., distribution="bernoulli", data=trainset1, n.trees=1000, 
              interaction.depth=7, shrinkage=0.01, cv.folds = 3)
summary(y1.GBM)


##trainset2

trainset2=trainset2[,!names(trainset2) %in% c("yes", "no")]
testset2=testset2[,!names(testset2) %in% c("yes", "no")]

trainset2$y = ifelse(trainset2$y=="yes", 1, 0)
testset2$y = ifelse(testset2$y=="yes", 1, 0)

set.seed(1234)
y2.GBM=gbm(y~., distribution="bernoulli", data=trainset2, n.trees=1000, 
              interaction.depth=7, shrinkage=0.01, cv.folds = 3)
summary(y2.GBM)


##trainset3

trainset3=trainset3[,!names(trainset3) %in% c("yes", "no")]
testset3=testset3[,!names(testset3) %in% c("yes", "no")]

trainset3$y = ifelse(trainset3$y=="yes", 1, 0)
testset3$y = ifelse(testset3$y=="yes", 1, 0)

set.seed(1234)
y3.GBM=gbm(y~., distribution="bernoulli", data=trainset3, n.trees=1000, 
              interaction.depth=7, shrinkage=0.01, cv.folds = 3)
summary(y3.GBM)




# prediction
pred = predict(y.GBM, testset, n.trees=1000)
pred1 = predict(y1.GBM, testset1, n.trees=1000)
pred2 = predict(y2.GBM, testset2, n.trees=1000)
pred3 = predict(y3.GBM, testset3, n.trees=1000)




# finding the best cut-off 

library(pROC)
y.roc=roc(testset$y, pred)
y1.roc=roc(testset1$y, pred1)
y2.roc=roc(testset2$y, pred2)
y3.roc=roc(testset3$y, pred3)
plot(y.roc)
lines(y1.roc,col='green')
lines(y2.roc,col='yellow')
lines(y3.roc,col='blue')

coords(y.roc, "best")
coords(y1.roc,"best")
coords(y2.roc,"best")
coords(y3.roc,"best")


# obtaining the classification table
predictions = ifelse(pred>coords(y.roc, "best") ["threshold"], 1, 0)
predictions1 = ifelse(pred1>coords(y1.roc, "best") ["threshold"], 1, 0)
predictions2 = ifelse(pred2>coords(y2.roc, "best") ["threshold"], 1, 0)
predictions3 = ifelse(pred3>coords(y3.roc, "best") ["threshold"], 1, 0)

table(predictions, testset$y)
table(predictions1, testset1$y)

table(predictions2, testset2$y)

table(predictions3, testset3$y)



# performance: confusion matrix

confusionMatrix(table(predictions, testset$y))
confusionMatrix(table(predictions1, testset1$y))
confusionMatrix(table(predictions2, testset2$y))
confusionMatrix(table(predictions3, testset3$y))




