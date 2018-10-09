rm(list=ls())

#Assign CSV file name
BCW<-file("C://Users/sanja/Google Drive/2ndSem/CS513_KDD_KashaDehnad/Project/dfr.csv",'r')

#Read CSV file and name the file as dfr. 
#Remove any row with a missing value in any of the columns.
dfr<-(read.csv(BCW, header = TRUE))

#Close the CSV file
close(BCW)

#Check the Dataframe
View(dfr)

#Check the Structure of Dataframe
str(dfr)

#I.	Summarizing each column (e.g. min, max, mean )
## II.	Identifying missing values
summary(dfr)


########################################################################################
#mmnorm<-function(x,minx,maxx){z<-((x-minx)/(maxx-minx))
#return(z)
#}


#dfr_normalized <- as.data.frame(
#  cbind(case_status=dfr[,1],
#        class_of_admission=mmnorm(dfr[,2],min(dfr[,2]),max(dfr[,2])),
#        country_of_citizenship=mmnorm(dfr[,3],min(dfr[,3]),max(dfr[,3])),
#        employer_city=mmnorm(dfr[,4],min(dfr[,4]),max(dfr[,4])),
#        employer_name=mmnorm(dfr[,5],min(dfr[,5]),max(dfr[,5])),
#        employer_state=mmnorm(dfr[,6],min(dfr[,6]),max(dfr[,6])),
#        pw_soc_code=mmnorm(dfr[,7],min(dfr[,7]),max(dfr[,7])),
#        pw_source_name_9089=mmnorm(dfr[,8],min(dfr[,8]),max(dfr[,8])),
#        year=mmnorm(dfr[,9],min(dfr[,9]),max(dfr[,9])),
#        remuneration =mmnorm(dfr[,10],min(dfr[,10]),max(dfr[,10]))
#  )
#)

#View(dfr_normalized)

#str(dfr_normalized)
#dfr_normalized$case_status<-as.factor(dfr_normalized$case_status)
#summary(dfr_normalized)
#########################################################################################

#idx<-sort(sample(nrow(dfr_normalized),as.integer(.65*nrow(dfr_normalized))))
#training<-dfr_normalized[idx,]

#test<-dfr_normalized[-idx,]
#library(class)
#predict<-knn(training[,-1],test[,-1],training[,1],k=1000)

#table(Prediction=predict,Actual=test[,1] )

# k-NN using caret:
#install.packages(ISLR);
#library(ISLR)

#library(caret)

# Split the data:
#data(iris)
#indxTrain <- createDataPartition(y = iris$Sepal.Length,p = 0.75,list = FALSE)
#training <- iris[indxTrain,]
#testing <- iris[-indxTrain,]

# Run k-NN:
#set.seed(400)
#ctrl <- trainControl(method="repeatedcv",repeats = 10)
#knnFit <- train(case_status ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneLength = 20)
#knnFit

#Use plots to see optimal number of clusters:
#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
#plot(knnFit)

#installed.packages()
#install.packages("kknn")
#Use the R library("kknn") 

#library(kknn)


library(e1071)
library(class)
#Copying the file into another file for performing classification
dfr1<-dfr
#Converting features into factors whereever necessary
dfr1$case_status<-as.factor(dfr1$case_status)
dfr1$class_of_admission<-as.factor(dfr1$class_of_admission)
dfr1$country_of_citizenship<-as.factor(dfr1$country_of_citizenship)
dfr1$employer_city<-as.factor(dfr1$employer_city)
dfr1$employer_name<-as.factor(dfr1$employer_name)
dfr1$employer_state<-as.factor(dfr1$employer_state)
dfr1$pw_soc_code<-as.factor(dfr1$pw_soc_code)
dfr1$pw_source_name_9089<-as.factor(dfr1$pw_source_name_9089)
dfr1$year<-as.factor(dfr1$year)
dfr1$remuneration<-as.factor(dfr1$remuneration)
str(dfr1)
dfr$case_status<-as.factor(dfr$case_status)

### Naive Bayes classification using all variables 

nBayes_all <- naiveBayes(case_status ~., data =dfr)
nBayes_all


category_all<-predict(nBayes_all,dfr  )
category_all 

table(NBayes_all=category_all,case_status=dfr$case_status)

NB_wrong<-sum(category_all!=dfr$case_status)

NB_error_rate<-NB_wrong/length(category_all)

NB_error_rate

NB_wrong

str(dfr)


index<-sort(sample(nrow(dfr),round(.25*nrow(dfr))))
training<-dfr[-index,]
test<-dfr[index,]
View(training)
str(training)
install.packages("C50", repos="http://R-Forge.R-project.org")
install.packages("C50")
library('C50')
View(dfr1)

# C50  classification 
library('C50')
C50_class <- C5.0( case_status~.,data=training)
C50_class
#c50 combines categories back

summary(C50_class )
#dev.off()
plot(C50_class)
C50_predict<-predict( C50_class ,test , type="class" )
C50_predict
table(actual=test[,1],C50=C50_predict)
wrong<- (test[,1]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,1])
c50_rate




#CART CLASSIFICATION

library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)

index1<-sort(sample(nrow(dfr1),round(.25*nrow(dfr1))))
training1<-dfr1[-index1,]
test1<-dfr1[index1,]
View(training1)
str(training1)

CART_class<-rpart(case_status~.,data=training1)
rpart.plot(CART_class)
CART_predict<-predict(CART_class,test1)
str(CART_predict)
print(CART_predict)
CART_predict2<-predict(CART_class,test1, type="class")
CART_wrong2<-sum(test1[,1]!=CART_predict2)
CART_error_rate2<-CART_wrong2/length(test1[,1])
CART_error_rate2 
table(Actual=test1[,1],CART=CART_predict2)



#ANN Classification

Denied<-ifelse(dfr$case_status==0,1,0)
Approved<-ifelse(dfr$case_status==1,1,0)



dfr2<- na.omit(data.frame(dfr,Denied,Approved))
str(dfr2)
index2 <- seq (1,nrow(dfr),by=5)
test2<-dfr2[index2,]
training2<-dfr2[-index2,]

library("neuralnet")
?neuralnet()
str(dfr)
net_dfr2  <- neuralnet(Denied+Approved~class_of_admission+country_of_citizenship+employer_city+employer_name+employer_state+pw_soc_code+pw_source_name_9089+year+remuneration
                      ,training2, hidden=5, threshold=0.01)


#str(dfr2)
#length(dfr2$Denied)

#Plot the neural network
plot(net_dfr2)

net_dfr2_results <-compute(net_dfr2, test[,c(-1,-11,-12)]) 
class(net_dfr2_results$net.result)


str(net_dfr2_results)

resutls<-data.frame(Actual_Benign=test$benign,
                    Actual_Malignant=test$malignant,
                    ANN_Benign=round(net_dfr2_results$net.result[,1]),
                    ANN_Malignant=round(net_dfr2_results$net.result[,2]))


resutls2<-data.frame(Actual_Denied=test$Denied,
                     Actual_Approved=test$Approved,
                     ANN_Denied=round(net_dfr2_results$net.result[,1]),
                     ANN_Approved=round(net_dfr2_results$net.result[,2])
                     ,Prediction=ifelse(round(net_dfr2_results$net.result[,1])==1,'D','A'))

table(Actual=resutls2$Actual_Approved,Prediction=resutls2$Prediction)

wrong<- (round(net_dfr2_results$net.result[,1])!=test$Denied )
error_rate<-sum(wrong)/length(wrong)
error_rate




