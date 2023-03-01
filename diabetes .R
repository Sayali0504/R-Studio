################# Data Import #######################
full_data = read.csv("C:/Users/Lenovo/Desktop/DATA SCIENCE DATA/R Studio/ML in R/Logistics in R/diabetes.csv")

#####################################################################
#################### Data Pre-Processing  #######################
#####################################################################

######### Conversion from integer to Categorical 
str(full_data)
full_data$Outcome = as.factor(full_data$Outcome)
str(full_data)

######## Identify & Treatment of Missing Value 
sapply(full_data,function(x) sum(is.na(x)))

################# Identifcation & Treatment of  Outlier Fare ################
boxplot(full_data)

##############333#Data Partition#789#######################
set.seed(123)
library(caret)

sample = createDataPartition(full_data$Outcome,p=0.7,list=FALSE)
training = full_data[sample,]
testing = full_data[-sample,]

################## model Building ################ 
model = glm(Outcome ~., family = 'binomial', data = training)
summary(model)

################## model Building ################ 
reg.model1 = step(glm(Outcome ~., family = 'binomial', data = training),direction = "both")
summary(reg.model1)

##################### To check multicollinearity #####################
library(car)
vif(reg.model1) # no multi-colinearity( Every X is independent)

##################### # odds Ratio ##################### 

exp(coef(reg.model1))

##################### ## Prediction on Training data ##################### 

training$probability  = predict(reg.model1, training, type='response')  # Response = Probability
training$Predict = as.factor(ifelse(training$probability >= 0.70,1,0))

###################### Accuracy of Training data  #####################

library(caret)
confusionMatrix(training$Outcome,training$Predict,positive = "1")

######################## Roc Curve  on Training #####################

library(ROCR)
library(ggplot2)

ROCRpred = prediction(training$probability, training$Outcome)
ROCRperf_train = performance(ROCRpred, "tpr", "fpr")
par(mfrow=c(1,2))

plot(ROCRperf_train,main="Training Data")

##################### ## Prediction on testing data ##################### 

testing$probability  = predict(reg.model1, testing, type='response')
testing$Predict = as.factor(ifelse(testing$probability >= 0.70,1,0))

###################### Accuracy of testing data  #####################

library(caret)
confusionMatrix(testing$Outcome,testing$Predict,positive = "1")

######################## Roc Curve on testing data#####################

library(ROCR)
library(ggplot2)

ROCRpred = prediction(testing$probability, testing$Outcome)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf,main="Testing Data")

######################## Live Data ##################################
Live_data = training[c(2,4,5),-c(9:11)]

Live_data$probability=predict(reg.model1,Live_data,type ="response") 
Live_data$Predict = as.factor(ifelse(Live_data$probability >= 0.70,1,0))
View(Live_data)

######################### AUC #####################

pred = prediction(testing$probability, testing$Outcome)
as.numeric(performance(pred, "auc")@y.values)



