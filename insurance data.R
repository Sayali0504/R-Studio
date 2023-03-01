######################### Data imported #########################

data = read.csv("C:/Users/Lenovo/Desktop/DATA SCIENCE DATA/R Studio/insurance.csv")
View(data)

###################### Removing irrelavent variable ######################
data$region = NULL


####################### Checking the Data type ######################
names(data)
str(data)

##################################################################
#######################       EDA         ######################
##################################################################

table(data$smoker)
table(data$sex)

################# converting categorical data to numeric 
data$Smoker_flag  =  ifelse(data$smoker=='yes',1,0)
data$sex_Flag = ifelse(data$sex=='male',1,0)

################### Data Conversion
str(data)
data$age = as.numeric(data$age)
data$children = as.numeric(data$children)
data$Smoker_flag  = as.numeric(data$Smoker_flag)
data$sex_Flag = as.numeric(data$sex_Flag)
data$Month = as.numeric(data$Month)

# Checking the Data type
str(data)

###################### Outlier  ######################

boxplot(data$age)
boxplot(data$bmi)
boxplot(data$children)
boxplot(data$charges)
boxplot(data$Month)

#######################  Treatment of outlier ###################### 
summary(data$bmi)
upper = 34.69+1.5*IQR(data$bmi);upper
data$bmi[data$bmi > upper] = upper
boxplot(data$bmi)
summary(data$bmi)

# Treatment of outlier for charges
summary(data$charges)
upper = 16640+1.5*IQR(data$charges);upper
data$charges[data$charges > upper] = upper
boxplot(data$charges)
summary(data$charges)

###################### data Subset ######################
abc = data[,-c(2,5)]     

####################### Data Partition ######################
# 546  # 123
set.seed(789)
library(caret)
Train  =  createDataPartition(abc$charges, p=0.70,list=FALSE) # random row number
training  =  abc[ Train, ]
testing  =  abc[ -Train, ]

##################################################################
####################### Model building ######################
##################################################################

cor(training)

model = lm(charges ~ .,data = training)
summary(model)

###################### Transformation #####################################
par(mfrow=c(2,2))
hist(training$charges,main="Original")
hist((1/training$charges),main="Reciporal") # reciporal
hist(log(training$charges),main="Log") # log
hist(sqrt(training$charges),main="Sqrt")
######################## model 2 ########################

model2 = step(lm(log(charges)~.,data = training),direction = "backward")
summary(model2)

############################# Multi-collinearity #############################
library(car)
vif(model2)

############################## Assumption of linear Regression ################
par(mfrow=c(2,2))
plot(model2)

hist(model2$residuals)

############################## Prediction on Test Data #############################

testing$Predicted = predict(model2,testing)

############################# Prediction to real value ########################
testing$Original = exp(testing$Predicted)

############################# Prediction For live data ########################

live=training[c(1,5,10),-4]
live$Predicted = exp(predict(model2,live))
View(live)
