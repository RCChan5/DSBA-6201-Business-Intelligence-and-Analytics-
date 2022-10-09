#1.	Read the training and test datasets into R-Studio.
data_Train <- read.csv("Data/org.train.csv", stringsAsFactors = TRUE)
data_Test <- read.csv("Data/org.test.csv", stringsAsFactors = TRUE)

#2.	If there are any categorical variables in the datasets, convert them to factors (use as.factor() function).


colnames(data_Test)
summary(data_Test)
head(data_Test)
str(data_Test)


#data_Test$ID  <- as.factor(data_Train$ID)
#contrasts(data_Train$DemGender)
data_Train$DemGender <- as.factor(data_Train$DemGender)
data_Train$PromClass <- as.factor(data_Train$PromClass)
#data_Train$TargetBuy <- as.factor(data$TargetBuy)

data_Test$DemGender <- as.factor(data_Test$DemGender)
data_Test$PromClass <- as.factor(data_Test$PromClass)


#factoring the test dataset
#data_Test <- as.factor(data_Test)



#3.	Fit a logistic regression to your training dataset with all the independent variables, except ID. 

testID <- data_Test$ID
trainID <- data_Train$ID

data_Test["ID"] <- NULL
data_Train["ID"] <-  NULL
model <- glm(data_Train$TargetBuy ~ data_Train$DemAffl + data_Train$DemAge + data_Train$DemGender + data_Train$PromClass + data_Train$PromSpend + data_Train$PromTime,  data = data_Train)

summary(model)



#4.	Use the estimated model to predict the classes for the test dataset. Use the threshold of 0.5 probability to determine if the customer is predicted buy organics products (i.e., predicted TargetBuy = 1).


p <- predict(model,data_Test)

prediction <- as.integer(p > 0.5)


confusion_mat <- addmargins(table(train$admission, prediction))
