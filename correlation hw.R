data <- read.csv("carprices.csv", stringsAsFactors = TRUE)

# 1. Generate a box-plot of the price and identify if there are outliers.  
boxplot(data$price)
# 2. Plot the distribution of price using a histogram. What do you observe? 
hist(data$price)


#3. Since price is not so normally distributed, we may consider taking a log transform, (use lprice= log(price)), and then plot a histogram of lprice instead. How does the distribution of lprice now differ from price before the log transformation? 
#log normalize
lprice=log(data$price)
hist(lprice)


#4. Generate scatter-plots for lprice vs horsepower and lprice vs citympg. What are the relationships  you found for each of these generated plots? 
plot(lprice,data$horsepower)

plot(lprice,data$citympg)

#data$fueltype <- factor(data$fueltype)
#data <- factor(data)



#5. Create a training set (call it carprices_train) with the first 164 records.  Create a test set (call it carprices_test) with the remaining 41 records. Fit a multiple linear regression (MLR) model to this dataset, with lprice as the dependent variable. Use the independent variables given below for your model: 
carprices_train <- data[1:164,]
carprices_test <- data[165:205,]

carprices_test$price


model <- lm(log(carprices_train$price) ~ carprices_train$fueltype + carprices_train$enginesize + 
              carprices_train$stroke + carprices_train$peakrpm + carprices_train$citympg + carprices_train$highwaympg + carprices_train$carwidth + 
              carprices_train$aspiration + carprices_train$boreratio)

summary

#6. Do any of the variables have to be dropped because of multicollinearity?  Explai
anova(model)
vif(model)


#dropping citympg
#carprices_train$citympg <- NULL
#-statistic evaluates whether or not there is significant association between the predictor and the outcome variable
#independant variable  = explanatory variable
#dependant = response varaible 
#carprices_train$cityhighwaympg <- NULL


#a. Re-run the model by dropping a variable you assess to be the most problematic for multicollinearity (i.e., the variable with the highest VIF). Examine the coefficients of the variables in the new model, in particular, the ones with high VIF in the previous model and check if the coefficient values changed significantly.   
model2 <- lm(log(carprices_train$price) ~ carprices_train$fueltype + carprices_train$enginesize + 
              carprices_train$stroke + carprices_train$peakrpm + carprices_train$citympg + carprices_train$carwidth + 
              carprices_train$aspiration + carprices_train$boreratio)
#7. Would you drop any of the variables used in your revised model based on the t-values or p-values? 
summary(model2)
vif(model2)



#a. If yes, drop the variables, one at a time, and re-run the model until you see no need to drop any variable. (Note: Again, you need to drop variables one at a time because the p-value of one predictor may increase or decrease when a new set of variables are used.) 
#remove boreratio from model
model3 <- lm(log(carprices_train$price) ~ carprices_train$fueltype + carprices_train$enginesize + 
               carprices_train$stroke + carprices_train$peakrpm + carprices_train$citympg + carprices_train$carwidth + 
               carprices_train$aspiration)
#9. Interpret the R-square of your final model.  What does the R-square tell you about the final model. 
summary(model3)


#remove aspiration from model
model4 <- lm(log(carprices_train$price) ~ carprices_train$fueltype + carprices_train$enginesize +  carprices_train$stroke + carprices_train$peakrpm + carprices_train$citympg + carprices_train$carwidth)
model4.5 <- lm(log(price)~fueltype+enginesize+stroke+peakrpm+citympg+carwidth, data=carprices_train)
#10.  Provide your interpretations on coefficients for each of the predictors in the model.  How do you determine if the coefficients are statistically significant (use the p-values to explain). 
summary(model4)




#11.  Report the MSE obtained on the carprices_train and carprices_test for the final model. What do you observe?  You may find that the MSE on the test set is much higher than the estimated MSE obtained using the training set.  Provide a reason why they are different. 
mean(model4.5$residuals^2)


#predict(model4, carprices_test)
predict(model4.5, carprices_test)

mean((log(carprices_test$price) - predict(model4.5 , carprices_test ))^2)

#mean(model5$residuals^2)
#unique(carprices_test$fueltype)
#unique(carprices_train$fueltype)