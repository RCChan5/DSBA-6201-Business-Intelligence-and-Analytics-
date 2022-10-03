# Check working directory and set it to the directory where data files are.
getwd()
setwd("~/CSVDatasets")
fitness<-read.csv(file="FITNESS.csv",header=TRUE)
# Check the dataset
fitness
head(fitness)
summary(fitness)
View(fitness)

# Scatter plot, histogram, boxplots
plot(fitness$RunTime, fitness$Oxygen_Consumption)
hist(fitness$Oxygen_Consumption)
boxplot(fitness$Maximum_Pulse)
boxplot(fitness[c(3,4,5)])

# 2x2 layout
par(mfrow=c(2,2))
boxplot(fitness$RunTime)
par(mfrow=c(1,1))

# All scatter plots in one picture
str(fitness)
pairs(fitness[3:length(fitness)])
names(fitness)
pairs(fitness[c(6,3,10)])

# Attach and detach can simplify the expression.
attach(fitness)
plot(Performance, Oxygen_Consumption)
boxplot(Maximum_Pulse)
detach(fitness)

# Some people hate using attach because it can attach the same data frame object
# multiple tims. The following can completely detach all fitness objects.
while("fitness" %in% search()) detach(fitness)

# Run a simple linear regression
lm.fit=lm(Oxygen_Consumption~RunTime, data=fitness)
lm.fit
summary(lm.fit)
plot(fitness$RunTime, fitness$Oxygen_Consumption)
abline(lm.fit)
predict(lm.fit,data.frame(RunTime=5))

# Multiple Linear Regression
attach(fitness)  # no need to specify the data frame name.
lm.fit=lm(Oxygen_Consumption~RunTime+Performance)
lm.fit
summary(lm.fit)
# Figure out what t-score and p-value represent using t distribution with d.f = 31-2-1
curve(dt(x,28),xlim=c(-5,5))
print(2*pt(-4.207,28)) # two-tail test
print(2*pt(-1.348,28))

# What is F-statistic (F value) here?  It is the value on F distribution.
# How is p-value (of F-test) calculated? First take a look at the pdf of F distribution.
curve(df(x,2,28),xlim=c(0,50))
# The following is the right tail probability with F-value = 44.09.
print(1-pf(44.09,2,28), digits=10) # one-tail (upper) test

# How do we find MSE (estimate)?  You need to divide the sum by error degree of freedom.
# If you square-root it, you will get 'Residual standard error' shown in the summary result.
sum((lm.fit$residuals)^2)/(31-2-1)
# R-squared can be calculated from the fact 
# that it is the proportion of the variance of y explained by the model,
# R-squared = (TSS-RSS)/TSS =  1-RSS/TSS
1-sum((lm.fit$residuals)^2)/sum((fitness$Oxygen_Consumption-mean(fitness$Oxygen_Consumption))^2)


# You can also create ANOVA table and figure out how R-squared is calculated.
anova(lm.fit)
# Do you get the same R-squared value in the summary report using this formula?
# R-squared = (Sum Sq explained by the model)/(Total Sum Sq)
print(R_sq<-(633.01+13.32)/(633.01+13.32+205.22))
# Adj. R-squared = 1-(n-1)(1-R-squared)/(n-p-1)
1-(31-1)*(1-R_sq)/(31-2-1)


# Checking for multicollinearity and running step-wise regression
cor(RunTime, Performance)
summary(lm(Oxygen_Consumption~.-Name-Gender,data=fitness))

# Name is a nominal variable that should not be used in the model.
#fitness<-subset(fitness, select=-Name) 

fitness$Name<-NULL # same result as above
# Gender is a qualitative predictor. 
# Indicator variables are automatically created in R if the type is a factor
fitness$Gender<-as.factor(fitness$Gender)

# You are advised to check the multicollinearity before step-wise regression
if(!require(car)) {install.packages("car");library(car)}
# Run with all variables and check multicollinearity
vif(lm(Oxygen_Consumption~.,data=fitness))
vif(lm(Oxygen_Consumption~.-Performance,data=fitness))
step.fit<-step(lm(Oxygen_Consumption~.-Performance,data=fitness))

# R-squared will be the maximum if all variables are used.  
# But, it may not be the case for Adjusted R-squared.
summary(lm(Oxygen_Consumption~.,data=fitness))
summary(step.fit)
detach(fitness)
