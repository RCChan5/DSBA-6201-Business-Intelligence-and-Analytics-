# Make sure that data files are in your working directory
getwd()
#setwd("H:/R")

titan <- read.csv("TITANIC_FORMATTED.csv")
head(titan)
summary(titan)
str(titan)
# Categorical variables have to be converted to factor data types
titan$Survival<-as.factor(titan$Survival)
titan$Gender<-as.factor(titan$Gender)
titan$Class<-as.factor(titan$Class)
str(titan)

# Male is coded as 1 because the factor level is determined alphabetically by default.
contrasts(titan$Gender)
# You can change the order of the factor level if you want to code Female as 1
titan$Gender<-factor(titan$Gender,levels=c("Male","Female"))
contrasts(titan$Gender)

titan$Gender<-factor(titan$Gender,levels=c("Female","Male")) # back to default

# To make sure that there are no pre-attached titan data frame object.
while('titan' %in% search()) detach(titan)
attach(titan)

# Can we run linear regression with this data?
lm.fit = lm(Survival~Age)
#Yes, if you transform Survival into a numeric variable
titan$DidSurvive = ifelse(titan$Survival=='Survived',1,0)
lm.fit = lm(DidSurvive~Age, data=titan)
summary(lm.fit)
plot(Age,titan$DidSurvive, pch=4, col='blue')
abline(lm.fit,col='red')

lm.fit = lm(DidSurvive~Gender, data=titan)
summary(lm.fit)
plot(ifelse(titan$Gender=='Male',1,0),titan$DidSurvive, pch=4, col='blue')
abline(lm.fit,col='red')

lm.fit = lm(DidSurvive~Age+Gender+Class, data=titan)
summary(lm.fit)

detach(titan)


# Prediction from linear regression
# Note that there are missing values.
# The observations with missing values are ignored.
predict(lm.fit,data.frame(Age=49,Gender='Male',Class='3rd'))
predict(lm.fit,data.frame(Age=3,Gender='Female',Class='1st'))

# Because the prediction can be greater than 1 or less than 0,
# consider using sigmoid function, which is the inverse function of logit

# Sigmoid function
curve(1/(1+exp(-x)),-10,10)
# Logit function
curve(log(x/(1-x)), 0,1)

# Logistic regression uses the logit function
glm.fit = glm(Survival~Age+Gender+Class,family=binomial, data =titan)

summary(glm.fit)
predict(glm.fit,data.frame(Age=49,Gender='Male',Class='3rd'))
# If type is not specified, the prediction is a logit value.
# Thus, the probability can be obtained using the sigmoid function.
1/(1+exp(-(-2.952054)))

# Or, simply use type='response'
predict(glm.fit,data.frame(Age=49,Gender='Male',Class='3rd'), type='response')

predict(glm.fit,data.frame(Age=3,Gender='Female',Class='1st'))
1/(1+exp(-(3.422031)))

predict(glm.fit,data.frame(Age=3,Gender='Female',Class='1st'), type='response')

# Wald test
if(!require(aod)) {install.packages("aod");library(aod)}

wald.test(Sigma = vcov(glm.fit), b = coef(glm.fit), Terms = 4:5)


# Manual recoding of categorical variables
titan$IsFemale = ifelse(titan$Gender=='Female',1,0)
titan$IsMale = ifelse(titan$Gender=='Male',1,0)
titan$C1 = ifelse(titan$Class=='1st',1,0)
titan$C2 = ifelse(titan$Class=='2nd',1,0)
titan$C3 = ifelse(titan$Class=='3rd',1,0)
summary(titan)

# This should give us the same result.
glm.fit1 = glm(Survival~Age+IsMale+C2+C3,family=binomial, data=titan)
summary(glm.fit1)

# Intercept has to change due to the recoding of Gender.
# Nonetheless, there is no change in the result.
glm.fit1a = glm(Survival~Age+IsFemale+C2+C3,family=binomial, data=titan)
summary(glm.fit1a)

# See how the intercept and coefficients change when we recode Class
glm.fit2 = glm(Survival~Age+IsMale+C1+C3,family=binomial, data=titan)
summary(glm.fit2) # Class 2 is the reference class

glm.fit3 = glm(Survival~Age+IsMale+C1+C2,family=binomial, data=titan)
summary(glm.fit3) # Class 3 is the reference class


                              