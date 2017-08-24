setwd("D:\\Education\\stepupanalytics_Blog\\linear regression")
help(package = "MASS")
install.packages("MASS")
install.packages("ISLR")

library(MASS) #loads dataset from the book MASS
library(ISLR) #dataset by Statistical Learning professors

##Simple Linear Regression

# reading data in R
data(Boston)

# name of the variables of Boston dataset  
names(Boston)

# description of the Boston dataset 
?Boston

# viewing visually the Boston dataset
View(Boston)

# dimension of the Boston Dataset
dim(Boston)

# plotting the variables to see if there is any relationship:
plot(medv~rm,main = "Boston", data = Boston)
plot(medv~lstat, main = "Boston", data = Boston) #as lower status people decrease, median value of houses increase

# fitting of model
basemodel<-lm(medv~lstat, Boston)
basemodel
summary(basemodel)
anova(basemodel)

#add a line to the fit 
abline(basemodel,col="red")

#see the components of fit
names(basemodel)
# [1] "coefficients"  "residuals"     "effects"       "rank"          "fitted.values" "assign"       
# [7] "qr"            "df.residual"   "xlevels"       "call"          "terms"         "model" 

basemodel$coefficients
# (Intercept)      lstat 
# 34.5538409  -0.9500494 

#95% confidence interval
confint(basemodel)
#                 2.5 %     97.5 %
# (Intercept) 33.448457 35.6592247
# lstat       -1.026148 -0.8739505

#predict medv (response) for these 3 values of lstat (predictor). 
#also show confidece intervals
predict(basemodel,data.frame(lstat=c(5,10,15)),interval="confidence")
#        fit      lwr      upr
# 1 29.80359 29.00741 30.59978
# 2 25.05335 24.47413 25.63256
# 3 20.30310 19.73159 20.87461

# Skill-Up Scale-Up !!!

#Multiple Linear Regression in R

# fitting the regression model
model <- lm(medv~.,data = Boston)

# summary of the fitted model
summary(model)

# anova of the fitted model
anova(model)

# plot of the fitted model
plot(model$residuals)

# fitting the regression model with only significant variables
finalmodel <- update(model,~.-age-indus, data = Boston)

# summary of fitted model
summary(finalmodel)

# anova of fitted model
anova(finalmodel)

# plotting of the finalmodel
plot(finalmodel$residuals)

# Skill-Up Scale-Up
