library(ggplot2)
library(car)
library(effects)
setwd("~/Documents/RU/Sem2/Statistics/Week8")


###1a Load in ChildAggression.csv. 
#This data set contains information about the aggressiveness of different children and their life habits. 
#You are going to use the variable Aggression as your outcome variable and the other variables as your predictors. 
#These include Television (how much TV the kids watch), Computer_Games (how much they play computer games), Sibling_Aggression (how aggressive their siblings are), Diet (how healthy the food is they eat), Parenting_Style (how much parents allow children to misbehave), and Sleep_Quality (how much sleep they get).
child_aggression <- read.table("ChildAggression.csv",sep="\t",header=T,comment.char="",quote="")
head(child_aggression)

###1b Build your first multiple regression model. Inspect the output. 
#Is the model significant? How much variance is explained in the sample data? 
#How much variance can we expect would be explained in the population? 
#Why is Adjusted R^2 lower than Multiple R^2? 
#Which predictors are significant? Which predictors are not significant?
linearModel <- lm(Aggression~Television+Computer_Games+Sibling_Aggression+Diet+Parenting_Style+Sleep_Quality, data=child_aggression)
summary(linearModel)

#Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)         0.006945   0.024355   0.285 0.775623    
#   Television          0.031752   0.046128   0.688 0.491477    
#   Computer_Games      0.140010   0.037136   3.770 0.000178 ***
#   Sibling_Aggression  0.081304   0.038806   2.095 0.036539 *  
#   Diet               -0.107916   0.038150  -2.829 0.004815 ** 
#   Parenting_Style     0.057022   0.014580   3.911 0.000101 ***
#   Sleep_Quality      -0.011852   0.021055  -0.563 0.573711    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.3072 on 659 degrees of freedom
#Multiple R-squared:  0.08302,	Adjusted R-squared:  0.07467 
#F-statistic: 9.944 on 6 and 659 DF,  p-value: 1.598e-10

#The models formula is Aggression~Television+Computer_Games+Sibling_Aggression+Diet+Parenting_Style+Sleep_Quality. 
#Not all the main effects were significant. Computer_Games, Sibling_Aggression, Diet and Parenting_Style are significant. Television and Sleep_Quality are not.
#The model is significant overall (F6,659=9.944, p=1.598e-10) and has a low variance explanation (mult.R-squared=0.08302,adj.R-squared=0.07467)
#All regression coefficients, as well as their SE, t-scores and p-values are provided in the appendix.


#adj. R-squared is lower then mult. R-squared because there are multiple variables involved which in mult. R-squared increase explained variance because of random factors.

###1c Is there a problem with multicollinearity in this model?
vif(linearModel) #avg=1.23061
cor(child_aggression[,1:7])
#the avarage is higher then one but there don't seeem to be highly correlated predictors

###1d Now check the assumptions of 
#no autocorrelation, 
durbinWatsonTest(linearModel) #dw-test return no significant autocorrelation
#homoscedasticity, 
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plot(linearModel)
# There does not seem to be homoscedasticity as the data points have a nice cloud shape
#normally distributed residuals, 
#the residuals do not seem normal distributed as the tail and the head deviate a lot from the line
#and check that there are no highly influential points.
#there is no highly influential point that has a Cook's distance of > 1

###1e Now you are going to perform model selection. 
#Are there any predictors you could remove to make the model better? 
#Consider both AIC and significance in your decision. 
#Pick the "worst" predictor and remove it. 
#Explain your choice. (Note: you should look for a drop in the real value of AIC, not in the absolute value). 
#How has removing the predictor affected the R^2 values? 
drop1(linearModel, test="F")
#the worst predictor is Sleep_Quality, which is not significant and will decrease the AIC value
linearModel <- lm(Aggression~Television+Computer_Games+Sibling_Aggression+Diet+Parenting_Style, data=child_aggression)
summary(linearModel)
#First (mult.R-squared=0.08302,adj.R-squared=0.07467)
#Now (mult.R-squared=0.08258,adj.R-squared=0.07563)
#The R^2 values have increased, meaning that the model now explains more of the variance

###1f Continue checking for predictors you can remove and remove them. 
#Stop when there are no more predictors whose removal would improve the model. 
#What are the predictors in the final model? 
#What did you notice about the change in multiple R^2 as you removed predictors?
#What did you notice about the change in adjusted R^2? Why do you think this is?
drop1(linearModel, test="F")
#Remove Television
linearModel <- lm(Aggression~Computer_Games+Sibling_Aggression+Diet+Parenting_Style, data=child_aggression)
summary(linearModel)
#First (mult.R-squared=0.08258,adj.R-squared=0.07563)
#Now (mult.R-squared=0.08187,adj.R-squared=0.07631)
#The predictors in the final model are Aggression~Computer_Games+Sibling_Aggression+Diet+Parenting_Style
#When I remove Television as a predictor the mult. R^2 value went down while adj. R^2 went up

###WHEN YOU REACH HERE, RAISE YOUR HAND SO THAT YOUR TA CAN SIGN YOU OFF FOR THE DAY.

###2a Load in the data from Supermodel.dat. 
#This data contains information about supermodels salaries, age, years spent working, and beauty. 
#Build a multiple regression that predicts salary as a function of the other 3 variables. 
#Which predictors are significant? 
#Which are not? 
#How much of the variance in the data is explained?
supermodel <- read.table("Supermodel.dat",sep="\t",header=T,comment.char="",quote="")
head(supermodel)

linearModel <- lm(salary~age + years + beauty, data=supermodel)
summary(linearModel)

#beauty is not significant
#(mult.R-squared=0.184,adj.R-squared=0.1733)

###2b Does the model violate any assumptions?
#no autocorrelation, 
durbinWatsonTest(linearModel) #dw-test return no significant autocorrelation
#homoscedasticity, 
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plot(linearModel)
# There does seem to be homoscedasticity as the data points fan out a bit
#normally distributed residuals, 
#the residuals do not seem normal distributed as the tail deviates a lot from the line
#and check that there are no highly influential points.
#there is no highly influential point that has a Cook's distance of > 1
#multicollinearity
vif(linearModel) #age & years > 10
cor(supermodel[,1:4])
#high correlation between years and age


###2c What do you think the results in 2b mean for the variables "age" and "years"?
#age and years are the same values?


###3a Using the data from exercise 1, 
#create a new multiple regression with only Computer_Games and Parenting_Style as predictors (and Aggression as the outcome). 
#Create a visualization of the 2-dimensional regression surface. Use the geom_tile() in ggplot2.
linearModel <- lm(Aggression~Computer_Games + Parenting_Style, data=child_aggression)