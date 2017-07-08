library(car)
library(ggplot2)


###1a Load in babyskin.csv. This spreadsheet contains data on the orangeness of different babies' skin and how many cans of carrot baby food they eat each month. 
#Run a simple regression with orangeness as your outcome and carrots as your predictor. Discuss significance, amount of variance explained, and direction of relationship.
babyskin <- read.table("babyskin.csv",sep="\t",header=T,comment.char="",quote="")
head(babyskin)

linearModel <- lm(Orangeness~Carrots, data=babyskin)
summary(linearModel)

#Coefficients:
#                 Estimate    Std. Error  t value   Pr(>|t|)    
#(Intercept)      103.2869    11.1231     9.286     <2e-16 ***
#  Carrots        7.8537      0.3213      24.445    <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 78.72 on 198 degrees of freedom
#Multiple R-squared:  0.7511,	Adjusted R-squared:  0.7499 
#F-statistic: 597.6 on 1 and 198 DF,  p-value: < 2.2e-16

#The models formula is Orangeness~Carrots. 
#Carrots as a prediction is significant.
#The model is significant overall (F 1,198=597.6, p < 2.2e-16) and has a high variance explanation (mult.R-squared=0.7111,adj.R-squared=0.7499)
#All regression coefficients, as well as their SE, t-scores and p-values are provided in the appendix.

###1b Now check assumptions. 
#does violate the linearity assumption
ggplot(babyskin, aes(Carrots, Orangeness)) + geom_point()
#no autocorrelation, 
durbinWatsonTest(linearModel) #dw-test return no significant autocorrelation
#homoscedasticity, 
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plot(linearModel)
# There does not seem to be homoscedasticity as the data points do not fan out a bit
#normally distributed residuals, 
#the residuals do seem normal distributed as the data does not deviate a lot from the line
#and check that there are no highly influential points.
#there is no highly influential point that has a Cook's distance of > 1

###1c You should have noticed that the assumption of linearity was violated. 
#Apply a regression technique that's appropriate to use when your data is curved. 
#Is your new model significant? How has R^2 changed?
model_2 <- lm(Orangeness~poly(Carrots,2), data=babyskin)
summary(model_2)
#Amount of explained variance (R^2) increased from 0.7511 to 0.8128

###1d Plot a curved line that corresponds to your new regression on top of a scatterplot of the data points.
predict(model_2, newdata=data.frame(Carrots=c(10,20,30)))
babyskin$predicted_Orangeness <- predict(model_2)
head(babyskin)
ggplot(babyskin, aes(Carrots, Orangeness)) + geom_point() + geom_smooth(method="lm") + geom_line(aes(Carrots,predicted_Orangeness), colour="red")


###2a Load in healing.csv. This spreadsheet contains data on the time it takes for small cuts in the skin heal for people of different ages. 
#Run a simple linear regression, discuss significance, variance explained, and direction of relationship, 
#and check assumptions (HINT: your plot for checking the assumption of homoscedasticity is going to look weird, 
#but remember that the assumption is violated if there is a "fan shape" to the data points)
healing <- read.table("healing.csv",sep="\t",header=T,comment.char="",quote="")
head(healing)

linearModel <- lm(healing_rate~age, data=healing)
summary(linearModel)

#Coefficients:
#  Estimate     Std. Error t value    Pr(>|t|)    
#(Intercept)    7.374313   0.220436   33.45   <2e-16 ***
#  age         -0.052480   0.004717  -11.12   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.9526 on 98 degrees of freedom
#Multiple R-squared:  0.5581,	Adjusted R-squared:  0.5536 
#F-statistic: 123.8 on 1 and 98 DF,  p-value: < 2.2e-16

#The models formula is healing_rate~age 
#age as a prediction is significant.
#The model is significant overall (F 1,98=123.8, p < 2.2e-16) and has a high variance explanation (mult.R-squared=0.5581,adj.R-squared=0.5536)
#All regression coefficients, as well as their SE, t-scores and p-values are provided in the appendix.

#does violate the linearity assumption
ggplot(healing, aes(age, healing_rate)) + geom_point()
#no autocorrelation, 
durbinWatsonTest(linearModel) #dw-test return no significant autocorrelation
#homoscedasticity, 
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plot(linearModel)
# There does not seem to be homoscedasticity as the data points do not fan out a bit
#normally distributed residuals, 
#the residuals do seem normal distributed as the data does not deviate a lot from the line
#and check that there are no highly influential points.
#there is no highly influential point that has a Cook's distance of > 1


#2b As in 1, the assumption of linearity has been violated. 
#Again, you need to apply a regression technique that's appropriate to use when your data is curved. 
#(HINT: HOW MANY CURVES ARE THERE IN THIS DATA??) Is your new model significant? How has R^2 changed?
model_2 <- lm(healing_rate~poly(age,3), data=healing)
summary(model_2)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     5.16281    0.07245  71.259  < 2e-16 ***
#  poly(age, 3)1 -10.59784    0.72452 -14.627  < 2e-16 ***
#  poly(age, 3)2   1.28423    0.72452   1.773   0.0795 .  
#  poly(age, 3)3  -6.07394    0.72452  -8.383 4.36e-13 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.7245 on 96 degrees of freedom
#Multiple R-squared:  0.7496,	Adjusted R-squared:  0.7418 
#F-statistic:  95.8 on 3 and 96 DF,  p-value: < 2.2e-16

#The predicotors appears to be sigificant with the only not significant predictor having a low significant of 0.0797
#The model is significant overall (F 3,96 = 95.8, p < 2.2e-16)
#Amount of explained variance (R^2) increased from 0.5581 to 0.7245

###2c Plot a curved line that corresponds to your new regression on top of a scatterplot of the data points.
predict(model_2, newdata=data.frame(age=c(10,20,30)))
healing$predicted_healing_rate <- predict(model_2)
head(healing)
ggplot(healing, aes(age, healing_rate)) + geom_point() + geom_smooth(method="lm") + geom_line(aes(age,predicted_healing_rate), colour="red")


###3a Load in uscrime.csv. This spreadsheet contains data on U.S. crime statistics in 1960 for different U.S. states. 
#You are going to run a multiple regression (in 3b) to determine which variables predict crime. The variables in this spreadsheet include:
#Crime: Crime rate (offenses per 100,000 people)
#M: Percentage of males aged 14-24 in state population
#Ed: mean years of schooling of the population aged 25 or older
#Po1: per capita expenditure on police protection
#U2: unemployment rate of males 35-39
#Ineq: income inequality
#Prob: probability of imprisonment for crime
#LF: labor force participation rate of urban males 14-24
#M.F: number of males per 100 females
#NW: percentage nonwhites in population
#Time: average time in months served vy offenders in state prisons
UScrime <- read.table("uscrime.csv",sep="\t",header=T,comment.char="",quote="")
head(UScrime)

###3b Run a multiple regression with Crime as your outcome variable and all other variables as predictors. 
#Is the model significant? How much variance is explained in the sample? How much variance would we expect to be explained in the general population? Which predictors are significant?
linearModel <- lm(Crime~M+Ed+Po1+LF+M.F+NW+U2+Ineq+Prob+Time, data=UScrime)
summary(linearModel)

#Coefficients:
#                Estimate     Std. Error  tvalue    Pr(>|t|)    
#(Intercept)      -5549.136   1383.300    -4.012    0.000292 ***
#  M              95.100      40.549      2.345     0.024640 *  
#  Ed             168.424     58.961      2.857     0.007071 ** 
#  Po1            113.991     16.692      6.829     0.51e-08 ***
#  LF             234.612     1172.477    0.200     0.842529    
#M.F              9.765       16.046      0.609     0.546650    
#NW               1.789       5.583       0.321     0.750426    
#U2               82.664      50.240      1.645     0.108596    
#Ineq             63.036      16.374      3.850     0.000466 ***
#  Prob           -4169.393   2143.516    -1.945    0.059603 .  
#Time             -1.084      6.421       -0.169    0.866836    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 209 on 36 degrees of freedom
#Multiple R-squared:  0.7714,	Adjusted R-squared:  0.7079 
#F-statistic: 12.15 on 10 and 36 DF,  p-value: 8.027e-09

#The models formula is Crime~M+Ed+Po1+LF+M.F+NW+U2+Ineq+Prob+Time. 
#Not all the main effects were significant. M, Ed, Pol and Ineq are significant. LF,M.F,NW,U2,Prob and Time are not.
#The model is significant overall (F 10,36 = 12.15, p=8.027e-09) and has a high variance explanation (mult.R-squared=0.7714,adj.R-squared=0.7079)
#All regression coefficients, as well as their SE, t-scores and p-values are provided in the appendix.


###3c Check assumptions of the multiple regression technique (don't worry about linearity here).
#Is there a problem with multicollinearity in this model?
vif(linearModel) #avg=2.915987 = (2.733841+4.580025+2.590702+2.363404+2.353677+3.469670+1.895208+4.492696+2.500527+2.180123)/10 
cor(UScrime[,1:10])
#Yes there is a problem. Te avg is three times higher then 1 which is a sign for multicollinearity. 
#If we look at the cor of the predictors there also seems to be quite some correlation between some predictors.

#no autocorrelation, 
durbinWatsonTest(linearModel) #dw-test return no significant autocorrelation
#homoscedasticity, 

par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plot(linearModel)
# There does not seem to be homoscedasticity as the data points have a nice cloud shape
#normally distributed residuals, 
#the residuals do seem normal distributed as the tail and the head do not deviate a lot from the line
#and check that there are no highly influential points.
#there is no highly influential point that has a Cook's distance of > 1


###3d Now perform model selection, removing predictors until you arrive at a final model. 
#At each step, state which predictor you have chosen to remove and why. When you get to the final model, explain why you aren't removing any more predictors. How do multiple R^2 and adjusted R^2 change each time you remove a predictor? Why do you think you see this pattern?
linearModel <- lm(Crime~M+Ed+Po1+LF+M.F+NW+U2+Ineq+Prob+Time, data=UScrime)
summary(linearModel) #At start : Multiple R-squared:  0.7714,	Adjusted R-squared:  0.7079
drop1(linearModel, test="F") 
#Remove time as AIC decreases from 511.66 to 509.70 and Time has a significance of 0.866
linearModel <- lm(Crime~M+Ed+Po1+LF+M.F+NW+U2+Ineq+Prob, data=UScrime)
summary(linearModel) #Multiple R-squared:  0.7712,	Adjusted R-squared: 0.7156 
drop1(linearModel, test="F") 
#Remove LF as AIC decreases from 509.70 to 507.82 and LF has a significance of 0.86
linearModel <- lm(Crime~M+Ed+Po1+M.F+NW+U2+Ineq+Prob, data=UScrime)
summary(linearModel) #Multiple R-squared:  0.771,	Adjusted R-squared: 0.7228 
drop1(linearModel, test="F") 
#Remove NW as AIC decreases from 507.82 to 505.87 and LF has a significance of 0.745
linearModel <- lm(Crime~M+Ed+Po1+M.F+U2+Ineq+Prob, data=UScrime)
summary(linearModel) #Multiple R-squared:  0.7704,	Adjusted R-squared: 0.7291 
drop1(linearModel, test="F") 
#Remove M.F as AIC decreases from 505.87 to 504.79 and LF has a significance of 0.387499
linearModel <- lm(Crime~M+Ed+Po1+U2+Ineq+Prob, data=UScrime)
summary(linearModel) #Multiple R-squared:  0.7659,	Adjusted R-squared: 0.7307  
drop1(linearModel, test="F") #Removing more predictors would increase the AIC value and all the predictors are significant
#We see that Multiple R-squared goes down as we remove predictors bit Adjusted R-squared goes up. We get a better prediction so Adjusted R-squared goed up and because we use less predictors it follows that Multiple R-squared goed down. 

###3e Refer back to what each predictor variable name means as described in the instructions for exercise 3a. 
#Provide a description of the different societal factors that predict crime rate in U.S. states (what I mean is don't just list the variable names from the spreadsheet, 
#but actually say what they refer to and how they relate to crime rate). Describe the direction of the relationship between each of these factors and crime rate.

#Crime: Crime rate (offenses per 100,000 people)

#M: Percentage of males aged 14-24 in state population
#The relation is 105.0 which is avg compared to the other predictors. It appears that the Percentage of males aged 14-24 in state population is a decent predictor of crime.
#This would be the case if Males are more criminal then Females. 

#Ed: mean years of schooling of the population aged 25 or older 
#The relation is 196.47 which is high compared to the other predictors. This is a bit weird of a relation as you would expect that education in general reduces crime. 
#The reason could be that if people of 25 and older receive a lot of education it means they didnt had it when young, and this can mean that young people are doing crimes.

#Po1: per capita expenditure on police protection 
#The relation is 115.02 which is avg compared to the other predictors. This relation is also a bit weird as more money to the police should reduce crime. 
#The reason is probably that when there is high crime somewhere there is more money for the police to fight it. Then it if also often the case that where there is alot of money for the police
#it is probably for the high crimes that are there

#U2: unemployment rate of males 35-39
#The relation is 89.37 which is little below avg compared to the other predictors. Unemployemt is a logical predictor of crime, people do not have anything to do and want money

#Ineq: income inequality
#The relation is 67.65 which is below avg compared to the other predictors. This means that when rich people earn a lot and people in a lower class earn less there is more crime.
#This is also very reasonable. The poor people want money, and the rich have stuff they can steal, there might also even be a touch of sense of fairness.

#Prob: probability of imprisonment for crime
#The relation is -3801.84 which is a very high negative relation. This means that as the probability of imprisonment increases a little the crimes reduce much more. 
#This is expected, the higher the consequences the less easy people commit crimes


###3f The directions of several of the relationships you described in 3e make some sense. 
#However, a couple are very strange. First, it is strange that, as per capita expenditure on police protection increases, so does crime rate. 
#What does this relationship suggest about causality between predictors and outcome variables in observational studies like this one?
#A predictor is not nessesaraly a cause of the thing you want to predict. In this case its plausible that it is the other way around, crime causes more per capita expenditure on police protection.
#Because there is a relation per capita expenditure will predict crime to some extend

###3g Another strange relationship is the positive one between number of years of schooling in the population and crime rate. 
#Do you think there is a direct relationship between these two?
#No, there is higly likely a third variable that explains this relation



###4a load in mtcars.csv (set row.names=1 in the read.table function). You've worked with this data before; it describes characteristics of different cars. 
#Perform a multiple regression, predicting mpg (miles per gallon, which is a measure of fuel efficiency) based on the following predictors:
#cyl: number of cylinders in the engine (a cylinder is an engine component where power is created)
#disp: size of engine
#hp: horsepower (how powerful the engine is)
#wt: the weight of the car.
mtcars <- read.table("mtcars.csv",sep="\t",header=T,comment.char="",quote="",row.names = 1)
head(mtcars)

###4b Is the model significant? How much variance in the sample is explained? 
#How much variance in the population would we expect to be explained? Which predictors are significant?
linearModel <- lm(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, data=mtcars)
summary(linearModel)

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)  
#(Intercept) 12.30337   18.71788   0.657   0.5181  
#cyl         -0.11144    1.04502  -0.107   0.9161  
#disp         0.01334    0.01786   0.747   0.4635  
#hp          -0.02148    0.02177  -0.987   0.3350  
#drat         0.78711    1.63537   0.481   0.6353  
#wt          -3.71530    1.89441  -1.961   0.0633 .
#qsec         0.82104    0.73084   1.123   0.2739  
#vs           0.31776    2.10451   0.151   0.8814  
#am           2.52023    2.05665   1.225   0.2340  
#gear         0.65541    1.49326   0.439   0.6652  
#carb        -0.19942    0.82875  -0.241   0.8122  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2.65 on 21 degrees of freedom
#Multiple R-squared:  0.869,	Adjusted R-squared:  0.8066 
#F-statistic: 13.93 on 10 and 21 DF,  p-value: 3.793e-07

#The models formula is mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb. 
#None of the main effects were significant.
#The model is significant overall (F 10,21 = 13.93, p=3.793e-07) and has a high variance explanation (mult.R-squared=0.869,adj.R-squared=0.8066)
#All regression coefficients, as well as their SE, t-scores and p-values are provided in the appendix.


###4c Test the model assumptions. Are any of them violated?
#Is there a problem with multicollinearity in this model?
vif(linearModel) #avg=9.577414 = (15.373833+21.620241+9.832037+3.374620+15.164887+7.527958+4.965873+4.648487+5.357452+7.908747)/10 
cor(mtcars[,2:10])
#Yes there is a problem. Te avg is 9.5 times higher then 1 which is a sign for multicollinearity. And for cyl,disp,hp,wt the value is very close or over 10
#If we look at the cor of the predictors there also seems to be quite some correlation between some predictors.

#no autocorrelation, 
durbinWatsonTest(linearModel) #dw-test return no significant autocorrelation
#homoscedasticity, 
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plot(linearModel)
# There does not seem to be homoscedasticity as the data points have a nice cloud shape
#normally distributed residuals, 
#the residuals do seem normal distributed as the tail and the head do not deviate a lot from the line
#and check that there are no highly influential points.
#there is one highly influential point that has a Cook's distance of > 0.5 but not > 1


###4d Run the drop1() function on your regression model. 
#Which predictor would be the best one to drop? Which would be the second best to drop? Why?  (don't actually drop them!)
drop1(linearModel, test="F") 
#The best predictor to drop is cyl with an decrease of AIC from 70.989 to 68.915 and has a significance of 0.91609 
#The second best predictor to drop is vs with an decrease of AIC from 70.989 to 68.932 and has a significance of 0.88142

###4e Now you're going to run 2 simple regressions: one predicting mpg as a function of disp, and a second predicting mpg as a function of hp. Are these models significant?
linearModel <- lm(mpg~disp, data=mtcars)
summary(linearModel)

#Coefficients:
#               Estimate    Std. Error t value  Pr(>|t|)    
#(Intercept)    29.599855   1.229720    24.070  < 2e-16 ***
#  disp         -0.041215   0.004712    -8.747  9.38e-10 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 3.251 on 30 degrees of freedom
#Multiple R-squared:  0.7183,	Adjusted R-squared:  0.709 
#F-statistic: 76.51 on 1 and 30 DF,  p-value: 9.38e-10

#The models formula is mpg~disp 
#disp as a prediction is significant.
#The model is significant overall (F 1,30=76.51 p = 9.38e-10) and has a high variance explanation (mult.R-squared=0.7183)
#All regression coefficients, as well as their SE, t-scores and p-values are provided in the appendix.

linearModel <- lm(mpg~hp, data=mtcars)
summary(linearModel)

#Coefficients:
#               Estimate  Std. Error t value Pr(>|t|)    
#(Intercept)    30.09886    1.63392  18.421  < 2e-16 ***
#  hp           -0.06823    0.01012  -6.742 1.79e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 3.863 on 30 degrees of freedom
#Multiple R-squared:  0.6024,	Adjusted R-squared:  0.5892 
#F-statistic: 45.46 on 1 and 30 DF,  p-value: 1.788e-07

#The models formula is mpg~hp 
#hp as a prediction is significant.
#The model is significant overall (F 1,30=45.46 p = 1.788e-07) and has a high variance explanation (mult.R-squared=0.6024)
#All regression coefficients, as well as their SE, t-scores and p-values are provided in the appendix.

###4f How do disp and hp behave differently in 4d versus 4e? Why do you think this is? (consider what assumption was violated in 4c. It may also help to think back to partial correlations.)
#in 4e they were not significant predictors, in 4d they are. In 4c we discovered that there is a multicollinarity violation, there is a correlation between the variables themselves.
#The model is better without the "double" predictors.

