library(ggplot2)
library(car)
setwd("~/Documents/RU/Sem2/Statistics/Week7")



###1a Load in the data from trains.csv. 
#This file contains data on rainfall (in mm) and number of minutes that a train is delayed. 
#You are interested in predicting the train delays based on rainfall. 
#First create a scatterplot of the data.
trains = read.table("trains.csv", sep="\t", header=TRUE, comment.char="", quote="")
head(trains)

ggplot(trains, aes(rainfall, train_delays)) + geom_point()

###1b  Now, run a regression, and analyze the output of the regression.
linearModel <- lm(train_delays ~ rainfall, data = trains)
summary(linearModel)

#b0 has an estimate of 0.16 with a significance of p < 0.05
#b1 has an estimate of 0.97 with a significance of p < 2e-16 
#The F value has a p < 2.2-16 so that is significant
#We can assume this is a good model based on the t and F tests

###1c Now you are going to check model assumptions and discuss whether there are any violations/problems in the data. 
#First, discuss whether the data violates assumptions about variable types.

#The data does not violate assumptions because the value of both rainfall and train_delays are ratio values
###1d What about the linearity assumption? (Refer to the scatterplot you created above).

#The data does not seem to violate the linearity assumption. In the scatteplot we can clearly see a positive trend in the data


###1e Now you are going to test assumptions regarding residuals. 
#First, check whether your residuals are autocorrelated and discuss the results.

durbinWatsonTest(linearModel)
#The DW-test gives a value > 2 so there is indeed a positive correlation. 
#The p value is 0.324 which is > 0.05 so we significanly assume the is no autocorrelation

###1f Now you are going to check whether your residuals display homoscedasticity and whether they are normally distributed. 
#Use model diagnostic plots to do this.(When you create the plots, you may need to click "Zoom" to enlarge the plots so that they are readable).

par(mfrow = c(2,2))
plot(linearModel)
#The residuals are not completly normal distributed but the middle part falls on a diagonal line
#The homoscedasticity assumption is violated. In the plot there are much more datapoints towards the left and they also do not spread nicely vertically

###1g Still referring to the model diagnostic plots you just created, discuss whether there are any high leverage and/or highly influential points, and any problems they might be causing.

#There are no highly influential points, not datapoints have a Cook's distance of > 1
#The are some high-leverage points which are also outliers because they do not follow the trend of the data

###1h Use the model to predict the train delay for when there is 3.5 mm of rainfall

x = 3.5
prediction = 0.15579 + 0.96893*x; prediction
predict(linearModel, rainfall = 3.5)

###IF YOU REACH THIS POINT, YOU CAN RAISE YOUR HAND TO HAVE THE TA SIGN YOU OFF FOR THE DAY.


###2a Load in the data from buildings.csv. 
#This file contains data on how many floors different office buildings (their height) have and how many employees, on average, leave each building to go out for lunch.  
#You are interested in predicting whether employees leave based on the height of the building. 
#First create a scatterplot of the data.
buildings = read.table("buildings.csv", sep="\t", header=TRUE, comment.char="", quote="")
head(buildings)

ggplot(buildings, aes(height, employees_leave)) + geom_point()


###2b  Now, run a regression, and analyze the output of the regression.
linearModel <- lm(employees_leave ~ height, data = buildings)
summary(linearModel)

#b0 has an estimate of 186.83 with a significance of p < 2e-16
#b1 has an estimate of -2.37 with a significance of p < 2e-16 
#The F value has a p < 2.2-16 so that is significant
#We can assume this is a good model based on the t and F tests


###2c As in 1c through 1g, now check the model assumptions and discuss the results.


#The variable assumption gets violated because floors are not interval or ratio (ARE THEY?)
#The liniarity assumption holds as there is a negative trend
durbinWatsonTest(linearModel)
#The DW-test gives a value 1.97 which is almost > 2 so there is a positive correlation. 
#The p value is 0.868 which is > 0.05 so we significanly assume there is no autocorrelation
par(mfrow = c(2,2))
plot(linearModel)
#The residual error has a normal distribution
#The homoscedasticity assumption gets violated as there is a wider spread on the right side of the plot
#There are no highly influentual points, no points have a Cook's distance > 1
#There are no clear outliers of high-leverage points as the data is spread very wide.

###3a Load in the data from coffee.csv. 
#This file contains data on the age that different people began drinking coffee and how tall they end up growing to be. 
#You want to predict height as a function of the age variable. First create a scatterplot of the data.
coffee = read.table("coffee.csv", sep="\t", header=TRUE, comment.char="", quote="")
head(coffee)

ggplot(buildings, aes(age_coffee, height_in_cm)) + geom_point()

###3b Now, run a regression, and analyze the output of the regression.
linearModel <- lm(height_in_cm ~ age_coffee, data = coffee)
summary(linearModel)


###3c Given the overall fit/significance level of the model, what might be suprising about the significance level of the slope? (We will discuss this phenomenon next week)

