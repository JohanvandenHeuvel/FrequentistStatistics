library(ggplot2)
library(car)

setwd("~/Documents/RU/Sem2/Statistics/Week7")


###1 Load in the data from Idiom_use_data.csv. 
#This spreadsheet contains information regarding Dutch idioms--word sequences such as "tegen de lamp lopen", which have both a literal meaning (walk into the lamp) and a figurative one (get caught). 
#Specifically, participants in a study provided ratings for how frequent/common they thought the idioms were, and how familiar they were with the figurative meanings of the idioms. 
#Your job is to graph the data and perform a linear regression on it, predicting familiarity as a function of frequency. 
#What do the results tell you about the relationship between familiarity and frequency ratings? 
#Make sure to check model assumptions and look for high-leverage and highly influential points. 
#Are there any problems with these things?
idiom = read.table("Idiom_use_data.csv", sep="\t", header=TRUE, comment.char="", quote="")
head(idiom)
summary(idiom)

linearModel <- lm(Familiarity ~ Frequency, data = idiom)
summary(linearModel)
#b0 = -0.08 with p = 0.102
#b1 = 0.93 with p < 2e-16
#the F-test has a p < 2.2e-16
#Both the test for b1 and the F-test return a p < 0.05 so we do have some evidence this models is a good fit

#ASSUMPTIONS
ggplot(idiom, aes(Frequency, Familiarity)) + geom_point()
#the data has a linear shape; the linearity assumption is not violated

#the data has ratio/interval variables and does not use cathegories. Therefore the variable assumption is not violated

durbinWatsonTest(linearModel)
#dw-test has a p-value of <0.05 so we can assume that there is significant autocorrelation

par(mfrow = c(2,2))
plot(linearModel)
#the data is alsmost normal distributed. At the start of the line it violated normality and at the end there is a small deviation from the line.
#the homoscedascity assumption gets violated a little bit as there are more data points to the right then to the left
#there are no highly influential points, not datapoints have a Cook's distance of > 1
#the are some high-leverage points, but no outliers as the they do not deviate a lot from the general trend of the data


###2 Load in the data from Science_fair.csv. 
#This spreadsheet contains the results from a competition at a school science fair. 
#Students were awarded first (winner) through fifth places for their science fair projects. 
#Many students can "tie" (be awarded the same place). 
#Again, plot the data and perform a regression predicting students' places as a function of how much time they spent preparing their project. 
#Discuss the results. 
#You should also examine whether this data and model violate any assumptions, and whether there are problematic data points.
science_fair = read.table("Science_fair.csv", sep="\t", header=TRUE, comment.char="", quote="")
head(science_fair)
summary(science_fair)

linearModel2 <- lm(Prize ~ Preparation, data = science_fair)
summary(linearModel2)
#b0 = 4.77 with p = 3.15e-16
#b1 = 0.93 with p = 3.05e-9
#the F-test has a p = 3.055e-09
#the tests for b1,b2 and the F-test return a p < 0.05 so we do have evidence this models is a good fit

#ASSUMPTIONS
ggplot(science_fair, aes(Preparation, Prize)) + geom_point()
#the data does not have a linear shape, there is a big deviation from the trend in the bottom, right corner; the linearity assumption is violated

#the data has a cathegorical variable, Prize. Therefore the variable assumption is violated

durbinWatsonTest(linearModel2)
#dw-test has a p = 0 so we can assume that there is significant autocorrelation

par(mfrow = c(2,2))
plot(linearModel2)
#the data is almost normal distributed. At the start of the line it violated normality and the datapoints do not line up perfectly.
#the homoscedascity assumption gets violated a little bit as there are more data points to the right then to the left
#there are no highly influential points, not datapoints have a Cook's distance of > 1
#the are some high-leverage points and outliers as the they seem to deviate from the general trend of the data











###3 Load in the data from mtcars.csv. 
#This spreadsheet contains various pieces information about different cars, from engine size to fuel efficiency to performance. 
#We are interested in the variables "hp" (horsepower--that is, how powerful a car's engine is) and "disp" (displacement--that is, how large the engine is). 
#Plot these two variables against each other and perform a regression predicting horsepower as a function of displacement. 
#Discuss the results. 
#Also check assumptions. Are any violated? Are there problematic data points?
cars = read.table("mtcars.csv", sep="\t", header=TRUE, comment.char="", quote="")
head(cars)
summary(cars)

linearModel3 <- lm(hp ~ disp, data = cars)
summary(linearModel3)
#b0 = 45.74 with p = 0.00811
#b1 = 0.44 with p = 7.14e-08
#the F-test has a p < 7.143e-08
#the test for b1,b2 and the F-test return a p < 0.05 so we do have evidence this model is a good fit

#ASSUMPTIONS
ggplot(cars, aes(disp, hp)) + geom_point()
#the data has a broad linear shape; the linearity assumption is not violated, as there is not 'curvy' trend

#the data has ratio/interval variables and does not use cathegories. Therefore the variable assumption is not violated

durbinWatsonTest(linearModel3)
#dw-test has a p-value of 0.05 so we can assume that there is no significant autocorrelation

par(mfrow = c(2,2))
plot(linearModel3)
#the data is alsmost normal distributed. At the end there is a deviation from the line.
#the homoscedascity assumption does not get violated
#there are no highly influential points, not datapoints have a Cook's distance of > 1
#the are some high-leverage points, but no outliers as the they do not deviate a lot from the general trend of the data







###4 Using the same data as is 3, now perform a regression predicting displacement as a function of horsepower. 
#How is this model different from the one in 3? How is it the same? 
#What would be the predicted displacement of an engine that outputs 195 horsepower?
linearModel4 <- lm(disp ~ hp, data = cars)
summary(linearModel4)
#b0 = 20.99 with p = 0.525
#b1 = 1.43 with p = 7.14e-08
#the F-test has a p < 7.143e-08
#the test for b2 and the F-test return a p < 0.05 so we do have some evidence this model is a good fit

#ASSUMPTIONS
ggplot(cars, aes(hp, disp)) + geom_point()
#the data has a broad linear shape; the linearity assumption is not violated, as there is not 'curvy' trend

#the data has ratio/interval variables and does not use cathegories. Therefore the variable assumption is not violated

durbinWatsonTest(linearModel4)
#dw-test has a p-value of <0.05 so we can assume that there is significant autocorrelation

par(mfrow = c(2,2))
plot(linearModel4)
#the data is not normal distributed. There is some sort of wave going on
#the homoscedascity assumption gets violated as there are more data points to the left then to the right
#there are is a highly influential point that has a Cook's distance of > 1
#the are some high-leverage points, but no outliers as the they do not deviate a lot from the general trend of the data

x = 195
20.99+1.43*x #299.84







###5 Load in the data from Baseball.csv. 
#This spreadsheet contains various pieces of information about different players from this slow and boring American sport. 
#We are interested in two variables: "homeruns" (how many times they hit the ball beyond the outfield) and "hits" (total number of times hitting the ball). 
#Graph these two variables against each other and then run a regression predicting homeruns as a function of hits. Discuss the results. 
#Then, check assumptions and whether there are problematic data points.
baseball = read.table("Baseball.csv", sep="\t", header=TRUE, comment.char="", quote="")
head(baseball)
summary(baseball)

linearModel5 <- lm(homeruns ~ hits, data = baseball)
summary(linearModel5)
#b0 = -4.6 with p = 0.302
#b1 = 0.1 with p < 2e-16
#the F-test has a p < 2.2e-16
#the test for b2 and the F-test return a p < 0.05 so we do have some evidence this model is a good fit

#ASSUMPTIONS
ggplot(baseball, aes(hits, homeruns)) + geom_point()
#the data has a broad curved shape; the linearity assumption is violated, as there is a 'curvy' trend

#the data has ratio/interval variables and does not use cathegories. Therefore the variable assumption is not violated

durbinWatsonTest(linearModel5)
#dw-test has a p-value of <0.05 so we can assume that there is significant autocorrelation

par(mfrow = c(2,2))
plot(linearModel5)
#the data is alsmost normal distributed. At the start of the line it violated normality and at the end there is a deviation from the line.
#the homoscedascity assumption gets violated as there are much more data points to the left then to the right
#there is a highly influential point that has a Cook's distance of > 1
#the are some high-leverage points, but no outliers as the they do not deviate a lot from the general trend of the data


###6 Load in the data from Tooth_decay.csv. 
#This spreadsheet contains data about rate of tooth decay for different people, and how much toothpaste they typically use (in ml). 
#Graph the data and perform a regression that predicts tooth decay as a function of how much toothpaste is used by different people. 
#Examine assumptions. Are there any problematic data points? If there are, try removing them, perform the regression again, and discuss any changes in the results of the regression.
tooth_decay = read.table("Tooth_decay.csv", sep="\t", header=TRUE, comment.char="", quote="")
head(tooth_decay)
summary(tooth_decay)

linearModel6 <- lm(decay ~ toothpaste, data = tooth_decay)
summary(linearModel6)
#b0 = 4.62 with p = 6.19e-09
#b1 = -0.59 with p = 0.00835
#the F-test has a p < 0.008349
#the test for b1,b2 and the F-test return a p < 0.05 so we do have some evidence this model is a good fit

#ASSUMPTIONS
ggplot(tooth_decay, aes(toothpaste, decay)) + geom_point()
#the data has a broad cloud shape; the linearity assumption is violated, as there is no clear linear trend

#the data has ratio/interval variables and does not use cathegories. Therefore the variable assumption is not violated

durbinWatsonTest(linearModel6)
#dw-test has a p-value of 0.978 so we can assume that there is no significant autocorrelation

par(mfrow = c(2,2))
plot(linearModel6)
#the data is not normal distributed. There is some sort of wave going on.
#the homoscedascity assumption gets violated as there are much more data points to the right then to the left
#there is a highly influential point that has a Cook's distance of > 1
#the is a high-leverage point which is not a outlier as the point does not deviate from the general trend of the data


sorted <- sort(tooth_decay$toothpaste)
x <- sorted[length(sorted)]
tooth_decay <- subset(tooth_decay, toothpaste != x)

ggplot(tooth_decay, aes(toothpaste, decay)) + geom_point()
#the data is all over the place, no trend at all

linearModel7 <- lm(decay ~ toothpaste, data = tooth_decay)
summary(linearModel7)
#b0 = 3.7 with p = 1.08e-05
#b1 = -0.067 with p = 0.829
#the F-test has a p < 0.8293
#only the test for b1 returns a p < 0.05 so we do have reason to believe this models is not a good fit at all

#ASSUMPTIONS
ggplot(tooth_decay, aes(toothpaste, decay)) + geom_point()
#the data has a broad cloud shape; the linearity assumption is violated, as there is no clear linear trend

#the data has ratio/interval variables and does not use cathegories. Therefore the variable assumption is not violated

durbinWatsonTest(linearModel7)
#dw-test has a p-value of 0.52 so we can assume that there is no significant autocorrelation

par(mfrow = c(2,2))
plot(linearModel7)
#the data is normal distributed. There is a bit of deviation but not too much
#the homoscedascity assumption does not get violated as there seems to be an even spread of data points
#there is no highly influential point that has a Cook's distance of > 1
#the are no high-leverage points or outliers


