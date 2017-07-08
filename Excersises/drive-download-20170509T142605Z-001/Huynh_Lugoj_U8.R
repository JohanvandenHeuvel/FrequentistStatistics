library(car)
library(ggplot2)
library(lmtest)

rm(list=ls(all=TRUE))
###1a Load in babyskin.csv. This spreadsheet contains data on the orangeness of different babies' skin and how many cans of carrot baby food they eat each month. Run a simple regression with orangeness as your outcome and carrots as your predictor. Discuss significance, amount of variance explained, and direction of relationship.
Babyskin = read.table(file.choose(), header=TRUE, sep="\t", quote="", comment.char="", fill=TRUE)

Babyskin_graph = ggplot(Babyskin, aes(Carrots, Orangeness))
Babyskin_graph + geom_point() + geom_smooth(method = "loess", alpha = 0.1)

Regression_Babyskin = lm(Orangeness~Carrots, data = Babyskin)
summary(Regression_Babyskin)

#Explanation: 
#The p-value is 2.2e-16 which is smaller than our alpha of 0.05, thus this data is statistically significant
#The R^2 value is 0.75, meaning 75% of the variability is explained by the model
#The direction of the line is positive as you can see in the scatterplot and the linear equation is: y = 103.3 + 7.85 * x
#Indicating a positive relationship

###1b Now check assumptions
durbinWatsonTest(Regression_Babyskin)
dwtest(Regression_Babyskin)

par(mfrow	=	c(2,2))	
par(mar	=	c(4.25,4.25,4.25,4.25))	
plot(Regression_Babyskin)

#Exlanation: 

#First we check variable types. The variable orangeness indicates how orange the skin is. 0 orangeness means the skin isn't orange, #you also can't have negative orangeness either and 4 orangeness is twice as much as 2 orangeness, thus orangeness is a ratio variable #Carrots is also a ratio type variable, because negative amount of carrots doesn't exist, 0 carrots means no carrots and 4 carrots are #twice as much as 2 carrots. So this assumption holds

#Assumption of linearity does not hold, because we can see in our scatterplot that the begin values of carrots are all over the place, some are beneath the line and some are under it. Also the line is curved. (usage of method loess, instead of lm)

#Assumption of independence holds, because according to Blackboard we can assume it holds

#Assumption of residuals: 

#For autocorrelation we can see that our durbin watson test shows us a p-value of 0.82 so the data is statistically non significant meaning there is no auto correlation. The D-W statistic value is 1.9688 which is below 2, meaning the residuals are positively correlated.

#For homoscedasticity we look at the residuals vs fitted and scale-location graphs who are both reasonably fitting the assumption of homoscedasticity. 

#For normally distributed we can see that the normal Q-Q is also showing a normally distributed dataset

#So our only assumption that is violated is that of linearity


###1c You should have noticed that the assumption of linearity was violated. Apply a regression technique that's appropriate to use when your data is curved. Is your new model significant? How has R^2 changed?
Regression_babyskin_new = lm(Orangeness~poly(Carrots,2), data = Babyskin)
summary(Regression_babyskin_new)

#Explanation:
#The new regression model is still statistically significant with a p-value of 2.2e-16
#R^2 now has a value of 0.81, so 81% of the variance is now explained by our model instead of the 75% we got in our previous model.


###1d Plot a curved line that corresponds to your new regression on top of a scatterplot of the data points.

Babyskin$Prediction = predict(Regression_babyskin_new)

ggplot(Babyskin, aes(Carrots, Orangeness)) + geom_point() + geom_smooth(method="lm", alpha = 0.1) + geom_line(aes(Carrots,Prediction), colour="red")


###2a Load in healing.csv. This spreadsheet contains data on the time it takes for small cuts in the skin heal for people of different ages. Run a simple linear regression, discuss significance, variance explained, and direction of relationship, and check assumptions (HINT: your plot for checking the assumption of homoscedasticity is going to look weird, but remember that the assumption is violated if there is a "fan shape" to the data points)
Healing = read.table(file.choose(), header=TRUE, sep="\t", quote="", comment.char="", fill=TRUE)

Healing_graph = ggplot(Healing, aes(age, healing_rate))
Healing_graph + geom_point() + geom_smooth(method = "loess", alpha = 0.1)

Regression_Healing = lm(healing_rate~age, data = Healing)
summary(Regression_Healing)

durbinWatsonTest(Regression_Healing)
dwtest(Regression_Healing)

par(mfrow	=	c(2,2))	
par(mar	=	c(4.25,4.25,4.25,4.25))	
plot(Regression_Healing)

#Explanation: Regression model
#Our p-value is 2.2e-16 so the data is statistically significant
#The R^2 value is 0.56, so 56% of the variance is explained by our model, which is quite low. 
#The direction of the data is negative. In the scatterplot we can see that there is a negative relationship although the line itself
#is not linear. The equation is y = 7.4 - 0.05 * x, this shows us that the data indeed has a negative relationship.

#Explanation: Assumptions
#Assumption of variables
#Both variables are ratio type variables so there is no problem with this assumption. Both can't be lower than 0, 0 means 0 and an age of 10 means you're twice as old as someone who is 5. Healing rate of 10 is also twice as big as a healing rate of 5.

#Assumption of linearity
#This assumption is violated, because the scatterplot shows us that the data is heavily curved towards the end and beginning

#Assumption of independence
#Holds because according to Blacboard we can assume it's independent

#Assumption of residuals
#For auto correlation we can see in the Durbin Watson test that the p-value is 0.22 so the data is non significant, meaning there is no auto correlation. The D-W statistic value is 2.26, indicating that the residuals are negatively correlated, which is true
#For homoscedasticity we look at the residuals vs fitted and scale-location graphs who should both be reasonably fitting for the  #assumption of homoscedasticity to hold. In this case we can see that scale-location fits nicely but residuals vs fitted has a weird
#pattern, however because there is not really a fan shape we can assume homoscedasticity is not violated.
#It's however quite hard to see, because the data isn't linear.
#For normally distributed we can see that the normal Q-Q plot shows us a nice line, only the tail and head are a bit curved but that doesn't matter.


#2b As in 1, the assumption of linearity has been violated. Again, you need to apply a regression technique that's appropriate to use when your data is curved. (HINT: HOW MANY CURVES ARE THERE IN THIS DATA??) Is your new model significant? How has R^2 changed?
Regression_Healing_new = lm(healing_rate~poly(age,3), data = Healing)
summary(Regression_Healing_new)

#Explanation:
#Because there are 2 curves now we use the power of 3.
#The data is still statistically significant with a p-value of 2.2e-16
#The R^2 variance did increase to 0.75, so 75% of the variance is explained by the model, instead of the 56% of the 
#simple regression model, which is quite a lot.


###2c Plot a curved line that corresponds to your new regression on top of a scatterplot of the data points.

Healing$Prediction = predict(Regression_Healing_new)
ggplot(Healing, aes(age,healing_rate)) + geom_point() + geom_smooth(method="lm", alpha = 0.1) + geom_line(aes(age,Prediction), colour="red")


###3a Load in uscrime.csv. This spreadsheet contains data on U.S. crime statistics in 1960 for different U.S. states. You are going to run a multiple regression (in 3b) to determine which variables predict crime. The variables in this spreadsheet include:
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
Crime = read.table(file.choose(), header=TRUE, sep="\t", quote="", comment.char="", fill=TRUE)

#To start things off this data set is so incredibly bad, it only has 47 data points, which is not representative at all, especially
#because this is for America which is a big country.

###3b Run a multiple regression with Crime as your outcome variable and all other variables as predictors. Is the model significant? How much variance is explained in the sample? How much variance would we expect to be explained in the general population? Which predictors are significant?
MultipleReg_crime = lm(Crime~M + Ed + Po1 + U2 + Ineq + Prob + LF + M.F + NW + Time, data = Crime)
summary(MultipleReg_crime)

#Explanation:
#The p-vale is 8.027e-09 which is smaller than our alpha of 0.05, thus the model is significant.
#Multiple R^2 value is 0.7714, so 77.1% of the variance is explained by the model in the sample
#Adjusted R squared value is 0.708, so 70.8% of the variance is explained by the model for the general population, it's lower because
#having multiple predictors penalizes you.
#The predictors with a star are significant, they are below our alpha of 0.05. The significant predictors are: M, Ed, Po1 and Ineq
#Prob is almost significant with a value of 0.0596 it's just above 0.05.


###3c Check assumptions of the multiple regression technique (don't worry about linearity here).
vif(MultipleReg_crime)
Average = ((2.73 + 4.58 + 1.90 + 4.50 + 2.50 + 2.36 + 2.35 + 3.47 + 2.18) / 9); Average

durbinWatsonTest(MultipleReg_crime)

par(mfrow	=	c(2,2))	
par(mar	=	c(4.25,4.25,4.25,4.25))	
plot(MultipleReg_crime)

#Explanation:
#Assumption of multicollinearity holds, even though all values are substantially bigger than 1, (however none of them are
#bigger than 10). The average is also 2.95 which means multicollinearity isn't that high. We do however have to use the cor function #to check which pairwise combinations of predictors are collinear, if Pearson's r is bigger than 0.8 it means the two predictors are #highly correlated. However we can see that no predictor pairs are above 0.8
#Thus no combinations of predictors are collinear. This is why we think the assumption holds, because multicollinearity is not extremely high.
cor(Crime)

#Explanation: assumption of variable types and independence
#All the variables are ratio type variables so this assumption holds
#We can also assume the data is independent according to Blackboard

#Explanation: Assumption of residuals
#The durbin watson test gives us a p-value of 0.824, thus the data is non significant so there is no auto correlation
#The d-w statistic value is 1.93 so that means the residuals are positively correlated
#For homoscedasticity we can see that there is a sort of fan shape in both residuals vs fitted and scale-location, so I think
#the assumption is being violated here.
#As for the normal Q-Q plot it's reasonably close to a nice normal linear line so the assumption of normally distributed holds.

###3d Now perform model selection, removing predictors until you arrive at a final model. At each step, state which predictor you have chosen to remove and why. When you get to the final model, explain why you aren't removing any more predictors. How do multiple R^2 and adjusted R^2 change each time you remove a predictor? Why do you think you see this pattern?
drop1(MultipleReg_crime, test = "F")
Regression_CrimeV2 = lm(Crime~M + Ed + Po1 + U2 + Ineq + Prob + LF + M.F + NW, data = Crime)
summary(Regression_CrimeV2)

#Drop Time, because it has the largest drop in AIC and is the biggest non-significant predictor.
#The removal of Time has caused multiple R^2 value to become slightly smaller with 0.7712 instead of 0.7714, this is because
#having multiple predictors eats up the variance (it increases). Adjusted R^2 however did go up from 0.7079 to 0.7156, this is because
#Having multiple predictors penalizes your adjusted R^2 value and now that we removed one it goes up.

drop1(Regression_CrimeV2, test = "F")
Regression_CrimeV3 = lm(Crime~M + Ed + Po1 + U2 + Ineq + Prob + M.F + NW, data = Crime)
summary(Regression_CrimeV3)
#Drop LF, because it has the largest drop in AIC and is the biggest non-significant predictor.
#The removal of LF has caused multiple R^2 value to become 0.771 which is slightly smaller than the previous one of 0.7712
#This is because having multiple predictors eats up the variance (it increases). Adjusted R^2 however did go up from 0.7156 to 0.7228
#This is because Having multiple predictors penalizes your adjusted R^2 value and now that we removed one it goes up.

drop1(Regression_CrimeV3, test = "F")
Regression_CrimeV4 = lm(Crime~M + Ed + Po1 + U2 + Ineq + Prob + M.F, data = Crime)
summary(Regression_CrimeV4)
#Drop NW, because it has the largest drop in AIC and is the biggest non-significant predictor.
#The removal of NW has caused multiple R^2 value to become 0.7704 which is slightly smaller than the previous one of 0.771
#This is because having multiple predictors eats up the variance (it increases). Adjusted R^2 however did go up from 0.7228 to 0.7291
#This is because Having multiple predictors penalizes your adjusted R^2 value and now that we removed one it goes up.

drop1(Regression_CrimeV4, test = "F")
Regression_CrimeV5 = lm(Crime~M + Ed + Po1 + U2 + Ineq + Prob, data = Crime)
summary(Regression_CrimeV5)
#Drop M.F, because it has the largest drop in AIC and is the biggest non-significant predictor.
#The removal of NW has caused multiple R^2 value to become 0.7659 which is smaller than the previous one of 0.7704
#This is because having multiple predictors eats up the variance (it increases). Adjusted R^2 however did go up from 0.7291 to 0.7307
#This is because Having multiple predictors penalizes your adjusted R^2 value and now that we removed one it goes up.

#Regression_CrimeV5 is our final model, because the AIC value doesn't drop for any remaining predictors and they're all significant as well
#However we haven't looked at whether the predictors make theoretical sense yet, but we believe we have to do this at 3e-f

###3e Refer back to what each predictor variable name means as described in the instructions for exercise 3a. Provide a description of the different societal factors that predict crime rate in U.S. states (what I mean is don't just list the variable names from the spreadsheet, but actually say what they refer to and how they relate to crime rate). Describe the direction of the relationship between each of these factors and crime rate.

#Crime: Crime rate (offenses per 100,000 people)

#M: Percentage of males aged 14-24 in state population
#Ed: mean years of schooling of the population aged 25 or older
#Po1: per capita expenditure on police protection
#U2: unemployment rate of males 35-39
#Ineq: income inequality
#Prob: probability of imprisonment for crime

#These are our remaining predictors and Crime is the outcome. 

#M related to Crime 
#In the scatterplot we can see that if you're younger your crime rate increases, this makes sense because young people are more easily
#influenced by others and are still young and stupid so it makes theoretical sense that they would have a higher crime rate as shown
#in the scatterplot
Crime_M_graph = ggplot(Crime, aes(M, Crime))
Crime_M_graph + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

#Ed related to Crime
#In the scatterplot we can see that if your mean years of schooling increases then so does your crime rate, but this makes no sense
#If your mean years of schooling increases then you'd expect your crime rate to drop, because if you're better schooled you obviously
#don't really have a reason to commit crimes under normal circumstances.
Crime_Ed_graph = ggplot(Crime, aes(Ed, Crime))
Crime_Ed_graph + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

#Po1 related to Crime
#In the scatterplot we can see a positive relationship between increased capita for police protection and crime. This again makes no
#sense at all. If you invest in more police protection then why would crime rate increase?
Crime_Po1_graph = ggplot(Crime, aes(Po1, Crime))
Crime_Po1_graph + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

#U2 related to Crime
#In the scatterplot we can see that there is a positive relationship between unemployment rate and crime rate. This does make 
#theoretical sense, because if you're unemployed and relatively old (35-39) then it would make sense to resort to crimes. 
Crime_U2_graph = ggplot(Crime, aes(U2, Crime))
Crime_U2_graph + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

#Ineq related to Crime
#In the scatterplot we can see that there is a negative relationship between in equality of income and crime rate, but this makes
#no sense at all. Why would a higher inequality of income result in lower crime? Wouldn't you be more aggravated because of the 
#inequality of income and resort to crime, like stealing money from the rich elite.
Crime_Ineq_graph = ggplot(Crime, aes(Ineq, Crime))
Crime_Ineq_graph + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

#Prob related to Crime
#In the scatterplot we can see that there is a negative relationship between the probability of imprisonment of a crime and crime rate
#If the probability rises then crime rate decreases, this makes theoretical sense, because if the probability is high then crime rate
#drops because you don't want to end up in jail. When the probability is low then crime rate increases because you're less likely
#to end up in jail
Crime_Prob_graph = ggplot(Crime, aes(Prob, Crime))
Crime_Prob_graph + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

###3f The directions of several of the relationships you described in 3e make some sense. However, a couple are very strange. First, it is strange that, as per capita expenditure on police protection increases, so does crime rate. What does this relationship suggest about causality between predictors and outcome variables in observational studies like this one?

#Explanation:
#As noted in 3e, the relationship between having spent more capita into police protection and crime rate makes no sense. This implies #that the causality between the outcome Crime rate and predictor Po1 does not exist. However I do think there is a causality between #these two variables. If you spend more money on police protection then it means criminals have less of a chance to commit crimes, #thus crime rate will drop. It's just that this data is weird and it only has 47 data points which isn't even near enough.
#This also a reason to drop the predictor fromour multiple regression model.

###3g Another strange relationship is the positive one between number of years of schooling in the population and crime rate. Do you think there is a direct relationship between these two?

#Explanation:
#As noted in 3e, the relationship between the mean years of schooling and crime rate is positive and makes no sense therefore. 
#It basically says that if you went to school for more years then your crime rate increases but this isn't right. I do think there's a #direct relationship between the two. Namely if you drop out of school, it means you're less well educated than your peers, which can #cause you to commit crimes simply because you're less educated and more prone to peer pressure and less able to think for yourself. On the other hand if you followed college and got a bachelor degree you have almost no reason to ever commit a crime. It's just that the data is very weird and it only has 47 data points which isn't even near enough.


###4a load in mtcars.csv (set row.names=1 in the read.table function). You've worked with this data before; it describes characteristics of different cars. Perform a multiple regression, predicting mpg (miles per gallon, which is a measure of fuel efficiency) based on the following predictors:
#cyl: number of cylinders in the engine (a cylinder is an engine component where power is created)
#disp: size of engine
#hp: horsepower (how powerful the engine is)
#wt: the weight of the car.
Mtcars = read.table(file.choose(), header=TRUE, sep="\t", quote="", comment.char="", fill=TRUE, row.names = 1)

MultipleReg_Mtcars = lm(mpg~cyl + disp + hp + wt, data = Mtcars)
summary(MultipleReg_Mtcars)

###4b Is the model significant? How much variance in the sample is explained? How much variance in the population would we expect to be explained? Which predictors are significant?

#Explanation:
#The model has a p-value of 1.061e-10, which is lower than our alpha is 0.05 so the model is significant.
#Multiple R^2 indicates that we have a variance of 0.8486, so 85% of the variance is explained by the model for this sample
#Adjusted R^2 indicates that we have a variance of 0.8262, so 83% of the variance is explained by the model for the population
#Only the predictor wt is significant with 0.000759. The rest is above our alpha of 0.05

###4c Test the model assumptions. Are any of them violated?

#Explanation: Assumption of multicollinearity
#It looks like the assumption is being violated, because the vif value of disp is above 10. Using the cor function we look whether two
#predictors are highly correlated. If Pearson's r > 0.8 they're highly correlated.
#Cyl and Hp are > 0.8 so it's highly correlated. Cyl and disp > 0.8 so also highly correlated. Cyl and wt < 0.8 so not highly #correlated (value is 0.78 though so really close). Disp with hp < 0.8 so not highly correlated (value of 0.79 so close).           #Disp with hp > 0.8 so highly correlated. Hp and wt < 0.8 so not highly correlated (value 0.66 so getting close)
#So yes the assumption is being violated, because the values are substantially bigger than 1 and one of the values is even bigger than #10. Also the cor indicates that most. Furthermore the tolerance level of cyl is 0.14, disp=0.09, hp=0.29,wt=0.21 This means that 
#cyl indicates a potential problem and disp indicates a serious problem.

vif(MultipleReg_Mtcars)
cor(Mtcars)
1/vif(MultipleReg_Mtcars)

#Explanation: Assumption of variable types and dependence
#All the variables are ratio types so this assumption holds
#According to blackboard we may assume that the data is dependent as well

#Explanation of Residuals
#The durbin watson test p-value is 0.214 so it's non significant thus there is no auto correlation. The d-w value is 1.69, indicating
#that the residuals are positively correlated. So it holds
#The assumption of homoscedasticity holds as well, because there is no fan shape in the plots of residuals vs fitted or scale-location
#The data is also normally distributed, because the data points form a nice linear line. 
durbinWatsonTest(MultipleReg_Mtcars)

par(mfrow	=	c(2,2))	
par(mar	=	c(4.25,4.25,4.25,4.25))	
plot(MultipleReg_Mtcars)
###4d Run the drop1() function on your regression model. Which predictor would be the best one to drop? Which would be the second best to drop? Why?  (don't actually drop them!)
drop1(MultipleReg_Mtcars, test = "F")

#Explanation:
#Drop disp first, because its AIC value has the biggest drop and it's non significant value is also the biggest.
#After that we drop hp, because its AIC value is the 2nd biggest drop and same holds for its non significance value.

###4e Now you're going to run 2 simple regressions: one predicting mpg as a function of disp, and a second predicting mpg as a function of hp. Are these models significant?

#Explanation: Mpg and disp
#The p-value is 9.38e-10 which is lower than our alpha of 0.05, so the model is significant
Regression_mpg_disp = lm(mpg~disp, data = Mtcars)
summary(Regression_mpg_disp)

#Explanation: Mpg and hp
#The p-value is 1.788e-07 which is lower than our alpha of 0.05, so the model is also significant
Regression_mpg_hp = lm(mpg~hp, data = Mtcars)
summary(Regression_mpg_hp)

###4f How do disp and hp behave differently in 4d versus 4e? Why do you think this is? (consider what assumption was violated in 4c. It may also help to think back to partial correlations.)

#Explanation:
#In 4d we drop disp first and hp second, because they're both non significant.
#In 4e we can see that they're both significant now when we run a simple regression
#I think this is because disp violated multicollinearity quite extremely with a vif-value of 10 and its tolerance was below 0.1
#hp also violates it but its vif-value is only substantially bigger than 1 and its tolerance was 0.14. This means that because they
#violated the assumption of multicollinearity, the predictors are correlated with each other, making the data biased and unrepresentative. 
#This is why you now put them in a simple regression, so you can see their true worth (they're both significant) .This is similar to #how partial correlation works
#Just one predictor does not fully explain the other variable and does have some degree of partial correlation with a 3rd of 4th or #xth variable




