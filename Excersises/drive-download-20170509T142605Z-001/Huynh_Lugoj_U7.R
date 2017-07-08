library(ggplot2)
library(car)
library(lmtest)

rm(list=ls(all=TRUE))
###1 Load in the data from Idiom_use_data.csv. This spreadsheet contains information regarding Dutch idioms--word sequences such as "tegen de lamp lopen", which have both a literal meaning (walk into the lamp) and a figurative one (get caught). Specifically, participants in a study provided ratings for how frequent/common they thought the idioms were, and how familiar they were with the figurative meanings of the idioms. 

#Your job is to graph the data and perform a linear regression on it, predicting familiarity as a function of frequency. What do the results tell you about the relationship between familiarity and frequency ratings? Make sure to check model assumptions and look for high-leverage and highly influential points. Are there any problems with these things?
Idioms = read.table(file.choose(), header=TRUE, sep="\t", quote="", comment.char="", fill=TRUE)

Idioms_graph = ggplot(Idioms, aes(Frequency, Familiarity))
Idioms_graph + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

Regression_idioms = lm(Familiarity~Frequency, data = Idioms)
summary(Regression_idioms)

durbinWatsonTest(Regression_idioms)
dwtest(Regression_idioms)

par(mfrow	=	c(2,2))	
par(mar	=	c(4.25,4.25,4.25,4.25))	
plot(Regression_idioms)

#Explanation:                                                                                                                         The scatterplot shows us that there is a positive linear correlation between frequency and familiarity.                              The squared R value is 0.92, which means 92% of the variability is explained by the model.                                           The p-value is 2.2-e16, meaning the data is statistically significant.                                                                The regression equation is: y = -08081 + 0.9314 * x

#Model assumptions: Variable types                                                                                                    For variable types: they're both ratio types, because a familiarity/frequency of 0 means 0, so no familiarity or frequency and there cannot be a negative amount of frequency/familiarity. Furthermore a familiarity/frequency of 4 is double the value of 2. The ratio variables match our regression model, because a regression model should be made out of ratio variables.

#Model assumptions: linearity                                                                                                         The scatterplot doesn't follow a curved pattern and is actually just a straight positive line so it's linear.

#Model assumptions: independence                                                                                                      According to blackboard we can assume that the data is independent

#Model assumptions: residuals    

#For autocorrelation we use the Durbin Watson test. This test has a D-W value of 1.9. This is less than 2, meaning the residuals are positively correlated. The p-value is 0.292, which means it's statistically non significant and thus there is no auto correlation.

#For homoscedasticity: The residuals vs fitted and scale-location graphs are fitting within the assumption of homoscedasticity.

#For normally distributed:                                                                                                            It's basically a straight linear line only the top deviates a tiny bit but that doesn't matter that much so the data is normally distributed.

#Explanation highly influential and highly leverage points                                                                            The residuals vs leverage graph doesn't show any signs of highly leverage points or highly influential. No data value is below the cook's distance so there is no problem.

#Explanation of all:                                                                                                                  All the assumptions hold, thus there are no problems with our dataset.

###2 Load in the data from Science_fair.csv. This spreadsheet contains the results from a competition at a school science fair. Students were awarded first (winner) through fifth places for their science fair projects. Many students can "tie" (be awarded the same place). 

#Again, plot the data and perform a regression predicting students' places (prizes) as a function of how much time they spent preparing their project. Discuss the results. You should also examine whether this data and model violate any assumptions, and whether there are problematic data points.
Science_fair = read.table(file.choose(), header=TRUE, sep="\t", quote="", comment.char="", fill=TRUE)

Science_graph = ggplot(Science_fair, aes(Preparation, Prize))
Science_graph + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

Science_regression = lm(Prize~Preparation, data = Science_fair)
summary(Science_regression)

durbinWatsonTest(Science_regression)
dwtest(Science_regression)

par(mfrow	=	c(2,2))	
par(mar	=	c(4.25,4.25,4.25,4.25))	
plot(Science_regression)

#Explanation:                                                                                                                         The regression equation is y = 4.77095 - 0.20033 * x                                                                                  The equation states that the line is negative because of the -3.94, in the scatterplot you can see that it's linear and going down, thus it's a negative linear line. The p-value is 3.055e-09 which means the data is statistically significant. The multiple R-squared value is 0.7893, which means 79% of the variability is explained by the model.

#Explanation data violations and assumptions:                                                                                         Let's see this data violates the assumption of variable types, because Prize is an outcome variable, it has ranks and this violates the assumption                                                                                                                       The data is independent and the scatterplot shows us that the line is linear, as there are no curved lines.                           Assumptions regarding residuals, with the Durbin Watson test value = 0.59 the value is less than 2, therefore it's positively autocorre-lated. The p-value is literally 0 and the DW value is below 1, meaning that there's a huge problem with this dataset. There is autocorrelation because the p-value is significant. DW value of 0.51 meaning the residuals are positively correlated              The normal Q-Q plot shows us that the dataset is not normally distributed either, the residuals at the start aren't following the line Also the dataframe exists of 25 objects and not 30.                                                                                   For homoscedasticity assumption it's also violated, because both graphs residuals vs fitted and scale-location aren't showing any homoscedasticity and there are too few datapoints. The data is not evenly distributed, also the line is very curved.                  So this dataset violates pretty much every single assumption, except independency.                                                    For problematic datapoints we look at the residuals vs leverage graph. We see there are no data points that are beneath Cook's distance thus there are no outliers, it does have high leverage points.



###3 Load in the data from mtcars.csv. This spreadsheet contains various pieces information about different cars, from engine size to fuel efficiency to performance. We are interested in the variables "hp" (horsepower--that is, how powerful a car's engine is) and "disp" (displacement--that is, how large the engine is). Plot these two variables against each other and perform a regression predicting horsepower as a function of displacement. Discuss the results. Also check assumptions. Are any violated? Are there problematic data points?
Mtcars_df = read.table(file.choose(), header=TRUE, sep="\t", quote="", comment.char="", fill=TRUE)

Mtcars_graph = ggplot(Mtcars_df, aes(disp,hp))
Mtcars_graph + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

Mtcars_regression = lm(hp~disp, data = Mtcars_df)
summary(Mtcars_regression)

durbinWatsonTest(Mtcars_regression)
dwtest(Mtcars_regression)

par(mfrow	=	c(2,2))	
par(mar	=	c(4.25,4.25,4.25,4.25))	
plot(Mtcars_regression)

#Explanation: Scatterplot and linear regression                                                                                      The scatterplot shows us there is a positive correlation, because the line is linearly positive, however we do see that there is one noticeable datapoint that is much higher than the rest. The regression model shows us the multiple R-squared value of 0.6256, which means 63% of the variability is explained by the model. The p-value is 7.143e-08 so it's statistically significant. The equation is   y = 45.7345 + 0.4375 * x this makes sense because it's a positive linear line as seen in the scatterplot.

#Explanation: assumptions                                                                                                             For variable types, the assumption holds, because both variables hp and disp are ratio variables. They can't be negative and 0 means 0 also 4 hp is 2x as big as 2hp and 80 displacement is 2x as much as 40 displacement.                                                 The data is independent and the scatterplot shows a positive line.                                                                    Using the dwtest function because durbin watson test function gives different p-values (because it takes samples). The p-value of the dwtest is 0.023, so it's significant and thus there is some auto correlation, but the durbin watson value is 1.34 which is still above 1 and this means the residuals are positively correlated. For the original durbin watson test function it's sometimes significant and sometimes non significant.                                                                                                            For normally distributed we can see in the QQ plot that the data points are not entirely showing a linear line, namely because of one big outlier at the end. Still the dataframe has 43 objects so i guess you could label it as normally distributed.                    For homoscedasticity we can see that it's violated, because the graphs residuals vs fitted and scale-location aren't evenly distributed As for problematic datapoints, we can see there are no outliers in the graph, because no datapoint is below cook's distance, it is however a high leverage point.                                                                                                   

###4 Using the same data as is 3, now perform a regression predicting displacement as a function of horsepower. How is this model different from the one in 3? How is it the same? What would be the predicted displacement of an engine that outputs 195 horsepower?
Mtcars_regression2 = lm(disp~hp, data = Mtcars_df)
summary(Mtcars_regression2)

Mtcars_graph2 = ggplot(Mtcars_df, aes(hp,disp))
Mtcars_graph2 + geom_point() + geom_smooth(method = "lm", alpha = 0.1)


#Explanation:                                                                                                                       The coefficients are different, the estimate of intercept (which is now displacement) is obviously different when the intercept is from horse power. Also it has different residuals. The equation of this regression model is y = 20.9925 + 1.4298 * x where x is horse power and y is displacement. If we fill in for x = 195, we get: y = 20.9925 + (1.4298 * 195) = 299.806 And when you look at the scatterplot you can see that around 195 hp the line predicts roughly 300 displacement. 


###5 Load in the data from Baseball.csv. This spreadsheet contains various pieces of information about different players from this slow and boring American sport. We are interested in two variables: "homeruns" (how many times they hit the ball beyond the outfield) and "hits" (total number of times hitting the ball). Graph these two variables against each other and then run a regression predicting homeruns as a function of hits. Discuss the results. Then, check assumptions and whether there are problematic data points.
Baseball = read.table(file.choose(), header=TRUE, sep="\t", quote="", comment.char="", fill=TRUE)

Baseball_graph = ggplot(Baseball, aes(homeruns,hits))
Baseball_graph + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

Baseball_regression = lm(homeruns~hits, data = Baseball)
summary(Baseball_regression)

durbinWatsonTest(Baseball_regression)
dwtest(Baseball_regression)

par(mfrow	=	c(2,2))	
par(mar	=	c(4.25,4.25,4.25,4.25))	
plot(Baseball_regression)

#Explanation: scatterplot and regression model                                                                                      According to our scatterplot there is a positive linear line. However yet again we see a few datapoints that are somewhat following the trending line, but it is nonetheless very high. In our regression model we have the equation: y = -4.600 + 0.103 * x            The squared R value is 0.61, meaning 61% of the variability is explained by our model. The p-value is 2.2e-16 so it's statistically significant                                                 

#Explanation: Assumptions                                                                                                            The variables homeruns and hits are both ratio variables, thus our variable types assumption is not violated. The data is independent and the line is positively linear, which you can see in the scatterplot.                                                              Using the durbin watson test we can see that the p-value is around 0.15, so the dataset is statistically not significant which means there is no autocorrelation. The D-W value is 1.84 which is less than 2, this means the residuals are positively correlated.         In our QQ plot we can see that the data is almost following a linear line, except for the parts of the tail and head. However the dataset consists of 322 objects and this together with the almost accurate qq-plot I think it's normally distributed.                 For homoscedasticity we can see that residuals vs fitted is a somewhat okay fit, but scale-location violates homoscedasticity, because it's not evenly distributed, some datapoints are way up and some are way down, while the rest is clamped together.                    Now for problematic datapoints we can see in the residuals vs leverage plot that: There is one datapoints lying under Cook's distance,this makes it an outlierm but it also follows the trend of the line, making it a highly influential point as well. There are also various high leverage points 


###6 Load in the data from Tooth_decay.csv. This spreadsheet contains data about rate of tooth decay for different people, and how much toothpaste they typically use (in ml). Graph the data and perform a regression that predicts tooth decay as a function of how much toothpaste is used by different people. Examine assumptions. Are there any problematic data points? If there are, try removing them, perform the regression again, and discuss any changes in the results of the regression.
ToothDecay = read.table(file.choose(), header=TRUE, sep="\t", quote="", comment.char="", fill=TRUE)

ToothDecay_graph = ggplot(ToothDecay, aes(toothpaste,decay))
ToothDecay_graph + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

ToothDecay_regression = lm(decay~toothpaste, data = ToothDecay)
summary(ToothDecay_regression)

durbinWatsonTest(ToothDecay_regression)
dwtest(ToothDecay_regression)

par(mfrow	=	c(2,2))	
par(mar	=	c(4.25,4.25,4.25,4.25))	
plot(ToothDecay_regression)

#Explanation Scatterplot and regression model                                                                                         The scatterplot shows us a negatively correlated line. The datapoint on (6.0) is a big outlier. The regression model equation is      y = 4.63 - 0.59 * x  which indeed shows that the y is decreasing with every increasing x. Squared r value is 0.31, so 31% of the variability is explained by this model. The p-value is 0.008 which means it's statistically significant.

#Explanation: Assumptions                                                                                                           Variable type assumption, decay and toothpaste are both ratio variables. So this assumption holds. Moving on, the dataset is dependent and it's linear, according to our scatterplot. For autocorrelation we have the value of around 0.90 and 0.98 (or 0.53 for the dwtest) So the p-value is not significant so no autocorrelation. The D-W value itself 1.99 so the residuals are positively correlated.       Then for homoscedasticity both graphs are violating homoscedasticity because of one outlier actually.                                 So obviously for problematic datapoints in the residuals vs leverage, we can see it's an outlier and a highly influential point as well. 

ToothDecay_new = subset(ToothDecay, toothpaste < 4)
row.names(ToothDecay_new) = seq(nrow(ToothDecay_new))

ToothDecay_graph2 = ggplot(ToothDecay_complete, aes(toothpaste,decay))
ToothDecay_graph2 + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

ToothDecay_regression2 = lm(decay~toothpaste, data = ToothDecay_complete)
summary(ToothDecay_regression2)

durbinWatsonTest(ToothDecay_regression2)
dwtest(ToothDecay_regression2)

par(mfrow	=	c(2,2))	
par(mar	=	c(4.25,4.25,4.25,4.25))	
plot(ToothDecay_regression2)

#Explanation: After removal of outlier                                                                                                So after removing the outlier, our scatterplot is actually not that negative anymore, it's slightly negative and the datapoints are now scattered nice and evenly now. In our regression model we see that our equation has changed too. It's now y = 3.71 - 0.07 * x     We immediately notice that the decrease is a mere 0.07 so it's still negatively correlated but not that severe. The R-squared value is now a mere 0.00265 so that's 0.265% variability explained by our model, which is a huge difference from the 31% we got earlier with the outlier. Furthermore this dataset now has a p-value of 0.83 so it's not significant either.

#Moving onto the assumptions. The variables have not changed so they remain the same so the assumption still holds. It's still dependent and the function still says it's a negative linear line. For the autocorrelation assumption our durbin watson test values have changed. D-W value is now 1.73 this still means the residuals are positively correlated and the p-value is around 0.55 so it's non significant thus there's no autocorrelation.                                                                                      Moving onto homoscedasticity we can see that both graphs have nice evenly distributed datapoints and thus the assumption holds. The normal line is looking way nicer too. The datapoints are following the line more closely now. And finally in the residuals vs leverage graph we can see that it's more evenly distributed and there are no outliers. 