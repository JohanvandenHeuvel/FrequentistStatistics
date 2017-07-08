library(pastecs)
library(ggplot2)
library(car)

setwd("~/Documents/RU/Sem2/Statistics/Week5")

#1 Using the data in Lecturer.csv, plot an error line chart showing the mean income for students and lecturers. Include confidence intervals. What do the confidence intervals suggest?
Lecturer <- read.table("Lecturer.csv", sep="\t", header=TRUE, comment.char="", quote="")
head(Lecturer)

LecturerPlot <- ggplot(Lecturer, aes(job, income))
LecturerPlot + stat_summary(fun.y = mean, geom="point") + stat_summary(fun.y = mean, geom = "line", aes(group = 1)) + stat_summary(fun.data = mean_cl_normal, geom = "errorbar")
#the confidence intervals suggest that the lecturers and students are significantly different from each other in the case of income

#2 Using the same data, plot an error line chart showing the mean neuroticism for students and lecturers. Include confidence intervals. What do the confidence intervals suggest?
LecturerPlot <- ggplot(Lecturer, aes(job, neurotic))
LecturerPlot + stat_summary(fun.y = mean, geom="point") + stat_summary(fun.y = mean, geom = "line", aes(group = 1)) + stat_summary(fun.data = mean_cl_normal, geom = "errorbar")
#the confidence intervals overlap thus we can assume that the two groups, lectures and students, are not significant different from each other in the case of neurotism

#Using the beer_ratings.csv data, your are going to check the assumptions of normality and homogeneity of variance for the two beers (Westmalle and Orval).

#3 Load in the data and plot a histogram for each beer. Make sure that density is on the y-axis and use binwidth=0.2 in the histogram geoms. What do the two histograms suggest about the normality of the ratings for the two beers?
Beer <- read.table("beer_ratings.csv", sep="\t", header=TRUE, comment.char="", quote="")
head(Beer)

Beer_Westmalle <- subset(Beer, Beer$BEER == "Westmalle")
Beer_Orval <- subset(Beer, Beer$BEER == "Orval")

Beer_Westmalle_Plot <- ggplot(Beer_Westmalle,aes(RATING)) + geom_histogram(aes(y=..density..), binwidth = 0.2, colour = "black", fill = "white")+labs(x="Rating", y="Density")+theme(legend.position = "none");    
Beer_Orval_Plot <- ggplot(Beer_Orval,aes(RATING)) + geom_histogram(aes(y=..density..), binwidth = 0.2, colour = "black", fill = "white")+labs(x="Rating", y="Density")+theme(legend.position = "none");    

mean_and_sd_Westmalle <- list(mean=mean(Beer_Westmalle$RATING,na.rm=TRUE),sd=sd(Beer_Westmalle$RATING,na.rm=TRUE))  
mean_and_sd_Orval <- list(mean=mean(Beer_Orval$RATING,na.rm=TRUE),sd=sd(Beer_Orval$RATING,na.rm=TRUE))

Beer_Westmalle_Plot + stat_function(fun=dnorm,args=mean_and_sd_Westmalle,colour="black", size=1)
#Beer_Westmalle seems to be normally distributed with just a little skew on the right
Beer_Orval_Plot + stat_function(fun=dnorm,args=mean_and_sd_Orval,colour="black", size=1)
#Beer_Orval does not seem normal distributed because there is a high skew to the right

###IF YOU'RE HERE, YOU'VE REACHED THE THRESHOLD FOR TODAY. CONTACT A TA TO GET SIGNED OFF.

#4 Now create Q-Q plots for the two beers. What do these plots suggest about the normality of the two beers?
Beer_Westmalle_Plot <- qplot(sample = Beer_Westmalle$RATING, stat = "qq")
Beer_Westmalle_Plot
#the qq plots support the assumption from the histograms. The Westmalle qq plot is almost diagonal but only has a small
#difference in the start, which can be explained by the small skew to the right

Beer_Orval_Plot <- qplot(sample = Beer_Orval$RATING, stat = "qq")
Beer_Orval_Plot
#The Orval qq plot is not normal. There are several points where the line is not diagonal


#5 Print a summary of descriptive statistics that includes information about skew and kurtosis for the two beers. What does this information tell you about the normality of the ratings of the two beers?
stat.desc(Beer_Westmalle$RATING, basic = FALSE, norm = TRUE)
stat.desc(Beer_Orval$RATING, basic = FALSE, norm = TRUE)

#In both cases is the sample size higher then 30 so we can ignore skew.2SE and kurt.2SE
#Westmalle: Skewness = 0.003479647 , Kurtosis = 0.360670923
#Orval: Skewness = -0.526897101 , Kurtosis = -0.682123655
#If we compare the two beers we can see that Westmalle is considerably more normal then Orval. The skewness of Westmalles is very
#close to zero, but is has a bit of kurtosis. However Orval does have skew and double the amount of kurtosis of Westmalles

nrow(Beer_Westmalle) #100
nrow(Beer_Orval) #100

#6 For both beers, run a statistical test that indicates whether their ratings significantly differ from a normal distribution. For each beer, in one sentence give a proper reporting of the results of the test.
#Shapiro_Wilk test:
shapiro.test(Beer_Westmalle$RATING)
#W= 0.98384, p = 0.2613 > 0.05 so non-significant so we can assume normallity

shapiro.test(Beer_Orval$RATING)
#W= 0.94391, p = 0.0003365 < 0.05 so significant so we can assume non-normal distribution

#7 Run a statistical test that indicates whether the variances of the ratings of the two beers are non-homogenous. In one sentence, give a proper reporting of the results of the test.
#Levene's test:
leveneTest(Beer$RATING, Beer$BEER)
#For the scores on the Rating, the variances were similar F(1,198) = 3.1711, p = 0.07649 so > .05 so is significant