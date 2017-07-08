library(ggplot2)
library(pastecs)
library(car)

setwd("~/Documents/RU/Sem2/Statistics/Week10")

###NOTE: for reporting, remember to mention if you have performed a one-tailed t-test


###Warm-up Exercises

#When you are comparing the means from 2 conditions in an experiment, 
#and the same participants participated in both conditions, what kind of test should you run?
answer = "If participants participate in both conditions it means the expiriment is dependent.
Thus we use a dependent t-test"


#When you are comparing the means from 2 conditions in an experiment, 
#and different participants participated in the 2 conditions, what kind of test should you run?
answer = "If different participants participate in each of the conditions it means the expiriment is independent.
Thus we use a independent t-test"


#When you are comparing the means from 3 or more conditions in an experiment, what kind of test should you run?
answer = "When we compare means of 3 or more conditions we use the ANOVA-test"

###Main Exercises

###1a Load in Smartphone.csv. 
#This spreadsheet contains data from a study on consumer satisfaction with Apple versus Samsung smartphones. 
#In this between-subjects design experiment, participants were given either an Apple or a Samsung phone, 
#told to use it for a week, and then assigned it a rating based on their satisfaction. 
#You are curious whether the mean satisfaction for one phone is greater than the other 
#(but you don't have a specific hypothesis about which phone should be better). 
#Perform the approprite analysis. Make sure to plot your data, test assumptions, 
#run the appropriate test, and report the results. If you violate assumptions, perform an alternative test.
smartphone <- read.table("Smartphone.csv", comment.char="", quote="", header=T, sep="\t")
summary(smartphone)
head(smartphone)

ggplot(smartphone, aes(Brand, Rating)) + geom_boxplot(notch = TRUE) + stat_summary(fun.y=mean, geom="point", colour="red")

shapiro.test(smartphone$Rating)
#W = .9854 p-value = 0.4981 
#smartphone$Rating is significantly normal
#As the Rating is ordinal we use a wilcox-test

by(smartphone$Rating, smartphone$Brand, stat.desc, basic=FALSE, norm=TRUE)
by(smartphone$Rating, smartphone$Brand, IQR)
model1 <- wilcox.test(Rating~Brand, data=smartphone, correct=FALSE, exact=FALSE); model1
qnorm(model1$p.value/2)/sqrt(dim(smartphone)[1])
# On avarage, participants gave a higher rating to Samsung phones (median = 75, IQR = 22.75) then Apple phones (median = 69.5, IQR = 12.25)
# However, according to a Wilcoxon rank-sum test, this difference is not significant, W = 645.5, p-value = .1369; 
# futhermore, it represented a small effect, r = -.1663062 

###1b The consumer research agency that conducted the study in 1a informs you that they made it mistake in their 
#original communication with you: in fact, the study was conducted with a within-subjects design. 
#Thus, the first rating for the Samsung phone is from the same study participant as the first rating for the 
#Apple phone (and so forth). Rerun your analysis from 1a, but with the necessary changes appropriate to a within-subjects design.
model2 <- wilcox.test(Rating~Brand, data=smartphone,paired=TRUE, correct=FALSE, exact=FALSE); model2 
qnorm(model2$p.value/2)/sqrt(dim(smartphone)[1])
# On avarage, participants gave a higher rating to Samsung phones (median = 75, IQR = 22.75) then Apple phones (median = 69.5, IQR = 12.25)
# According to a Wilcoxon signed-rank test, this difference is significant, V = 157.5, p-value = .001997; 
# futhermore, it represented a medium effect, r = -.3455453 


###1c Why do you think the p-values in 1a and 1b differ? 
#It may be helpful to create a scatterplot that compares, for each study participant, 
#the Samsung rating they gave (x-axis) against the Apple rating they gave (y-axis).
smartphoneWide <- data.frame(AppleRating = smartphone[smartphone$Brand == 'Apple',]$Rating, 
                             SamsungRating = smartphone[smartphone$Brand == 'Samsung',]$Rating)
ggplot(smartphoneWide, aes(x = SamsungRating, y = AppleRating)) + geom_point()
#We can see in the plot that people who give a high rating to an Apple phone also give a high rating to a Samsung phone.
#This can be explained by that some people give higher ratings in general, which makes sense as this is within-subjects design.


###1d You discover results from a previous study that suggests that Samsung phones are better liked. 
#Thus, you update your hypothesis to a directional one: you now expect the Samsung phone ratings to be on average HIGHER than the Apple ones. 
#Rerun your analysis from 1b to reflect the change to the hypothesis.
model3 <- wilcox.test(Rating~Brand, data=smartphone,paired=TRUE, correct=FALSE, exact=FALSE, alternative = "less"); model3
qnorm(model3$p.value/2)/sqrt(dim(smartphone)[1])
# On avarage, participants gave a higher rating to Samsung phones (median = 75, IQR = 22.75) then Apple phones (median = 69.5, IQR = 12.25)
# According to a Wilcoxon signed-rank test, this difference is significant, V = 157.5, p-value = 0.0009986; 
# futhermore, it represented a medium effect, r = -.3679364 


###1e Why do you think the p-values in 1b and 1d differ (see our discussion way back from chapter 2)?
"In 1d we look only at that Samsung ratings are higher then Apple ratings. As we cannot test the whole population we only test a sample.
The p-value respresents that chance that we get the results while there is the predicted effect in the population. Thus when we limit
the test to only look at "less" it folows that the chance is smaller that we get such results while there is not a real difference in the 
complete population."


###2 Load in Bacteria.csv. This spreadsheet contains data from a study on bacteria concentrations in 2 lakes. 
#Water samples were taken from different locations in both lakes, and the amount of bacteria was measured. 
#You are curious whether the overall bacteria level in one lake is higher than in the other lake, 
#but you do not have a hypothesis about which lake might have more bacteria. Perform the appropriate analysis. 
#Again, make sure to plot your data, test assumptions, run the appropriate test, and report the results. 
#If you violate assumptions, perform an alternative test.
bacteria <- read.table("bacteria.csv", comment.char="", quote="", header=T, sep="\t")
head(bacteria)
summary(bacteria)


shapiro.test(bacteria$RoseLake)
#W = 0.95671, p-value = 0.6355
#RoseLake does seem to be significant normal
shapiro.test(bacteria$LakeSulfur)
#W = 0.87425, p-value = 0.03897
#RoseLake does seem to be significant non-normal
#We use a wilcoxon test because of non-normal data

bacteriaRoseLake <- data.frame(Lake = "Rose", Rating = bacteria$RoseLake); bacteriaRoseLake
bacteriaLakeSulfur <- data.frame(Lake = "Sulfur", Rating = bacteria$LakeSulfur); bacteriaLakeSulfur
bacteriaLong <- rbind(bacteriaRoseLake , bacteriaLakeSulfur); bacteriaLong

ggplot(bacteriaLong, aes(Lake, Rating)) + geom_boxplot(notch = FALSE) + stat_summary(fun.y=mean, geom="point", colour="red")
ggplot(bacteria, aes(x = RoseLake, y = LakeSulfur)) + geom_point()

by(bacteriaLong$Rating, bacteriaLong$Lake, IQR)
by(bacteriaLong$Rating, bacteriaLong$Lake, stat.desc, basic=FALSE, norm=TRUE)

model4 <- wilcox.test(Rating~Lake, data=bacteriaLong, correct=FALSE, exact=FALSE); model4
qnorm(model4$p.value/2)/sqrt(dim(bacteriaLong)[1])
# On avarage, there are more bacteria in the RoseLake (median = 51, IQR = 17.5) then the SulfurLake (median = 40, IQR = 45)
# However, according to a Wilcoxon rank-sum test, this difference is not significant, W = 132, p-value = .4184; 
# futhermore, it represented a small effect, r = -.1477358 


###3 Load in the data in DutyFree.csv. 
#This spreadsheet contains data on how much money customers spent at duty free shops at different European airports (in Euros). 
#Each data point represents a sale. Conduct an analysis to investigate whether the mean sales at the various airports differ. 
#Make sure to graph your data, check assumptions, and calculate an effect size. 
#Furthermore, while you expect the means to differ, you do not have specific hypotheses about which means should differ from one another; 
#therefore, you should do a post hoc analysis to investigate pairwise differences between means. 
#When you are finished, report your results of the main analysis as well as the post hoc findings.
dutyfree <- read.table("DutyFree.csv", comment.char="", quote="", header=T, sep="\t")
head(dutyfree)
summary(dutyfree)
#Sample sizes are almost equal so no need to worry about normality

#Check Homogeneity of variance
leveneTest(dutyfree$Sales, dutyfree$Airport, center = "median")
#[F(3,26) = 0.827, p-value = 0.491]
#As p > .05 it is lickely that population variances are equal and the obtained differences
#in sample variances are based on random sampling from a population with equal variances

by(dutyfree$Sales, dutyfree$Airport, IQR)
by(dutyfree$Sales, dutyfree$Airport, stat.desc, basic=FALSE, norm=TRUE)

ggplot(dutyfree, aes(Airport, Sales)) + geom_boxplot(notch = FALSE) + stat_summary(fun.y=mean, geom="point", colour="red")

model5 <- aov(Sales ~ Airport, data=dutyfree); summary(model5)
summary(lm(Sales ~ Airport, data=dutyfree))

#There was a significant difference in sales at the p<.05 level for the three airports [F(3,26) = 8.317, p-value = 0.000483]


SSm = 4181 
SSr = 4357
DFm = 3   
MSr = 167.6

Omega = (SSm - DFm * MSr) / (SSm + SSr + MSr); Omega
#0.4225096



#No specific hypotheses about which means should differ from one another
pairwise.t.test(dutyfree$Sales, dutyfree$Airport, p.adjust.method="BH")


###4 Load in the data from cardio.csv. 
#In this study, researchers measures the amount of calories burned by people in 3 different exercises: jogging, rowing, and swimming. 
#Run an analysis to check whether the mean number of calories burnt differs across the 3 exercises. 
#Make sure to follow all necessary steps. 
#Furthermore, previous research on exercise has suggested that swimming is the most intense workout of the 3, 
#followed by rowing, while jogging is the least rigorous. Using planned comparisons, investigate this hypothesis.
cardio <- read.table("cardio.csv", comment.char="", quote="", header=T, sep="\t")
head(cardio)
summary(cardio)
#Sample sizes are equal so no need to worry about normality

#Check Homogeneity of variance
leveneTest(cardio$calories, cardio$workout, center = "median")
#[F(2,27) = 1.7343, p-value = 0.1956]
#As p > .05 it is lickely that population variances are equal and the obtained differences
#in sample variances are based on random sampling from a population with equal variances

by(cardio$calories, cardio$workout, IQR)
by(cardio$calories, cardio$workout, stat.desc, basic=FALSE, norm=TRUE)

ggplot(cardio, aes(workout, calories)) + geom_boxplot(notch = FALSE) + stat_summary(fun.y=mean, geom="point", colour="red")

model6 <- aov(calories ~ workout, data=cardio); summary(model6)

SSm = 1205.1 
SSr = 774.4
DFm = 2   
MSr = 28.7

Omega = (SSm - DFm * MSr) / (SSm + SSr + MSr); Omega
#0.5715068

contrast1 <- c(1,1,-2) #swimming to rowing and jogging
contrast2 <- c(-1,1,0) #rowing to jogging

#r = sqrt(t^2 / (t^2 + df))
#df = num of data points - num of predictors (i.e., contrast variables) - 1

r_contrast1 = sqrt(5.978^2 / (5.978^2 + 27));r_contrast1
#contrast 1 has a big effect size of r = 0.7547373         
r_contrast2 = sqrt((-3.925)^2 / ((-3.925)^2 + 27));r_contrast2
#contrast 2 has a big effect size of r = 0.6027371     

contrasts(cardio$workout) <- cbind(contrast1, contrast2); contrasts(cardio$workout)
model6Planned <- aov(calories ~ workout, data=cardio); summary.lm(model6Planned)
#There was a significant difference in calories at the p<.05 level for the three workouts [F(2,27) = 21.01, p-value: 3.145e-06]
