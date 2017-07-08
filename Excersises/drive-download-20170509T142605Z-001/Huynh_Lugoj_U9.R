library(ggplot2)
library(car)
library(pastecs)
library(tidyr)

rm(list=ls(all=TRUE))
###NOTE: for reporting, remember to mention if you have performed a one-tailed t-test


###Warm-up Exercises

#When you are comparing the means from 2 conditions in an experiment, and the same participants participated in both conditions, what kind of test should you run?
#A dependent T-test


#When you are comparing the means from 2 conditions in an experiment, and different participants participated in the 2 conditions, what kind of test should you run?
#An independent T-test


#When you are comparing the means from 3 or more conditions in an experiment, what kind of test should you run?
#You run an ANOVA test

###Main Exercises

###1a Load in Smartphone.csv. This spreadsheet contains data from a study on consumer satisfaction with Apple versus Samsung smartphones. In this between-subjects design experiment, participants were given either an Apple or a Samsung phone, told to use it for a week, and then assigned it a rating based on their satisfaction. You are curious whether the mean satisfaction for one phone is greater than the other (but you don't have a specific hypothesis about which phone should be better). Perform the appropriate analysis. Make sure to plot your data, test assumptions, run the appropriate test, and report the results. If you violate assumptions, perform an alternative test.
Smartphone <-read.table(file.choose(), sep="\t", header=T, comment.char="", quote="")

phone_graph = ggplot(Smartphone, aes(Brand, Rating))
phone_graph + geom_boxplot(notch=TRUE) + stat_summary(fun.y=mean, geom="point", colour="red")

shapiro.test(Smartphone$Rating)
#The shapiro test shows us a p-value of 0.4981 so it's non significant, meaning that the data is normally distributed. Thus no normal assumptions are violated.
#Our data uses interval variable so there are no problems with the assumptions and thus we continue with a t-test for independent data
#Also use stat.desc and IQR to report results and finally calculate the effect size.
by(Smartphone$Rating,Smartphone$Brand, stat.desc, basic=FALSE, norm=TRUE)
by(Smartphone$Rating,Smartphone$Brand, IQR)

t.test(Rating~Brand, data=Smartphone)
sqrt((-1.5669)^2/((-1.5669)^2 + 59.429))

#Reporting the results:
#In our boxplot we can see that the mean of Samsung is higher than that of Apple
#On average participants with an Apple phone rate their satisfaction lower(Mean = 70.35, Median = 69.50 IQR = 12.25, SE = 1.3) than 
#people with a Samsung (Mean = 74.70, Median = 75.00, IQR = 22.75, SE = 2.45)
#However according to t-test for independent samples, this difference is not significant: t(59.43) = -1.57, p = 0.1225
#furthermore it does represent a low/medium effect size of r = 0.1991

###1b The consumer research agency that conducted the study in 1a informs you that they made a mistake in their original communication with you: in fact, the study was conducted with a within-subjects design. Thus, the first rating for the Samsung phone is from the same study participant as the first rating for the Apple phone (and so forth). Rerun your analysis from 1a, but with the necessary changes appropriate to a within-subjects design.

#So now the only thing that changes is that the participants used both phones so the sample size becomes dependent, thus we use a 
#dependent t-test.
#Nothing else changes, so the shapiro test, summary statistics and interquartile still hold.
t.test(Rating~Brand, data=Smartphone, paired=TRUE)
sqrt((-3.152)^2/((-3.152)^2 + 39))

#Reporting the results:
#In our boxplot we can see that the mean of Samsung is higher than that of Apple
#On average participants with an Apple phone rate their satisfaction lower(Mean = 70.35, Median = 69.50 IQR = 12.25, SE = 1.3) than 
#people with a Samsung (Mean = 74.70, Median = 75.00, IQR = 22.75, SE = 2.45)
#According to a dependent t-test, this difference is significant: t(39) = -3.15, p = 0.003113.
#Furthermore it represents a big effect size of r = 0.45


###1c Why do you think the p-values in 1a and 1b differ? It may be helpful to create a scatterplot that compares, for each study participant, the Samsung rating they gave (x-axis) against the Apple rating they gave (y-axis).
widephones = data.frame(apple = subset(Smartphone$Rating, Smartphone$Brand == "Apple"), samsung = subset(Smartphone$Rating, Smartphone$Brand == "Samsung"))
head(widephones)
scatter_phone = ggplot(widephones, aes(samsung, apple))
scatter_phone + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

#Explanation
#The p-values of 1a and 1b are respectively 0.1225 and 0.003113. 1b shows a significant value while 1a does not, this means that in
#1a where the sample size is independent, the difference between samsung and apple wasn't significant and in the dependent example
#the difference was significant. This is because the ratings of the participants that only rated one brand shouldn't have any or much #correlation with each other, while if you let the participants rate both phones it makes more sense that their opinion makes more of
#difference than independent sample sizes in this case. In the scatterplot we can see the ratings of the dependent sample size, where
#we can see that the participants rated Samsung much higher than Apple, explaining our significant p-value.


###1d You discover results from a previous study that suggests that Samsung phones are better liked. Thus, you update your hypothesis to a directional one: you now expect the Samsung phone ratings to be on average HIGHER than the Apple ones. Rerun your analysis from 1b to reflect the change to the hypothesis.

t.test(Rating~Brand, data=Smartphone, alternative = "less", paired=TRUE)
#Samsung phones are better liked so we take a one directional test to the left side (negative), because it's from the point of view of
#Apple. So saying alternative="greater" actually means we say Apple is better liked than Samsung so we do the opposite.
#Its p-value is now 0.001556 meaning it's strongly significant and even more significant than 1b's p-value of 0.003113.
#The rest stays the same so same effect size of 0.45 


###1e Why do you think the p-values in 1b and 1d differ (see our discussion way back from chapter 2)

#Explanation the p-values are different, because in 1b you use a two tailed test, where you only look at 0.025 from both the negative #and positive part and together it becomes 0.05. However in a one tailed test you take the entire 0.05 of either the positive or #negative side. So it makes sense that the p-value is different for a two tailed test and one tailed test.

###2 Load in Bacteria.csv. This spreadsheet contains data from a study on bacteria concentrations in 2 lakes. Water samples were taken from different locations in both lakes, and the amount of bacteria was measured. You are curious whether the overall bacteria level in one lake is higher than in the other lake, but you do not have a hypothesis about which lake might have more bacteria. Perform the appropriate analysis. Again, make sure to plot your data, test assumptions, run the appropriate test, and report the results. If you violate assumptions, perform an alternative test.
Bacteria <-read.table(file.choose(), sep="\t", header=T, comment.char="", quote="")

bact_long = data.frame(Amount_Bacteria = c(Bacteria$RoseLake, Bacteria$LakeSulfur))
bact_long$Lake[1:15] = "RoseLake"
bact_long$Lake[16:30] = "LakeSulfur"

bact_graph = ggplot(bact_long, aes(Lake,Amount_Bacteria))
bact_graph + geom_boxplot(notch=TRUE) + stat_summary(fun.y=mean, geom="point", colour="red")

shapiro.test(Bacteria$RoseLake)
shapiro.test(Bacteria$LakeSulfur)
#The shapiro.test for LakeSulfur is 0.04 so it's significant, thus the assumption of normally distributed is violated, also less than #30 datapoints. Therefore we continue with the Wilcoxon rank-sum test, because the data is independent.

by(bact_long$Amount_Bacteria,bact_long$Lake, stat.desc, basic=FALSE, norm=TRUE)
by(bact_long$Amount_Bacteria,bact_long$Lake, IQR)

Wilcoxbact = wilcox.test(Amount_Bacteria~Lake, data = bact_long, correct=FALSE, exact=FALSE); Wilcoxbact
qnorm(Wilcoxbact$p.value/2)/sqrt(dim(bact_long)[1])

#Reporting the results Wilcoxon
#In our boxplot we can see that RoseLake's mean is higher than that of LakeSulfur
#On average, lake RoseLake has more amounts of bacteria (Median = 51.00, Mean = 51.2667, IQR = 17.5, SE = 3.3), than lake LakeSulfur 
#(Median = 41.33, Mean = 41.33, IQR = 45, SE = 6.51)
#However According to a Wilcoxon ranked-sum test, this difference is not significant, W = 93, p = 0.4184
#It represents a small negative effect with an effect size of -0.147

###3 Load in the data in DutyFree.csv. This spreadsheet contains data on how much money customers spent at duty free shops at different European airports (in Euros). Each data point represents a sale. Conduct an analysis to investigate whether the mean sales at the various airports differ. Make sure to graph your data, check assumptions, and calculate an effect size. Furthermore, while you expect the means to differ, you do not have specific hypotheses about which means should differ from one another; therefore, you should do a post hoc analysis to investigate pairwise differences between means. When you are finished, report your results of the main analysis as well as the post hoc findings.
Duty <-read.table(file.choose(), sep="\t", header=T, comment.char="", quote="")

ggplot(Duty, aes(Airport, Sales)) + geom_boxplot(notch=FALSE) + stat_summary(fun.y=mean, geom="point", colour="red")

#checking assumptions:
#So the data must be independent, it's probably independent because the sales in Paris shouldn't affect the sales in London etc.
#The shapiro test shows us a p-value of 0.5357 which means it's non significant so normally distributed holds.
#The leventest gives us a p-value of 0.491 which means it's non significant so there is homogeneity of variance, which is what we want
#Thus all assumptions hold.
shapiro.test(Duty$Sales)
leveneTest(Duty$Sales,Duty$Airport,center = "median")

Anova_duty = aov(Sales~Airport, data = Duty); summary(Anova_duty)
EffectSize = (4181 - 3 * 167.6) / (4181 + 4357 + 167.6); EffectSize

pairwise.t.test(Duty$Sales,Duty$Airport, p.adjust.method="BH")

by(Duty$Sales, Duty$Airport, stat.desc, basic=FALSE, norm=TRUE)
by(Duty$Sales, Duty$Airport, IQR)

#Reporting the results
#Looking at the boxplot we can see that London has the highest mean, followed by Amsterdam and then Paris and the lowest mean goes to Frankfurt
#On average, London has the highest average (Median = 62.00, Mean = 60.33, IQR= 14.75, SE = 7.3) followed 
#by Amsterdam (Median = 43.50, Mean = 41.625, IQR=14, SE = 4.32) then by Paris (Median= 38.00, Mean = 35.375, IQR = 14.5, SE = 4.73)
#and lastly with the lowest mean Frankfurt (Median = 27.50, Mean = 26.25, IQR = 12, SE = 2.9), According to our Anova test this difference was significant ((Pr(>F) = 0.000483) and it represents a calculated effect size of the overal model which is 0.422 
#a medium/high effect.

#Post hoc findings
#All pairwise means are significant so less than 0.05, except for the pairwise combinations: Amsterdam-Paris (0.34311)
#And Frankfurt-Paris (0.20452)


###4 Load in the data from cardio.csv. In this study, researchers measures the amount of calories burned by people in 3 different exercises: jogging, rowing, and swimming. Run an analysis to check whether the mean number of calories burnt differs across the 3 exercises. Make sure to follow all necessary steps. Furthermore, previous research on exercise has suggested that swimming is the most intense workout of the 3, followed by rowing, while jogging is the least rigorous. Using planned comparisons, investigate this hypothesis.
Cardio <-read.table(file.choose(), sep="\t", header=T, comment.char="", quote="")

ggplot(Cardio, aes(workout, calories)) + geom_boxplot(notch=FALSE) + stat_summary(fun.y=mean, geom="point", colour="red")

#Checking assumptions:
#The data is independent, because they have 10 persons who each did swimming, rowing and jogging so the data for each excercise comes
#from one person thus it has to be independent.
#The shapiro test shows us a p-value of 0.075 which is bigger than 0.05 so the data is not significant so it's normally distributed, #thus the assumption holds.
#For homogeneity we can see that our levene test displays a p-value of 0.1956 so it's non significant, thus there is homogeneity
#of variance so this assumption holds as well. All assumptions hold so we move on
shapiro.test(Cardio$calories)
leveneTest(Cardio$calories,Cardio$workout,center = "median")

Anova_cardio = aov(calories~workout, data=Cardio); summary(Anova_cardio)
Eff_size = (1205.1 - 2 * 28.7 ) / (1205.1 + 774.4 + 28.7);Eff_size

by(Cardio$calories,Cardio$workout, stat.desc, basic=FALSE, norm=TRUE)
by(Cardio$calories, Cardio$workout, IQR)

#Reporting the results for main analysis
#Looking at the boxplot we can see that the mean of swimming is much higher than the rest. Rowing comes in 2nd and jogging last.
#On average swimming has the highest burnt calories (Median=66.00, Mean =  65.4, IQR=6.5, SE = 1,36) and rowing comes in 
#2nd (Median=54.50, Mean = 56.0, IQR=11, SE = 2.25) and last place is jogging (Median=49.50, Mean = 50.0,IQR=5.75, SE = 1.31) #according to our anova test this difference is significant with a p-value of 3.15e-06 and it represents an effect size of r=  0.57, #which indicates a big effect

#Planned comparisons
levels(Cardio$workout)

#No need to relevel because the order is how we want it
contrasts(Cardio$workout) <- contr.treatment(3)
CardioPlanned = aov(calories~workout, data=Cardio); summary.lm(CardioPlanned)

contrast1 <- c(-2,1,1)   #Jogging to swimming and rowing
contrast2 <- c(0,1,-1)   #Swimming to rowing

contrasts(Cardio$workout) = cbind(contrast1, contrast2)
CardioPlanned2 = aov(calories~workout, data=Cardio); summary.lm=(CardioPlanned2)

#Calculating effect size
#r = sqrt(t^2 / (t^2 + df))

#Contrast1
Contrast1_EF = sqrt((5.159)^2 / ((5.159)^2 + 27)); Contrast1_EF
#0.7

#Contrast2
Contrast2_EF = sqrt((-3.925)^2 / ((-3.925)^2 + 27)); Contrast2_EF
#0.6

#Report of planned comparisons
#All significant values are reported at p < 0.05. There was a significant effect of workout on calories burnt, 
#F(2, 27) = 21.01, r = 0.57.
#Furthermore planned contrasts revealed that swimming and rowing lead to higher calories burnt than jogging, t(27) = 5.159, r = 0.7
#(very high effect size)
#And swimming leads to higher calories burnt than rowing (Going from swimming to rowing means less calories burnt therefore 
#the t-value is negative), t(27) = -3.925, r = 0.6 (Also a very big effect size)

