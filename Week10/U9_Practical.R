library(ggplot2)
library(pastecs)

rm(list=ls())
setwd("~/Documents/RU/Sem2/Statistics/Week10")

###1 Load in the data from ExamGradesWide.csv. 
#This spreadsheet contains the data for the final exam grades from 2 different French language courses. 
#No students from the first course were in the second course. 
#Use the appropriate method to check whether the means in the two courses are different from one another. 
#You do not have a hypothesis about which course students might score higher in. 
#Make sure to graph the data (include means), check assumptions, run the appropriate test, calculate an effect size, and report the results. Hint: remember that certain functions may prefer/require "wide" or "long" data format. If you violate a test assumption, state this, and do not proceed with the rest of the analysis.
examGradesWide <- read.table("ExamGradesWide.csv", comment.char="", quote="", header=T, sep="\t")
summary(examGradesWide)

stat.desc(examGradesWide, basic=FALSE, norm=TRUE)
#means beginner 75 intermediate 64
#SE beginner 2.47380917 intermediate 2.51493622
shapiro.test(examGradesWide$beginner)
#W = .97717 p-value = 0.7465
shapiro.test(examGradesWide$intermediate)
#W = .97046 p-value = 0.5517

t.test(examGradesWide$beginner, examGradesWide$intermediate)
#t = 2.693
#df = 57.984
# The test is significant with a p-value of =.009244
sqrt((2.693)^2/((2.693)^2 + 57.984))
#The effect size is .3334202


###2 Load in the data from ColdMedicine.csv. 
#This spreadsheet contains data for how long two different medications suppressed the symptons of a cold (e.g., cough, runny nose, sore throat). 
#In this within-subjects experiment, sick individuals were given one medicine, and then, at a later point, were given the other. 
#Doctors measured how long (in hours) the symptons were suppressed after each dosing. 
#Use the appropriate method to check your hypothesis that DiseaseEx suppresses symptons for longer than SunshineDrops. 
#Again, make sure to proceed through the various steps of the analysis. 
#If you violate a test assumption, state this, and do not proceed with the rest of the analysis.
coldMedicine <- read.table("ColdMedicine.csv", comment.char="", quote="", header=T, sep="\t")
summary(coldMedicine)

by(coldMedicine$Time, coldMedicine$Medicine, stat.desc, basic=FALSE, norm=TRUE)
#means DiseaseEx 3.4147668 SunshineDrops 1.688190017
#SE DiseaseEx 0.2091904 SunshineDrops 0.212961256
by(coldMedicine$Time, coldMedicine$Medicine, shapiro.test)
#DiseaseEX: W = 0.94622, p-value = 0.3133
#SunshineDrops: W = 0.8559, p-value = 0.006704

t.test(Time ~ Medicine, data=coldMedicine, alternative = "greater", paired= TRUE)
#t = 5.1614
#df = 19
# The test is significant with a p-value of = 2.776e-05
sqrt((5.1614)^2/((5.1614)^2 + 19))
#The effect size is 0.7640019


###3a In one of the two questions above, the assumption of normality was violated. 
#Because of this, a t-test cannot be used. 
#When this happens, an alternative that can be used is the Wilcoxon test. 
#Just like the t-test, there are 2 versions of the Wilcoxon test: one for independent samples (called the Wilcoxon rank-sum test) 
#and one for dependent samples (called the Wilcoxon signed-rank test). 
#One of the key differences between Wilcoxon tests and t-tests is that the former compare medians between groups, not means. 
#Furthermore, they are based on the rank order of the data points (1st highest, 2nd highest, 3rd highest, etc.) 
#rather than the actual values of the data points. 
#Thus, Wilcoxon tests are also useful when you violate the assumption that your outcome variable is interval/ratio 
#(i.e., when you have ordinal data!). 
#Perform a Wilcoxon test on the data by using the wilcox.test() function. 
#Passing arguments to this function is the same as for the t-test (in terms of entering the data, 
#specifying directional hypotheses, and indicating whether the test should be independent or dependent). 
#However, in addition to these arguments, you must pass two more: correct=False and exact=False. 
#Remember that our hypothesis is that DiseaseEx should be better than SunshineDrops!
coldMedicine <- read.table("ColdMedicine.csv", comment.char="", quote="", header=T, sep="\t")
summary(coldMedicine)

wilcox.test(Time ~ Medicine, data=coldMedicine, alternative = "greater", paired = TRUE)
#V = 198
# The test is significant with a p-value of = 6.676e-05

###3b You are now going to calculate an effect size for the results of the Wilcoxon test. 
#To do this, you need the following information:

#p = the p-value from the Wilcoxon output
#N = total number of datapoints across both samples

#The equation to calculate an effect size r is qnorm(p/2) / sqrt(n)
p = 6.676e-05
N = nrow(coldMedicine)
qnorm(p/2) / sqrt(N) 
# -0.6304865


###THIS IS THE THRESHOLD; RAISE YOUR HAND TO GET SIGNED OFF BY THE TAs


###3c You should now get the medians of the two samples as well as the interquartile range (IQR). To get the medians, you can use the same function from 1 and 2 you used to get summary statistics. To get the IQR, use the function IQR(). You can pass each sample separately, or use the by() function.


###3d To report the results of a Wilcoxon test, you should use the same format as reporting the t-test. But some things should change, obviously. Instead of reporting the mean and the standard error for each sample, report the median and the IQR. Also, instead of reporting the t statistic, its degrees of freedom, and its p-value, you just report the W or V statistic (depending on whether you are using the rank sum or the signed-rank test) along with the p-value. Also, you should mention if the Wilcoxon test is one-tailed.



