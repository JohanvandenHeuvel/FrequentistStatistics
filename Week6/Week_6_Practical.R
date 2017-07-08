library(ggm)
library(ggplot2)
setwd("~/Documents/RU/Sem2/Statistics/Week6")

###Exercises are (mostly) based on those in the book (page 243)

##1a A student was interested in whether there was a positive relationship between the time spent doing an essay and the mark received. 
#He got 45 of his friends and timed how long they spent writing an essay (hours) and the percentage they got in the essay (essay). 
#He also translated these grades into their degree classifications (grade): in the UK, a student can get a first-class mark (the best), an upper second, a lower second, a third, a pass or a fail (the worst). 
#Using the data in the file EssayMarks.dat find out what the relationship was between the time spent doing an essay and the eventual mark in terms of percentage (draw a scatterplot, too). 
#Don't forget about assumptions for using Pearson's R! What do you find?
essayData = read.table("EssayMarks.dat", sep="\t", header=TRUE, comment.char="", quote="")

cor.test(essayData$essay, essayData$hours, alternative = "greater")
#p-value = 0.03829 so the is a significant correlation. The 95% interval is 0.0194 - 1
ggplot(essayData, aes(hours, essay)) + geom_point()

essayData_Plot <- qplot(sample = essayData$essay, stat = "qq")
essayData_Plot

##1b Now find out the relationship between time spent doing the essay and grade received (from the grade column). 
#Draw a scatterplot and boxplots of the data (note that ggplot needs to draw boxplots vertically). 
#Again, don't forget about assumptions! Use two different tests to examine the relationship. What do you find? 
#Note that the levels of the grade factor must be reordered so that they appear in desired order [best grade to worst grade] in the scatterplot and boxplot. I have provided the code to do this reordering for you here.
#Also, you will need to use as.numeric() to convert the grade column from a factor into a numeric vector when you calculate the correlation.

essayData$grade <- factor(essayData$grade, levels=c("First Class","Upper Second Class","Lower Second Class","Third Class"))
essayData$grade <- as.numeric(essayData$grade)
summary(essayData)

cor.test(essayData$hours, essayData$grade, method = "spearman")
#p-value = 0.2 so the is not a significant correlation.
cor.test(essayData$hours, essayData$grade, method = "kendall")
#p-value = 0.17 so the is not a significant correlation.

ggplot(essayData, aes(hours, grade)) + geom_point()
ggplot(essayData, aes(grade, hours)) + geom_boxplot() + coord_flip()


###1c Did you choose a one- or two-tailed test for comparing essay and hours? If it was a one-tailed test, in what direction? Why? What about for hours and grade?
#One tailed test for essay and hours. This is because we are interested if there is a positive relationship. So this test wat in the 'greater' direction
#For hours and grade we use a two-tailed test because we do not know if we are looking for a positive or negative relation


#2a One potentially important factor regarding whether students will do well in a statistics course is their previous expertise with mathematics. 
#Imagine I took 25 students and looked at their degree grades for my statistics course at the end of their first year at university: first, upper second, lower second and third class. 
#I also asked these students what grade they got in their GCSE maths exams. 
#In the UK GCSEs are school exams taken at age 16 that are graded A, B, C, D, E or F (an A grade is better than all of the lower grades). 
#The data for this study are in the file grades.csv. 
#Carry out the appropriate analysis to see if GCSE maths grades correlate with first-year statistics grades.
#Don't forget a graph.
grades = read.table("grades.csv", sep="\t", header=TRUE, comment.char="", quote="")
summary(grades)

cor.test(grades$stats, grades$gcse, method = "spearman", alternative = "greater")
#p-value = 0.01121 so the is a significant correlation.
cor.test(grades$stats, grades$gcse, method = "kendall", alternative = "greater")
#p-value = 0.01465 so the isa significant correlation.

ggplot(essayData, aes(gcse, stats)) + geom_point()

###2b Did you perform a one-tailed or two-tailed test? Explain your choice.
#one tailed because we are interested in a positive relation. It would not makes sense if the is a negative relation between the grade of GCSE and the grade in stats


######IF YOU'VE COMPLETED ALL EXERCISES UP UNTIL HERE, YOU'VE REACHED THE THRESHOLD. RAISE YOUR HAND TO HAVE A TA SIGN YOU OFF.



###3a Load in the data "ice_cream.csv" and perform an analysis examining the relationship between ice cream consumption rate and rate of drowning. 
#Is the correlation significant?
iceCream = read.table("ice_cream.csv", sep="\t", header=TRUE, comment.char="", quote="")
summary(iceCream)
cor.test(iceCream$ICE_CREAM_CONSUMPTION, iceCream$DROWNING)
ggplot(iceCream, aes(ICE_CREAM_CONSUMPTION, DROWNING)) + geom_point()

#No, the correlation is significant, p = 1.557e-07 < 0.05 


###3b Now, using a single line of code, simultaneously examine the pairwise correlations among ice cream consumption rate, rate of drowning, and daytime high temperature. Which pairs are highly correlated?
cor(iceCream[,1:3])
#All of them. Ice cream & temp has 0.98 but the other two pairs have 0.72 which is high aswell


###3c Using a partial correlation, re-examine the relationship between ice cream consumption rate and rate of drowning, this time controlling for daytime high temperature. 
#What do the results tell you about the true relationship between ice cream and drowning? 
#What do the results tell you about the cause of ice cream consumption?

pc <- pcor(c("ICE_CREAM_CONSUMPTION","DROWNING","DAYTIME_HIGH_TEMP"), var(iceCream)); pc
pcor.test(pc, 1, nrow(iceCream))
#Without controlling for the temp we have a correlation of 0.72 and if we do control we get 0.12
#This is not significant because p = 0.43 > 0.05

###4 Use the data in the table below to manually calculate the Kendall's Tau value.

####Race versus Leg Length Data
#
#                     Jeroen      Julia      Tessa          Bas      Bastiaan       Jolanda     
#Place in Race          1           2           3            4           5             6
#Rank of Leg Length     4           6           2            1           3             5

#C =  2 0 2 2 1 = 7
#D =  3 4 1 0 0 = 8
C= 7
D = 8
(C-D)/(C+D)


