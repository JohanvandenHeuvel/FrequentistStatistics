#Initiate ggplot2
library(ggplot2)
library(reshape)
library(plyr)
library(lattice)
library(Hmisc)



#--------Boxplot----------
#also known as "box and whisker" plot
#useful for viewing numerical data that is grouped; displays median and dispersion information

festivalData <- read.table("DownloadFestival.dat", sep="\t", header=TRUE, comment.char="", quote="")
head(festivalData)
#ticknumb: ticket number of person at musical festival
#gender
#days 1-3: the hygience scores for festival goers on days 1 through 3 (0 is dirty, 4 is clean)

festivalBoxplot <- ggplot(festivalData, aes(gender, day1))

festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")

#Notice the outlier (i.e., extreme score). Outliers distort the mean and inflate the standard devation.
#Easy way to find the outlier is to sort the data (increasing) and look at the last score:
festivalData <- festivalData[order(festivalData$day1),]
tail(festivalData,1)

#value is 20.02--This is not possible since hygiene scores range from 0 to 4. Probably typo. Should have been 2.02
festivalData$day1[dim(festivalData)[1]] <- 2.02 
tail(festivalData,1)

#fixed! Let's replot...
festivalBoxplot <- ggplot(festivalData, aes(gender, day1))
festivalBoxplot + geom_boxplot(notch=TRUE) + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")

#very top and very bottom represent the range
#middle bars represent the medians
#top and bottom of boxes represent the upper and lower quartiles (i.e., the boxes span the interquartile range)
#therefore, the boxes represent the middle 50% of the data
#above the top of the box is 25% of the data
#below the bottom of the box is 25% of the data
#notches display confidence interval around the median

#Median hygiene for women is higher than for men on Day 1; this appears to be significant

#Boxplots also tell us about skew and outliers (the single dots)


#------------Line Graphs------------------

#Remember Bar charts?
chickFlick = read.table("ChickFlick.dat",  header = TRUE, sep="\t", comment.char="", quote="")

plot <- ggplot(chickFlick, aes(film, arousal))

plot + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black")

#Instead of bars, we can also plot this same data using points
plot + stat_summary(fun.y = mean, geom = "point")

#And then we can connect these points with lines
plot + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = 1), colour = "Blue", linetype="dashed")

#Now let's add confidence intervals & change the axis labels
plot + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = 1), colour = "Blue", linetype="dashed") + stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.2) + labs(x = "Film", y = "Mean Arousal")

#-------------Line Graphs for multiple variables & Converting from Repeated Measures Dataframe to "Molten" format-------------

textData <- read.table("TextMessages.dat", header=TRUE, sep="\t", comment.char="", quote="")
head(textData)
#Grammar scores at baseline and after 6 months for children who did and did not send text messages (SMS).
#This data frame is set up in a repeated-measures format: each row = 1 child, but each child provides 2 datapoints.
#We want each datapoint on a row!

#attach the six month scores to the end of the baseline scores...one long vector
GRAMMAR_SCORES <- c(textData$Baseline, textData$Six_months)

#create a factor that records whether each score is from the baseline or six month condition. Same length as GRAMMAR_SCORES!
number_of_children = length(GRAMMAR_SCORES)/2
TIME <- as.factor(c(rep("Baseline",number_of_children), rep("Six_months",number_of_children)))

#Cast the Group column from factor to character vector. Repeat this vector twice. Cast the whole thing as factor.
group_char_vector <- as.character(textData$Group)
GROUP <- as.factor(rep(group_char_vector,2))

#Create a new dataframe.
textData_stacked <- data.frame(GROUP,TIME,GRAMMAR_SCORES)
head(textData_stacked)

#Now we are ready to plot a line graph w/ multiple variables!
line <- ggplot(textData_stacked, aes(TIME, GRAMMAR_SCORES, colour=GROUP))

#Add points for the means
line + stat_summary(fun.y = mean, geom = "point")

#Add lines that connect the means
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = GROUP))

#Add confidence intervals and override default axis and legend labels
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = GROUP)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Time", y = "Mean Grammar Score", colour = "Group")

