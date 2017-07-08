#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 4 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#Set the working directory
#
#NOTE ALL FILES THAT YOU MUST LOAD IN BELOW USE THE TAB AS SEPARATOR!


######Initiate packages


#If you don't have ggplot2 installed then use:
install.packages(c("ggplot2", "plyr","reshape","lattice", "Hmisc"))


#Initiate ggplot2
library(ggplot2)
library(reshape)
library(plyr)
library(lattice)
library(Hmisc)

#--------Bar Charts----------
#
#Bar Charts are useful for plotting means of different groups, but default behavior is for counts:

GENDER <- c("male","male","female","female","female")
counts_bars <- ggplot(data.frame(GENDER), aes(GENDER))
counts_bars + geom_bar()


#How do we use them to plot means?

chickFlick = read.table("ChickFlick.dat",  header = TRUE, sep="\t", comment.char="", quote="")
head(chickFlick)
#film: a 'chick flick' (Bridget Jones Diary) versus not a 'chick flick' (Memento)
#arousal: physical arousal of participant (how much they enjoyed the film)

bar <- ggplot(chickFlick, aes(film, arousal))

#we must use a "stat" to override the default behavior of plotting counts. Here, we use the stat_summary() stat. The stat_summary() stat takes the following general form:
#
#stat_summary(function=x, geom=y)   ...whereby we must specify a statistical function and a geometric object
#
#
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black")

#For plotting means, we assign the mean function to the fun.y argument (which handles single value function output). We want the "bar" geom. (Note that we define our layer in terms of the stat, with the geom as an argument value of the stat)




#If we want to add confidence intervals, we add another layer using the stat_summary() stat again. 
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange")

#This time, we assign the mean_cl_normal function to the fun.data argument (which handles multiple value function output). We want the "pointrange" geom.




#Override default axis labels
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Film", y = "Mean Arousal")




#---------------Bar Charts with different genders colored--------------

#set fill aesthetic to gender variable. This means that geoms at subsequent layers will be filled according to gender
bar <- ggplot(chickFlick, aes(film, arousal, fill = gender))



#Set position to dodge in stat_summary. This forces male and female bars to stand side-by-side
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge")



#Add additional stat_summary level for confidence intervals. Uh oh! They're printed on top of each other.
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange")


#Gotta make the confidence intervals dodge each other too! May need to play with the width to get the positioning of the bars just right.
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange", position=position_dodge(width=0.90))



#This time, use "errorbar" geom (notice different format from "pointrange" geom).
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90))



#Error bars are too wide! Use width argument to make them narrower.
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2)



#Change default labels for axes and for the title of the legend. If you omit these labels, the axes will be assigned the names of the variables
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Film", y = "Mean Arousal", fill = "Gender")



#Note that a less efficient option would be to define our x, y and fill variables not in the graph object declaration, but at each individual level. This code produces the exact same graph as above.
bar <- ggplot(chickFlick)

bar + stat_summary(aes(film, arousal, fill = gender ), fun.y = mean, geom = "bar", position="dodge") + stat_summary(aes(film, arousal, fill = gender ), fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Film", y = "Mean Arousal", fill = "Gender")



#You can add a layer with scale_fill_manual() that allows you to customize the specific colors that you want a variable to use (here, Gender). Note that this variable should be one that you've already told R to vary color by at other levels.
bar <- ggplot(chickFlick, aes(film, arousal, fill = gender))

bar + stat_summary(fun.y = mean, geom = "bar", position="dodge")+ stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Film", y = "Mean Arousal", fill = "Gender") + scale_fill_manual("Gender", values=c(Female = "Blue", Male = "Green"))



#You can even pass RGB color codes!
bar <- ggplot(chickFlick, aes(film, arousal, fill = gender))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Film", y = "Mean Arousal", fill = "Gender") + scale_fill_manual("Gender", values=c("Female" = "#3366FF", "Male" = "#336633"))



#--------Scatterplot----------
#

setwd("~/Documents/RU/Sem2/Statistics/Week4")
#Scatterplots represent individual datapoints measured along 2 dimensions simultaneously

facebookData <- read.table("FacebookNarcissism.dat",  header = TRUE, sep="\t", comment.char="", quote="")
#Participants rate facebook users according to different criteria based on their profile picture

head(facebookData)
#NPQC_R_Total: score on narcissism questionnaire (facebook user)
#rating: rating given by others based on profile picture (1-5)

graph <- ggplot(facebookData, aes(NPQC_R_Total, Rating))                            #specify plot object; pass data; NPQC_R_TOTAL on x-axis, Rating on y-axis

graph + geom_point()                                                                #add a layer

graph + geom_point() + labs(title = "Facebook Data Scatterplot")                    #add another layer

graph + geom_point() + labs(title = "Facebook Data Scatterplot", x="Narcissism", y="Rating")     #specify axes and title

graph + geom_point(shape = 17) + labs(title = "Facebook Data Scatterplot")             #override point shape default (p.126)

graph + geom_point(size = 6) + labs(title = "Facebook Data Scatterplot")              #override point size default (shape default again)

graph + geom_point(aes(colour = Rating_Type)) + labs(title = "Facebook Data Scatterplot")    #color function of rating type


graph2 <- ggplot(facebookData, aes(NPQC_R_Total, Rating, colour=Rating_Type))        #could also define color at top level!

graph2 + geom_point()                                                                #then don't need to specify color within the individual levels


graph + geom_point(aes(colour = Rating_Type), position = "jitter") + labs(title = "Facebook Data Scatterplot")
#avoid overplotting

graph + geom_point(aes(shape = Rating_Type), position = "jitter", size=4) + labs(title = "Facebook Data Scatterplot")               #avoid overplotting                                               

ggsave("facebook_narcissim_plot.png")




#--------Scatterplots, example 2----------
examData <- read.table("Exam Anxiety.dat",  header = TRUE, sep="\t", comment.char="", quote="")
head(examData)
#Revise: hours spent revising the exam
#Anxiety: anxiety score
#Exam: exam percentage



#Simple scatter (note relationship in data)
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + labs(x = "Exam Anxiety", y = "Exam Performance %") 


#Simple scatter with smoother (i.e., a curved line that summarizes the data)
scatter + geom_point() + geom_smooth() + labs(x = "Exam Anxiety", y = "Exam Performance %") 



#Simple scatter with regression line (i.e., a smoother that is a straight line)
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Exam Anxiety", y = "Exam Performance %") 


#Simple scatter with regression line + confidence interval around line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red")+ labs(x = "Exam Anxiety", y = "Exam Performance %") 



#Simple scatter with regression line + coloured confidence interval (default value for alpha is too dark so set it to 0.1, which makes the confidence interval more transparent)
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", alpha=0.1, fill = "Red") + labs(x = "Exam Anxiety", y = "Exam Performance %") 



#Grouped scatter with regression lines & confidence intervals for each gender
scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))

scatter + geom_point() + geom_smooth(method = "lm") + labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender")


#change the colour of the confidence intervals to match the colours of the data points/regression lines
scatter + geom_point() + geom_smooth(method = "lm", aes(fill = Gender), alpha = 0.1) + labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender") 










###PRACTICAL EXERCISES

setwd("~/Documents/RU/Sem2/Statistics/Week4")

#1 Load in the data from Lecturer.csv. This spreadsheet contains interesting data on different lecturers and students. 
LecturerData <- read.table("Lecturer.csv",  header = TRUE, sep="\t", comment.char="", quote="")

#Plot an error bar chart showing the mean number of friends for students and lecturers. 
#Make sure to override the y-axis label so that we know we are measuring means and not counts
head(LecturerData)

bar_friends <- ggplot(LecturerData, aes(job, friends))
bar_friends + labs(y = "mean of friends") + stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90))


#2 Using this same lecturer data, plot an error bar chart showing the *median* alcohol consumption for students and lecturers. 
#This time, make the bars themselves green. Don't forget axis labels. 
#Use a different geom for plotting the confidence intervals than the one you used in question 1. 
#How do the medians compare to the means for alcohol consumption?

bar_alcohol <- ggplot(LecturerData, aes(job, alcohol))
bar_alcohol + labs(y = "median of alcohol") + stat_summary(fun.y = median, geom = "bar", fill = "green", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange", position=position_dodge(width=0.90))

#The medians are lower then the means. So we can assume the second half of the data is comparably higher then the left side, the data is skewed to the right.

#3a Now create a scatterplot with regression lines of alcohol consumption and neuroticism grouped by lecturer/student. 
graph <- ggplot(LecturerData, aes(alcohol, neurotic, color = job))   
graph + geom_point() + geom_smooth(method = "lm", se = F, aes(fill = job))

#3b Now see if you can also plot a regression line that summarizes ALL the data (not grouped by job) in addition to the job-specific regression lines. Remember that if you set colour to vary by job when you define the graph object, this will filter down to all geoms (including the geom_smooth that you use for the general summary regression line!). 
#To avoid this, you can set colour to vary by job only at certain individual levels (i.e., not at the geom_smooth for the general regression line).
graph <- ggplot(LecturerData, aes(alcohol, neurotic, color = job))   
graph + geom_point() + geom_smooth(method = "lm", color = "Red", se = F)

#3c What trend(s) in the data does the general regression line fail to show that the by-job regression lines reveal?
# The general regression line is very similar to the regression line of the lecturer. However it is not a good 
# representation for the students. The gerenal line does show that for the students the neurotic decreases as the alcohol
# consumption decreases


#4 Load Gifts.csv. Plot a clustered error bar chart of the mean number of gifts given to oneself versus to one's partner for male and female. 
#Include confidence intervals. See if you can figure out how to add a scale_fill_manual() layer that customizes the color of the bars.
Gifts <- read.table("Gifts.csv",  header = TRUE, sep="\t", comment.char="", quote="")
head(Gifts)

bar_gifts <- ggplot(Gifts, aes(Recipient,Number.of.Gifts, fill = Gender))
bar_gifts + labs(y = "mean of gifts") + stat_summary(fun.y = mean, geom = "bar", colour = "Black", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange", position=position_dodge(width=0.90))


