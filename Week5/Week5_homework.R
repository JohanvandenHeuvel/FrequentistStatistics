library(ggplot2)
library(car)
library(pastecs)


#1 Load Gifts.csv. Remember that this data reports how many gifts different people give to themselves versus to their partner (over a 5 year period). 
#In the week 4 practical, you were asked to plot a clustered error bar chart of the mean number of gifts given to oneself versus to one's partner for male and female. 
#Here, you are going to create another bar chart using these variables. 
#However, this time you will include gender as a facet so that there are different plots for male and female, and these plots are placed side-by-side. 
#We did not discuss this in the lecture, so you will have to look at chapter 4 of the book to figure out how to do this. 
#Include confidence intervals and don't include the legend denoting which color is partner vs. self (since this information will be apparent on the graph). 
#Note that the opts() function, which is used in the book example, has been deprecated, so you can no longer use it to suppress the legend--however, you can use the theme() function with the same argument that opts() used to take.

setwd("~/Documents/RU/Sem2/Statistics/Week5")
Gifts <- read.table("Gifts.csv", sep="\t", header=TRUE, comment.char="", quote="")
head(Gifts)

bar_gifts <- ggplot(Gifts, aes(Recipient,Number.of.Gifts, fill = Gender))
bar_gifts + labs(y = "mean of gifts") + stat_summary(fun.y = mean, geom = "bar", colour = "Black", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange", position=position_dodge(width=0.90)) + theme(legend.position = "none") + facet_grid(. ~ Gender)


#2 Now create a line graph with the Gifts.csv data. Each gender should have its own color/line. Include confidence intervals.
bar_gifts <- ggplot(Gifts, aes(Recipient,Number.of.Gifts, colour = Gender))
bar_gifts + labs(y = "mean of gifts") + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = Gender)) + stat_summary(fun.data = mean_cl_normal, geom = "pointrange")

#3 Load in the data from powerplant.csv. 
#This data contains information on the power output of a powerplant as well as other factors effecting the powerplant's performance. 
#You are going to create a grouped scatterplot. 
#On the x-axis will be z-scores and on the y-axis will be power output (from the column POWER_OUTPUT). 
#You will use different colors to group according to measurement type: that is, temperature versus ambient pressure. 
#Include confidence intervals and a smoother.
Powerplant <- read.table("powerplant.csv", sep="\t", header=TRUE, comment.char="", quote="")

Powerplant$TEMPERATURE <- scale(Powerplant$TEMPERATURE)
Powerplant$AMBIENT_PRESSURE <- scale(Powerplant$AMBIENT_PRESSURE)

#Dont use EXHAUST_VACUUM or RELATIVE_HUMIDITY

Z_SCORES <- c(Powerplant$TEMPERATURE,Powerplant$AMBIENT_PRESSURE)
LABELS <- as.factor(c(rep("TEMPERATURE",nrow(Powerplant)), rep("AMBIENT_PRESSURE",nrow(Powerplant))))

Powerplant_new <- data.frame(Z_SCORES, LABELS,Powerplant$POWER_OUTPUT)
head(Powerplant_new)

Sample <- Powerplant_new[sample(nrow(Powerplant_new), 100), ]
Sample

Sample_plot <- ggplot(Sample, aes(x = Z_SCORES,y = Powerplant.POWER_OUTPUT, color = LABELS))

#Why missing values?
Sample_plot + geom_point()  + geom_smooth(method = "lm", se = F, aes(fill = LABELS)) + labs(y = "Power output", x = "Z-scores")
# + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") doest not work:
#Warning message:
#Removed 98 rows containing missing values (geom_pointrange). 
#TA said I could leave it out and that it was not in the answers of this exercise either

#Note that the TEMPERATURE and AMBIENT_PRESSURE columns contain actual scores, 
#so you will first need to convert them to z-scores so that they are measured in terms of the same unit. 
#Furthermore, the data set is way too big, so a scatterplot with all the datapoints would be severely overplotted. 
#Take a random sample of 100 rows of the dataframe and work with this data only. 
#Hint: to create the scatterplot, 
#you will need to reorganize the dataframe so a single datapoint is in each row (i.e., you should end up with 200 rows)!

#What patterns do you observe in the data?
#There is a negative correlation between temperature and power output and 
#a possitive correlation between ambient pressure and power output

#4 Load in the data from class_grades.csv. This dataframe contains grades from 4 different classes. 
#Crucially, the classes are "nested" inside of schools: class1 and class2 are nested in schoolA, and class3 and class4 are nested in schoolB. 
#In other words, this means that the grades for classes 1 and 2 together make up the grades for schoolA. 
#Create a boxplot comparing the medians and dispersions of the two schools.
class_grades <- read.table("class_grades.csv", sep="\t", header=TRUE, comment.char="", quote="")
head(class_grades)

school_boxplot <- ggplot(class_grades, aes(SCHOOL, GRADE))
school_boxplot + geom_boxplot()

#5 Assess the normality of the grades from the three schools by looking at graphs (the 2 types discussed in lecture; 
#for histograms, experiment with different binwidths to find one that looks good); 
#by inspecting the skew and kurtosis values; 
#and by performing a statistical test for normality. 
#In addition, using a statistical test, assess the homogeneity of variance between the two schools.
#For the statistical tests, provide a proper reporting of the results.

schoolA <- subset(class_grades, class_grades$SCHOOL == "schoolA")
schoolB <- subset(class_grades, class_grades$SCHOOL == "schoolB")

schoolA_historgram <- ggplot(schoolA,aes(GRADE)) + geom_histogram(aes(y=..density..), binwidth = 0.8, colour = "black", fill = "white")+labs(x="Rating", y="Density")+theme(legend.position = "none");    
mean_and_sd_A <- list(mean=mean(schoolA$GRADE,na.rm=TRUE),sd=sd(schoolA$GRADE,na.rm=TRUE))  
schoolA_historgram + stat_function(fun=dnorm,args=mean_and_sd_A,colour="black", size=1)


schoolB_historgram <- ggplot(schoolB,aes(GRADE)) + geom_histogram(aes(y=..density..), binwidth = 0.8, colour = "black", fill = "white")+labs(x="Rating", y="Density")+theme(legend.position = "none");    
mean_and_sd_B <- list(mean=mean(schoolB$GRADE,na.rm=TRUE),sd=sd(schoolB$GRADE,na.rm=TRUE))  
schoolB_historgram + stat_function(fun=dnorm,args=mean_and_sd_B,colour="black", size=1)

schoolA_qq <- qplot(sample = schoolA$GRADE, stat = "qq")
schoolA_qq

schoolB_qq <- qplot(sample = schoolB$GRADE, stat = "qq")
schoolB_qq

shapiro.test(schoolA$GRADE)
shapiro.test(schoolB$GRADE)

stat.desc(schoolA$GRADE, basic = FALSE, norm = TRUE)
#School A is not normal distibuted:
#The historgram is pointy (kurtosis) and on both sides is a part missing, its not from end to end but somewhere in the middle
#The qq plot supports this, the plot is almost shaped like a fancy f. It is not a nice diagonal line so not normal
#The result of the shapiro test is W = 0.96715, p-value = 2.469e-06 so p < 0.05 so we can assume non-normal
#All of the used methods here support the assumption that the data is non-normal

stat.desc(schoolB$GRADE, basic = FALSE, norm = TRUE)
#School B is normal distibuted:
#The historgram is pointy but the rest looks normal
#The qq plot is close to a linear line, but its not completly normal
#The shapiro test results : W = 0.99093, p-value = 0.06112 where p > 0.05 so we can assume normality
#The tests support the assumption that school B is normal distibuted

leveneTest(class_grades$GRADE, class_grades$SCHOOL)
#For the scores on the GRADE, F = 6.4623, p = 0.01127 p < 0.05 so it is significant

#6 Now, assess the normality for class1 and class2, and then for class3 and class4. We'll skip homogeneity of variance here (I'll just tell you that they're homogenous :-))
#How is meant "assess the normality for class1 and class2, and then for class3 and class4"? class1 and 2 together? Because that is the same as for schoolA if I am not mistaken
#CLASS 1
class <- subset(class_grades, class_grades$CLASS == "class1")

mean(class$GRADE,na.rm=TRUE)

class_historgram <- ggplot(class,aes(GRADE)) + geom_histogram(aes(y=..density..), binwidth = 0.8, colour = "black", fill = "white")+labs(x="Rating", y="Density")+theme(legend.position = "none");    
mean_and_sd_ <- list(mean=mean(class$GRADE,na.rm=TRUE),sd=sd(class$GRADE,na.rm=TRUE))  
class_historgram + stat_function(fun=dnorm,args=mean_and_sd_,colour="black", size=1)

class_qq <- qplot(sample = class$GRADE, stat = "qq")
class_qq

shapiro.test(class$GRADE)

#Histogram has a little skew to the left and has some kurtosis
#QQ test loks normal
#shapiro: W = 0.99037, p-value = 0.3983 so p > 0.05 so we can assume normality

#CLASS 2
class <- subset(class_grades, class_grades$CLASS == "class2")

mean(class$GRADE,na.rm=TRUE)

class_historgram <- ggplot(class,aes(GRADE)) + geom_histogram(aes(y=..density..), binwidth = 0.8, colour = "black", fill = "white")+labs(x="Rating", y="Density")+theme(legend.position = "none");    
mean_and_sd_ <- list(mean=mean(class$GRADE,na.rm=TRUE),sd=sd(class$GRADE,na.rm=TRUE))  
class_historgram + stat_function(fun=dnorm,args=mean_and_sd_,colour="black", size=1)

class_qq <- qplot(sample = class$GRADE, stat = "qq")
class_qq

shapiro.test(class$GRADE)

#Histogram has some skew to the right
#QQ plot looks normal, but is somewhat more to the left then normal
#shapiro: W = 0.98728, p-value = 0.1871 so p > 0.05 so we can asusme normality

#CLASS 3
class <- subset(class_grades, class_grades$CLASS == "class3")

mean(class$GRADE,na.rm=TRUE)

class_historgram <- ggplot(class,aes(GRADE)) + geom_histogram(aes(y=..density..), binwidth = 0.8, colour = "black", fill = "white")+labs(x="Rating", y="Density")+theme(legend.position = "none");    
mean_and_sd_ <- list(mean=mean(class$GRADE,na.rm=TRUE),sd=sd(class$GRADE,na.rm=TRUE))  
class_historgram + stat_function(fun=dnorm,args=mean_and_sd_,colour="black", size=1)

class_qq <- qplot(sample = class$GRADE, stat = "qq")
class_qq

shapiro.test(class$GRADE)

#Histogram has heavy skew to the left
#The qq plot is not normal
#shapiro: W = 0.94182, p-value = 7.27e-06 so p < 0.05 so we can assume non-normal

#CLASS 4 
class <- subset(class_grades, class_grades$CLASS == "class4")

mean(class$GRADE,na.rm=TRUE)

class_historgram <- ggplot(class,aes(GRADE)) + geom_histogram(aes(y=..density..), binwidth = 0.8, colour = "black", fill = "white")+labs(x="Rating", y="Density")+theme(legend.position = "none");    
mean_and_sd_ <- list(mean=mean(class$GRADE,na.rm=TRUE),sd=sd(class$GRADE,na.rm=TRUE))  
class_historgram + stat_function(fun=dnorm,args=mean_and_sd_,colour="black", size=1)

class_qq <- qplot(sample = class$GRADE, stat = "qq")
class_qq

shapiro.test(class$GRADE)

#histogram: heavy skew to the left
#QQ plot is not normal, strong curve to in bottom-left corner
#shapiro: W = 0.91908, p-value = 1.851e-07 so p < 0.05 so we can assume non-normal

#7 How do the patterns that you've seen at the class-level distributions contribute to the patterns that you've seen at the school-level distributions? 
#It might help to consider where the means of class 1 and class 2 are compared to each other, and where the means of class 3 and class 4 are compared to each other.
#mean: class 1 =  84.37078, class 2 = 69.70083
#mean: class 3 =  75.09728, class 4 = 75.27297
#The means are much closer for schoolB then for schoolA. However if we look at the normality of the classes we have class1 and class2 as normal and class3 and class4 not.
#This is in contrast with the normality at school level where schoolA is non-normal and schoolB is normal.
#I assume this is because class3 and class4 even out the differecens between them when combined.