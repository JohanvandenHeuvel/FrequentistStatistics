library(ggplot2)
library(car)
library(pastecs)

rm(list=ls(all=TRUE))
#1 Load Gifts.csv. Remember that this data reports how many gifts different people give to themselves versus to their partner (over a 5 year period). In the week 4 practical, you were asked to plot a clustered error bar chart of the mean number of gifts given to oneself versus to one's partner for male and female. Here, you are going to create another bar chart using these variables. However, this time you will include gender as a facet so that there are different plots for male and female, and these plots are placed side-by-side.
#We did not discuss this in the lecture, so you will have to look at chapter 4 of the book to figure out how to do this. Include confidence intervals and don't include the legend denoting which color is partner vs. self (since this information will be apparent on the graph). Note that the opts() function, which is used in the book example, has been deprecated, so you can no longer use it to suppress the legend--however, you can use the theme() function with the same argument that opts() used to take.
gifts_dataframe = read.table(file.choose(), header=TRUE, sep="\t", quote="", comment.char="", fill=TRUE)
gifts_bar = ggplot(gifts_dataframe,aes(Recipient, Number.of.Gifts, colour = Gender))
gifts_bar + stat_summary(fun.y = mean, geom = "bar", position = "dodge", fill ="white" ) + stat_summary(fun.data = mean_cl_normal, geom = "errorbar" , width=0.2) + facet_grid(.~gifts_dataframe$Gender) + labs(title = "Mean of number of gifts from females and males" ,x = "Recipient", y = "Mean of number of gifts") + theme(legend.position = "none")

#2 Now create a line graph with the Gifts.csv data. Each gender should have its own color/line. Include confidence intervals.
linegraph_gifts = ggplot(gifts_dataframe, aes(Recipient, Number.of.Gifts, colour = Gender))
linegraph_gifts + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = Gender), linetype="solid") + stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.2)

#3 Load in the data from powerplant.csv. This data contains information on the power output of a powerplant as well as other factors effecting the powerplant's performance. You are going to create a grouped scatterplot. On the x-axis will be z-scores and on the y-axis will be power output (from the column POWER_OUTPUT). You will use different colors to group according to measurement type: that is, temperature versus ambient pressure. Include confidence intervals and a smoother.
powerplant_dataframe = read.table(file.choose(), header=TRUE, sep="\t", quote="", comment.char="", fill=TRUE)
sub_powerplant = powerplant_dataframe[sample(nrow(powerplant_dataframe),100),]
row.names(sub_powerplant) = seq(nrow(sub_powerplant))

temperature_zScore = (sub_powerplant$TEMPERATURE - mean(sub_powerplant$TEMPERATURE))/ sd(sub_powerplant$TEMPERATURE) 
ambientpressure_zscore = (sub_powerplant$AMBIENT_PRESSURE - mean(sub_powerplant$AMBIENT_PRESSURE)) / sd(sub_powerplant$AMBIENT_PRESSURE)

zscore_temp = data.frame(group = "Temperature", value = c(temperature_zScore))
zscore_ambient = data.frame(group = "Ambient", value = c(ambientpressure_zscore))

total_zscore = rbind(zscore_temp, zscore_ambient)

poweroutputDouble = c(sub_powerplant$POWER_OUTPUT, sub_powerplant$POWER_OUTPUT) 

scatter = ggplot(total_zscore, aes(value, poweroutputDouble))
scatter + geom_point(aes(color = group)) + geom_smooth(method = "lm", alpha = 0.1) + labs(title = "Grouped scatterplot of Temperature and Ambient pressure",x = "Z-score", y = "Power Output") 


#Note that the TEMPERATURE and AMBIENT_PRESSURE columns contain actual scores, so you will first need to convert them to z-scores so that they are measured in terms of the same unit. 
#Furthermore, the data set is way too big, so a scatterplot with all the datapoints would be severely overplotted. Take a random sample of 100 rows of the dataframe and work with this data only. 
#Hint: to create the scatterplot, you will need to reorganize the dataframe so a single datapoint is in each row (i.e., you should end up with 200 rows)!

#What patterns do you observe in the data?

#Explanation:
#Temperature's z-score of power output starts from -2 and high and along the way it ends up low but with a higher z-score
#So a higher z-score for temperature means lower power output and a negative z-score means higher power output.

#Similarly Ambient pressure's power output increases when the z-score increases as well.
#However since this is a scatterplot you can see that some points are outliers and are scattered around.

#4 Load in the data from class_grades.csv. This dataframe contains grades from 4 different classes. Crucially, the classes are "nested" inside of schools: class1 and class2 are nested in schoolA, and class3 and class4 are nested in schoolB. In other words, this means that the grades for classes 1 and 2 together make up the grades for schoolA. Create a boxplot comparing the medians and dispersions of the two schools.
class_gradesdf = read.table(file.choose(), header=TRUE, sep="\t", quote="", comment.char="", fill=TRUE)
class_boxplot = ggplot(class_gradesdf, aes(SCHOOL, GRADE, colour = SCHOOL)) 
class_boxplot + geom_boxplot(aes(colour = SCHOOL)) + labs(title = "Boxplot of grades from schools A and B")

#5 Assess the normality of the grades from the two schools by looking at graphs (the 2 types discussed in lecture; for histograms, experiment with different binwidths to find one that looks good)
#; by inspecting the skew and kurtosis values; and by performing a statistical test for normality. In addition, using a statistical test, assess the homogeneity of variance between the two schools. For the statistical tests, provide a proper reporting of the results.
schoolA = class_gradesdf[which(class_gradesdf$SCHOOL == "schoolA"),]
schoolB = class_gradesdf[which(class_gradesdf$SCHOOL == "schoolB"),]

class_histA = ggplot(schoolA, aes(GRADE))
class_histA + geom_histogram(aes(y= ..density..), colour ="Black", fill ="white", binwidth = 1.5) + stat_function(fun = dnorm, args = list(mean = mean(schoolA$GRADE, na.rm = TRUE), sd = sd(schoolA$GRADE, na.rm = TRUE)), colour = "black", size = 1)

class_histB = ggplot(schoolB, aes(GRADE))
class_histB + geom_histogram(aes(y= ..density..), colour ="Black", fill ="white", binwidth = 1.5) + stat_function(fun = dnorm, args = list(mean = mean(schoolB$GRADE, na.rm = TRUE), sd = sd(schoolB$GRADE, na.rm = TRUE)), colour = "black", size = 1)

qqplot_schoolA = qplot(sample = schoolA$GRADE, stat = "qq")
qqplot_schoolA

qqplot_schoolB = qplot(sample = schoolB$GRADE, stat = "qq")
qqplot_schoolB

stat.desc(cbind(schoolA$GRADE,schoolB$GRADE), basic = FALSE, norm = TRUE)

shapiro.test(schoolA$GRADE)
shapiro.test(schoolB$GRADE)

leveneTest(class_gradesdf$GRADE,class_gradesdf$SCHOOL)

#Explanation:
#According to a Shapiro Wilk test, the grades of schoolA, W = 0.96715 and p-value = 2.469e-06 were significantly non-normal
#According to a Shapiro Wilk test, the grades of schoolB, W = 0.99093, p-value = 0.06112 were significantly normal
#For the grade scores, the variances were similar for the two schools, F(1,598) = 6,5, p = 0.01. So it's heterogenous
#SchoolA is significantly non normally distributed and schoolB is significantly normally distributed
#According to the shapiro test. In the histogram of school B it's pretty straight forward that it's normally distributed
#However in the histogram of school A it's harder to tell.



#6 Now, assess the normality for class1 and class2, and then for class3 and class4. We'll skip homogeneity of variance here (I'll just tell you that they're homogenous :-))
class1 = class_gradesdf[which(class_gradesdf$CLASS == "class1"),]
class2 = class_gradesdf[which(class_gradesdf$CLASS == "class2"),]
class3 = class_gradesdf[which(class_gradesdf$CLASS == "class3"),]
class4 = class_gradesdf[which(class_gradesdf$CLASS == "class4"),]

class1_hist = ggplot(class1, aes(GRADE))
class1_hist + geom_histogram(aes(y= ..density..), colour ="Black", fill ="white", binwidth = 4.5) + stat_function(fun = dnorm, args = list(mean = mean(class1$GRADE, na.rm = TRUE), sd = sd(class1$GRADE, na.rm = TRUE)), colour = "black", size = 1)

class2_hist = ggplot(class2, aes(GRADE))
class2_hist + geom_histogram(aes(y= ..density..), colour ="Black", fill ="white", binwidth = 4) + stat_function(fun = dnorm, args = list(mean = mean(class2$GRADE, na.rm = TRUE), sd = sd(class2$GRADE, na.rm = TRUE)), colour = "black", size = 1)

class3_hist = ggplot(class3, aes(GRADE))
class3_hist + geom_histogram(aes(y= ..density..), colour ="Black", fill ="white", binwidth = 3.5) + stat_function(fun = dnorm, args = list(mean = mean(class3$GRADE, na.rm = TRUE), sd = sd(class3$GRADE, na.rm = TRUE)), colour = "black", size = 1)

class4_hist = ggplot(class4, aes(GRADE))
class4_hist + geom_histogram(aes(y= ..density..), colour ="Black", fill ="white", binwidth = 5) + stat_function(fun = dnorm, args = list(mean = mean(class4$GRADE, na.rm = TRUE), sd = sd(class4$GRADE, na.rm = TRUE)), colour = "black", size = 1)

qqplot_class1 = qplot(sample = class1$GRADE, stat = "qq")
qqplot_class1

qqplot_class2 = qplot(sample = class2$GRADE, stat = "qq")
qqplot_class2

qqplot_class3 = qplot(sample = class3$GRADE, stat = "qq")
qqplot_class3

qqplot_class4 = qplot(sample = class4$GRADE, stat = "qq")
qqplot_class4

stat.desc(cbind(class1$GRADE,class2$GRADE,class3$GRADE,class4$GRADE), basic = FALSE, norm = TRUE)

shapiro.test(class1$GRADE)
shapiro.test(class2$GRADE)
shapiro.test(class3$GRADE)
shapiro.test(class4$GRADE)

#Explanation:
#Classes 1 and 2 are normally distributed, however 3 and 4 are not.
#This is because 1 and 2 have a normally distributed histogram but 3 is skewered to the right and 4 skewered to the left
#Furthermore the QQ plots of 1 and 2 are are looking normally distributed but 3 and 4 aren't
#Now with the statistical tests class 1's p value = 0.4 so it's non significant, class 2's p value = 0.19 so also non significant
#Class 3's p value = 7.27e-06 so it's significant, class 4's p value = 1.851e-07 so also significant.


#7 How do the patterns that you've seen at the class-level distributions contribute to the patterns that you've seen at the school-level distributions?
#It might help to consider where the means of class 1 and class 2 are compared to each other, and where the means of class 3 and class 4 are compared to each other.

#Explanation:
#The mean of class 1 = 69,7 class 2 = 84,4, class 3 = 75,1, class 4 = 75,3
#So the means of classes 3 and 4 are very similar and the classes of 1 and 2 however are not.
#This explains why school B is normally distributed and school A is not.
mean(class1$GRADE)
mean(class2$GRADE)
mean(class3$GRADE)
mean(class4$GRADE)
