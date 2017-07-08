library(ggplot2)
library(ggm)
setwd("~/Documents/RU/Sem2/Statistics/Week6")
###1a Load in the data in Idioms.csv. 
#This data contains results from a rating study on Dutch idioms. 
#Idioms are word sequences like "tegen de lamp lopen" (literally, "walk into the lamp") which have a figurative meaning that is different from the literal one (figuratively, "tegen de lamp lopen" means "to get caught"). 
#There are 3 measured variables in this study: Familiarity (study participants rated how familiar they were with the figurative meaning of the idioms), MC_Correct (study participants' performance on a multiple-choice test where they had to select the correct figurative meaning of the idiom), and Transparency (study participants rated how related the figurative meaning was the literal meaning of the word sequence). 
#"Tegen de lamp lopen" would by a non-transparent idiom, since walking into lamps is not clearly related to getting caught. 
#An example of a transparent idiom would be "tussen twee vuren zitten" ("to sit between two fires"), which figuratively means "to be threatened from both sides."
idiom = read.table("Idioms.csv", sep="\t", header=TRUE, comment.char="", quote="")
summary(idiom)

#In this first exercise, analyze the correlations between these 3 variables (don't forget scatterplots). 
#You should report the correlation coefficients as well as whether they are statistically significant. 
#You should also state how strong the effect is.

nrow(idiom) # 394 datapoints (CENTRAL LIMIT THEOREM)
cor(idiom[,c("Familiarity", "Transparency", "MC_Correct")], use = "pairwise.complete.obs")
#There was a significant, high correlation between Familiarity, Transparency of r = 0.52. It is significant because the data has 394 datapoints
#There was a significant, high correlation between Familiarity, MC_Correct of r = 0.58. It is significant because the data has 394 datapoints
#There was a significant, high correlation between Transparency, MC_Correct of r = 0.62. It is significant because the data has 394 datapoints
#P-values are related to effect size which is big enough here, correlation coefficients are effect sizes

ggplot(idiom, aes(Familiarity, Transparency)) + geom_point()
ggplot(idiom, aes(Familiarity, MC_Correct)) + geom_point()
ggplot(idiom, aes(Transparency, MC_Correct)) + geom_point()

###1b How much variance is shared by each of the three pairwise combinations of variables?
cor(idiom[,c("Familiarity", "Transparency", "MC_Correct")], use = "pairwise.complete.obs")^2
#shared variance(coefficient of determination) is the correlation coefficient squared
#Shared variance Familiarity, Transparancy r^2 = 0.27
#Shared variance Familiarity, MC_Correct r^2 = 0.33
#Shared variance Transparancy, MC_Correct r^2 = 0.39


###2a You want to know how much variance is shared just between each pairwise combination of variables in the idioms data, controlling for the third variable. 
#Perform the proper analyses. 
#State how strong the effects are, how much variance is shared between each pair of variables, and whether the correlation is significant. 
#Hint: you will have to create a subset of the dataframe that includes only complete cases (there is no special argument for this within the correlation function you will use here :-( ).

idiom_noNAN <- subset(idiom, !(is.na(Familiarity) | is.na(Transparency) | is.na(MC_Correct)))
nrow(idiom_noNAN) #391

pc1 <- pcor(c("Familiarity", "Transparency", "MC_Correct"), var(idiom_noNAN)); pc1^2
pcor.test(pc1, 1, 391)
#The is a significant, very small shared variance r = 0.06, p < 0.05 between Familiarity and Transparency when controlled for MC_Correct

pc2 <- pcor(c("Familiarity", "MC_Correct", "Transparency"), var(idiom_noNAN)); pc2^2
pcor.test(pc2, 1, 391)
#The is a significant, medium shared variance r = 0.14, p < 0.05 between Familiarity and MC_Correct when controlled for Transparency

pc3 <- pcor(c("Transparency", "MC_Correct", "Familiarity"), var(idiom_noNAN)); pc3^2
pcor.test(pc3, 1, 391)
#The is a significant, high shared variance r = 0.21, p < 0.05 between Transparency and MC_Correct when controlled for Familiarity


###2b In light of the results in question 1, what do the results in question 2a suggest about the relationship between these three variables.
#When controlling for the third variable, MC_Correct, there is almost no shared variance between Familiarity and Transparancy
#When controlling for the third variable, Transparency, half of the shared variance between Familiarity and MC_Correct can be explained by Transparency
#When controlling for the third variable, Familiarity, one third of the shared variance between Transparanct and MC_Correct can be explained by Familiarity

###2c You think that the transparency of an idiom may be related to how well participants are able to figure out the idiom's meaning in the multiple choice test. 
#Separate from this, you also think that participants may rate transparent idioms as more familiar. 
#Is there evidence that suggests this may be true?
#The hypothesis is: The transparency of an idiom is related to how well participants are able to figure out the idiom's meaning in the multiple choice test
#We are interested in the correlation between Transparency of the idiom and the result of MC_Correct, we want to control for Familiarity of course
pc3 <- pcor(c("Transparency", "MC_Correct", "Familiarity"), var(idiom_noNAN)); pc3
pcor.test(pc3, 1, 391)
#There seems to be a significant, large positive correlation between Transparency and MC_correct r = 0.46, p < 0.05, when controlled for Familiarity

#The hypothesis is: Participants rate transparent idioms as more familiar
pc1 <- pcor(c("Transparency", "Familiarity", "MC_Correct"), var(idiom_noNAN)); pc1
pcor.test(pc1, 1, 391)
#There seems to be a significant, medium positive correlation between Transparenct and Familiarity r = 0.25, p < 0.05 when controlled for MC_Correct
#The data supports both of the statements, so the evidence does suggest this is true

###3a In this exercise, you will be using ordinal data. Load in the data from singing.csv. 
#This data is from a singing competition where contestants sing songs originally recorded by other artists. 
#The column "Place" indicates the results: 1=the winner of the competition, 12=the person who came in last place. 
#The YouTube_Song_Likes column indicates how many "likes" the song (the version by the original singer) received on YouTube. 
#First, create a plot of the datapoints; include a smoother (using the "lm" method) and add nicer axis labels than the default. 
#Then run two different correlation analyses of the data using two different methods. 
#This time, you don't need to include significance tests.

singing = read.table("singing.csv", sep="\t", header=TRUE, comment.char="", quote="")
summary(singing)
head(singing)
nrow(singing) #12

ggplot(singing, aes(Place, YouTube_Song_Likes)) + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Place in competition", y = "Original song likes on YouTube")
cor.test(singing$Place, singing$YouTube_Song_Likes, method = "spearman")
#There appears to be a small correlation between Place and YouTube_Song_likes of rho = -0.13
cor.test(singing$Place, singing$YouTube_Song_Likes, method = "kendall")
#There appears to be a medium correlation between Place and YouTube_Song_likes of tau = -0.27

###3b How does the slope of the smoother in your scatterplot compare to the pattern of the data points in the scatterplot? 
#Why do you think this is?
#The smoother in the scatterplot is much more "neutral" then the pattern in the middle appear to be. This is because there are two outliers


###3c Try removing any data points that appear to be outliers and then rerun the correlation analyses from 3a. 
#What does this tell you about differences between the two types of analysis for ordinal data?
singing <- singing[!singing$Place == 1, ]
singing <- singing[!singing$Place == 12, ]
singing

ggplot(singing, aes(Place, YouTube_Song_Likes)) + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Place in competition", y = "Original song likes on YouTube")
cor.test(singing$Place, singing$YouTube_Song_Likes, method = "spearman")
#There appears to be a very large correlation between Place and YouTube_Song_likes of rho = -0.96
cor.test(singing$Place, singing$YouTube_Song_Likes, method = "kendall")
#There appears to be a very large correlation between Place and YouTube_Song_likes of tau = -0.87

#tau appears to give a lower value of correlation (10%-15%). Spearman gives a much lower p value. Its seems like tau is more reliable but there is not enough evidence to support this. 

###4 Why does the covariance value grow in the positive direction when there is a positive trend in your scatterplot 
#and in the negative direction when there is a negative trend in your scatterplot?

#The formula for covariance is: sum i for n data points ( (xi - x_mean) (yi - y_mean)) / n - 1
#This formula can tell us about the positive and negative trend.
#When the data has a positive trend it goes from left bottom to upper right. Bottom left are negative values, xi and yi are under the corresponding mean.
#Upper right are positive values, xi and yi are above their corresponding mean. This means that for every datapoint we have a positive value, -x * -y = +c
#Thus the covariance value grows in the positive direction.

#For the negative trend we have upper left, -x * y, and bottom right, x * -y. This means that for every datapoint i we get a negative value.
#So the covariance value grows in the negative direction

