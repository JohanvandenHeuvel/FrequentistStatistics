library(ggplot2)
library(ggm)

rm(list=ls(all=TRUE))
###1a Load in the data in Idioms.csv. This data contains results from a rating study on Dutch idioms. Idioms are word sequences like "tegen de lamp lopen" (literally, "walk into the lamp") which have a figurative meaning that is different from the literal one (figuratively, "tegen de lamp lopen" means "to get caught"). There are 3 measured variables in this study: Familiarity (study participants rated how familiar they were with the figurative meaning of the idioms), MC_Correct (study participants' performance on a multiple-choice test where they had to select the correct figurative meaning of the idiom), and Transparency (study participants rated how related the figurative meaning was the literal meaning of the word sequence). "Tegen de lamp lopen" would by a non-transparent idiom, since walking into lamps is not clearly related to getting caught. An example of a transparent idiom would be "tussen twee vuren zitten" ("to sit between two fires"), which figuratively means "to be threatened from both sides."
Idioms = read.table(file.choose(), header=TRUE, sep="\t", quote="", comment.char="", fill=TRUE)


#In this first exercise, analyze the correlations between these 3 variables (don't forget scatterplots). You should report the correlation coefficients as well as whether they are statistically significant. You should also state how strong the effect is.
scatter_fam_trans = ggplot(Idioms, aes(Familiarity, Transparency))
scatter_fam_trans + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

scatter_fam_mc = ggplot(Idioms, aes(Familiarity, MC_Correct))
scatter_fam_mc + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

scatter_trans_mc = ggplot(Idioms, aes(Transparency, MC_Correct))
scatter_trans_mc + geom_point() + geom_smooth(method = "lm", alpha = 0.1)

#Explanation:
#The dataframe consists of 394 objects which is larger than 30, thus it's normally distributed
#It's significant because the p value of 2.2e-16 < alpha value.
#For the first cor.test of familiarity and transparency the cor = 0.52. This means that there is a large positive effect.
cor.test(Idioms$Familiarity, Idioms$Transparency, use="pairwise.complete.obs")
#Explanation:
#It's significant because the p value of 2.2e-16 < alpha value.
#For the cor.test of familiarity and mc correct the cor = 0.58. This means that there is also a large positive effect.
cor.test(Idioms$Familiarity, Idioms$MC_Correct, use="pairwise.complete.obs")

#Explanation:
#It's significant because the p value of 2.2e-16 < alpha value.
#For the cor.test of transparency and mc correct the cor = 0.62. This means that there is also a large positive effect.
cor.test(Idioms$Transparency, Idioms$MC_Correct, use="pairwise.complete.obs")

###1b How much variance is shared by each of the three pairwise combinations of variables?
cor(Idioms$Familiarity, Idioms$Transparency, use="pairwise.complete.obs")^2 * 100
cor(Idioms$Familiarity, Idioms$MC_Correct, use="pairwise.complete.obs")^2 * 100
cor(Idioms$Transparency, Idioms$MC_Correct, use="pairwise.complete.obs")^2 * 100

###2a You want to know how much variance is shared just between each pairwise combination of variables in the idioms data, controlling for the third variable. Perform the proper analyses. State how strong the effects are, how much variance is shared between each pair of variables, and whether the correlation is significant. Hint: you will have to create a subset of the dataframe that includes only complete cases (there is no special arguement for this within the correlation function you will use here)
Idioms_complete = Idioms[complete.cases(Idioms),]
row.names(Idioms_complete) = seq(nrow(Idioms_complete))

#Explanation of familiarity and transparency controlling for mc correct
#The pcor has a result of 0.25, meaning that it has a small to medium positive effect
#The R^2 value is 6.22% So 6.22% variance is shared between familiarity and transparency while controlling for mc correct.
#The pcor.test gives us a p value of 6.018725e-07 which is smaller than our alpha value of 0.05. Thus it's statistically significant
pc_fam_trans <- suppressWarnings(pcor(c("Familiarity","Transparency","MC_Correct"), var(Idioms_complete))); pc_fam_trans
pc_fam_trans^2 * 100
pcor.test(pc_fam_trans, 1, 391)

#Explanation of familiarity and mc correct controlling for transparency
#The pcor has a result of 0.38, meaning it has a medium to large positive effect.
#The R^2 value is 14.4%. So 14.45 variance is shared between familiarity and mc correct while controlling for transparency.
#The pcor.test gives us a p value of 9.145546e-15 which is smaller than our alpha value of 0.05. Thus it's statistically significant
pc_fam_MC <- suppressWarnings(pcor(c("Familiarity","MC_Correct", "Transparency"), var(Idioms_complete))); pc_fam_MC
pc_fam_MC^2 * 100
pcor.test(pc_fam_MC, 1, 391)

#Explanation of mc correct and transparency controlling for familiarity
#The pcor has a result of 0.46, meaning it has a medium to large positive effect
#The R^2 value is 21.5%. So 21.5% variance is shared between mc correct and transparency while controlling for familiarity.
#The pcor.test gives us a p value of 3.865423e-22 which is smaller than our alpha value of 0.05. Thus it's statistically significant
pc_MC_trans <- suppressWarnings(pcor(c("MC_Correct", "Transparency","Familiarity"), var(Idioms_complete))); pc_MC_trans
pc_MC_trans^2 * 100
pcor.test(pc_MC_trans, 1, 391)

###2b In light of the results in question 1, what do the results in question 2a suggest about the relationship between these three variables.

#Explanation:
#In question 1 the variance of familiarity with transparency was 26.8%. But in 2a the variance of these 2 while controlling for mc correct is only 6.22%.
#Variance of familiarity and mc correct is 33.2% and while controlling for transparency it's 14.4%.
#Variance of transparency and mc correct is 38.7% and while controlling for familiarity it's 21.5%

#The partial correlation has shown us that the third variable that wasn't accounted for in the original correlation test does have significant impact on the relation between the three variables. The third variable does have some influence on the other 2 variables.


###2c You think that the transparency of an idiom may be related to how well participants are able to figure out the idiom's meaning in the multiple choice test. Separate from this, you also think that participants may rate transparent idioms as more familiar. Is there evidence that suggests this may be true?

#Explanation of: You think that the transparency of an idiom may be related to how well participants are able to figure out the idiom's meaning in the multiple choice test.

#Yes the correlation between the amount of multiple choice questions being correct and the transparency of the idiom has been calculated in question 1. The correlation is 0.62 which means there is a large positive effect. The variance of these 2 variables is 38.7%, but we should look at the partial correlation of 2a where the partial correlation is 0.46, which is a less of a large effect but still significant enough. Also the variance is 21.5%. So the transparency of an idiom definitely is related to the amount of correct multiple choice answers

#Explanation of: You also think that participants may rate transparent idioms as more familiar.

#The correlation of familiarity and transparency is 0.52, which is a large positive effect and the variance of these 2 variables is  26.8%. However by looking at the partial correlation we see that it's 0.25 which is a small to medium effect. Furthermore the partial variance is now 6.22%. So there is a relation between familiarity and transparency but not that big. This is logical because being familiar with an idiom and the idiom being transparent shouldn't have that much of an influence and that's what we see in these partial correlations.


###3a In this exercise, you will be using ordinal data. Load in the data from singing.csv. This data is from a singing competition where contestants sing songs originally recorded by other artists. The column "Place" indicates the results: 1=the winner of the competition, 12=the person who came in last place. The YouTube_Song_Likes column indicates how many "likes" the song (the version by the original singer) received on YouTube. First, create a plot of the datapoints; include a smoother (using the "lm" method) and add nicer axis labels than the default. Then run two different correlation analyses of the data using two different methods. This time, you don't need to include significance tests.
Singing = read.table(file.choose(), header=TRUE, sep="\t", quote="", comment.char="", fill=TRUE)

Singing_plot = ggplot(Singing, aes(Place, YouTube_Song_Likes))
Singing_plot + geom_point() + geom_smooth(method = "lm", alpha = 0.1) + labs(title = "Scatterplot of place and amount of YouTube likes of the song", x = "Place in competition", y = "Amount of YouTube likes of the song") + scale_x_continuous(breaks = 1:12)

cor(Singing$Place, Singing$YouTube_Song_Likes, method = "spearman")
cor(Singing$Place, Singing$YouTube_Song_Likes, method = "kendall")

###3b How does the slope of the smoother in your scatterplot compare to the pattern of the data points in the scatterplot? Why do you think this is?

#Explanation:                                                                                                                         The smoother doesn't show a very negative correlation, despite seeing an obvious trend in decrease of YouTube likes and place in the competition. This is because there are 2 outliers, namely the first and last place that disrupt the data.


###3c Try removing any data points that appear to be outliers and then rerun the correlation analyses from 3a. What does this tell you about differences between the two types of analysis for ordinal data?
Singing_with_NA = Singing
Singing_with_NA$YouTube_Song_Likes[5] = NA
Singing_with_NA$YouTube_Song_Likes[2] = NA

cor(Singing_with_NA, use = "pairwise.complete.obs", method = "spearman")
cor(Singing_with_NA, use = "pairwise.complete.obs", method = "kendall")

#Explanation:                                                                                                                         When calculating the correlation with the full dataset the results were: Spearman = -0.13 and Kendall = -0.27, but when calculating the correlation without the outliers of 1st and last place. We get the results: Spearman = -0.867 and Kendall = -0.96.                Kendall's tau is always more negative than Spearman's rho. This is because Kendall should rather be used when the dataset is small and when the scores have the same rank. This is not the case for the singing dataframe. 


###4 Why does the covariance value grow in the positive direction when there is a positive trend in your scatterplot and in the negative direction when there is a negative trend in your scatterplot?

#Explanation:                                                                                                                         If the variance is above 0 it means that there is a positive correlation. Meaning that the increase of one variable causes the increase of the other variable. In the scatterplot you can clearly see if there is a negative or positive correlation. If the line is positively increasing then it means that the higher the X goes then the Y will also become higher. The variance shows you just how big of an effect there is. This effect could be positive or negative. A big covariance value like 0.5 has a larger effect on the smoother line of the scatterplot than a value of 0.1. So if your line is more linear then it means the covariance value must be bigger as well. Vice versa for negative linear lines and its covariance value.
