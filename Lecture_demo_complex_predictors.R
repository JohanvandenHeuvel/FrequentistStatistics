library(effects)
library(car)
library(gvlma)

###Interaction between two interval/ratio predictors (multiple regression)

#Outcome variable is how crazy someone is
#Predictor 1: how many beers per week that person drinks
#Predictor 2: how many years they've been working at the university
crazy_data <-read.table("crazy_data.csv",sep="\t",header=TRUE,comment.char="",quote="")
head(crazy_data)

#We have added an interaction between two predictors using the ":"
crazy_model <- lm(craziness ~ beers_per_week + years_working_at_uni + beers_per_week:years_working_at_uni, data=crazy_data)

summary(crazy_model)

#Relationship between coefficients and prediction; how crazy are you after 10 beers when you've worked for 15 years?
# y = b0 + b1*beers_per_week + b2*years_working_at_uni + b3*beers_per_week*years_working_at_uni

1997.56 + 30.14*10 + 17.42*15 + -3.03*10*15

#Must drop an interaction before the main effects that make it up
drop1(crazy_model, test="F")


###Check Assumptions!
durbinWatsonTest(crazy_model)

#homoscedasticity normality, and high influence points
par(mfrow=c(2,2))
par(mar=c(4,4,4,4))

par(mfrow=c(1,1))
plot(crazy_model)


#No severe multicollinearity
vif(crazy_model)                #Don't worry about multicollinearity for interaction; it's going to be high since the interaction is a product of 2 predictors which were already included in the model as separate terms


###Graphing

plot(allEffects(crazy_model))

#Create vectors of beers_per_week and years_working_at_uni values
xgrid <-  seq(min(crazy_data$beers_per_week), max(crazy_data$beers_per_week), 0.5)
ygrid <-  seq(min(crazy_data$years_working_at_uni), max(crazy_data$years_working_at_uni), 0.5)

#create dataframe of all possible combinations of the values in these two vectors
all_combos <- expand.grid(beers_per_week=xgrid, years_working_at_uni=ygrid)
head(all_combos)

predicted_outcomes <- predict(crazy_model, newdata=all_combos)


#Put the predicted outcomes into the data frame as a new column
all_combos$craziness <- predicted_outcomes
head(all_combos)

#Now we plot!
ggplot(all_combos, aes(x=beers_per_week, y=years_working_at_uni, z=craziness)) + geom_tile(aes(fill = craziness)) + scale_fill_gradient(low="white", high="black") + labs(x="Beers per Week", y="Years Working at University")

#Note that main effects can't be interpreted independent of the interaction


#Reporting: The final model’s formula was craziness ~ beers_per_week + years_working_at_uni + beers_per_week:years_working_at_uni. All main effects and the two-way interaction were very significant: p<0.001. When years_working_at_uni was at its baseline level (i.e., 0), there was a positive relationship between beers_per_week and craziness. However, this relationship eventually reversed as years_working_at_uni increased. The model was highly significant overall (F(3,96)=36060, p<0.001) and achieved a high variance explanation (mult. R2=0.9991, adj. R2=0.9991). All regression coefficients, as well as their standard errors, t scores, and p-values, are provided in the appendix, and checking of model assumptions revealed no problems.





###Interaction between Nominal and Interval/Ratio predictor

RTs <- read.table("RTs.csv", header=T, sep="\t", row.names=1) # </_inputfiles/05-2_reactiontimes.csv>
head(RTs)

#In this data, we will predict reaction times to word stimuli on the basis of how frequent and familiar the words are; thus more frequent and familiar words should be reacted to faster.

#check levels; hi is ref level since it is alphabetically first
levels(RTs$FAMILIARITY)

#change default level to lo
RTs$FAMILIARITY <- relevel(RTs$FAMILIARITY, "lo"); levels(RTs$FAMILIARITY)


rt_model <- lm(RT ~ FREQUENCY + FAMILIARITY + FREQUENCY:FAMILIARITY, data=RTs)
summary(rt_model)

###plot the effects
plot(allEffects(rt_model))

#Default is dummy contrasts for nominal variable
#      FAMLIARITYmed     FAMILIARITYhi
#lo         0                 0
#med        1                 0
#hi         0                 1
#
# RT = b0 + b1*FREQUENCY + b2*FAMILIARITYhi + b3*FAMILIARITYmed + b4*FREQUENCY*FAMILIARITYhi + b5*FREQUENCY*FAMILIARITYmed
#
#What is the predicted RT when familiarity is med and freq is 3?
#
697.96 + -32.73*3 + -94.76*0 +-66.40*1 + 28.27*3*0 + 21.65*3*1


###What about planned contrasts?

levels(RTs$FAMILIARITY)
contrast1 <- c(-2, 1, 1)
contrast2 <- c(0, 1, -1)

contrasts(RTs$FAMILIARITY) <- cbind(contrast1,contrast2)
contrasts(RTs$FAMILIARITY)

#        contrast1 contrast2
#lo         -2         0
#hi          1         1
#med         1        -1

rt_model_planned <- lm(RT ~ FREQUENCY + FAMILIARITY + FREQUENCY:FAMILIARITY, data=RTs)
summary(rt_model_planned)

#What is the predicted RT when familiarity is med and freq is 3?
#
644.244 + -16.091*3 + -26.859*1 + -14.18*-1 + 8.32*3*1 + 3.314*3*-1


#What does the p-value of the coefficient (beta) of contrast 1 tell us?
#
#It represents the slope of the line connecting the mean of the lo condition with the mean of the means of the hi and med condition. Because this represents a 3-unit change along the contrast1 axis (-2 to 1), the slope is 1/3 the difference between the lo mean and the hi/med mean.

#What does the p-value of the coefficient (beta) of FREQUENCY:FAMILIARITYcontrast1 tell us?
#
#It represents an adjustment on the slopes of the main effects of Familiarity and Frequency. 

plot(allEffects(rt_model_planned))


#Model selection (notice our interaction wasn't significant, so we actually probably want to drop it)
drop1(rt_model_planned, test="F")

rt_model_planned_2 <- lm(RT ~ FREQUENCY + FAMILIARITY, data=RTs)
summary(rt_model_planned_2)


#And we can get statistics for the whole FAMILIARITY predictor, not just the contrasts:
Anova(rt_model_planned_2, type="III")


#Checking assumptions (use GVIF values as you would VIF values)
vif(rt_model_planned_2)

plot(rt_model_planned_2)

durbinWatsonTest(rt_model_planned_2)

#Reporting (for planned contrasts): The final model’s formula was RT ~ FREQUENCY + FAMILIARITY. The overall effect of FAMILIARITY was significant (p=0.015). However, the only planned contrast that was significant was between the low familiarity condition and the medium and high familiarity conditions (p=0.004). Specifically, reaction times in the medium and high familiarity conditions were lower than in the low condition. While the reaction times in the medium condition were higher than in the high condition, this difference was not significant (p=0.52). There was also a marginally significant effect of FREQUENCY (p=0.051); as Frequency increased, reaction times decreased. Finally, the model was significant overall (F(3,51)=6.959, p<0.001) and explained  a large amount of variance (mult. R2=0.2904, adj. R2=0.2487). All regression coefficients, as well as their standard errors, t scores, and p-values, are provided in the appendix. In addition, the appendix includes the results of an F test provided by the Anova() function to investigate the overall effect of the FAMILIARITY predictor. Checking of model assumptions revealed no problems.


###Remember, we also could have done post-hoc tests instead of planned comparisons...
pairwise.t.test(RTs$RT, RTs$FAMILIARITY, p.adjust.method="BH")

#Each value is a p-value that tells us whether the particular pair of factor levels have different means. Notice that this actually gives us more information than the planned comparisons.






###2 Binary predictors

Goggles <- read.table("goggles.csv", sep=",", header=T, comment.char="", quote="")
head(Goggles)

#Our infamous beer goggles data. How attractive are the people that men and women talk to after the men and women have no alcohol versus after they have 4 pints of beer?

#Let's just make this into a binary predictor by excluding the 2 pints condition (to make things easier)
Goggles <- subset(Goggles, alcohol!="2 Pints")

levels(Goggles$alcohol) <- c("4 pints","4 pints","none")

Goggles$alcohol <- relevel(Goggles$alcohol, "none")

goggles_model <- lm(attractiveness ~ gender + alcohol + gender:alcohol, data=Goggles) 

summary(goggles_model)

#By now, we've talked about contrasts several times
#Contrasts are just numbers we assign to levels of our categorical predictors (female = 0, male = 1)
#They allow us to place our non-numerical categories in a numerical multi-dimensional space
#We want to do this so we can connect our categories' means with lines
#The slopes of these lines tell us about how different the means are
#
#Different kinds: dummy, custom
#R uses dummy contrasts by default--THIS IS FINE FOR FINAL ASSIGNMENT
#With dummy contrasts, REFERENCE CATEGORY is at position 0 and each non-reference category is at position 1 on its own axis
#Thus, every slope (coefficient) of a main effect represents the difference between the reference category mean and the mean of that category, when the other main effects are at their reference levels as well
#
#Intercept: all predictors at their reference level
#genderMale: slope (difference) from female to male WHEN ALCOHOL AT ITS REFERENCE LEVEL
#alcohol4 pints: slope (difference) from none to 4 pints when ALCOHOL AT ITS REFERENCE LEVEL

#Well what about difference from female to male when alcohol = 4 pints?
#Or, what about difference from none to 4 pints when gender = male?

#If there is not a significant interaction, then there is no difference in the slope
#If there is a significant interaction, then the slope must be adjusted by the value of the interaction coefficient


plot(allEffects(goggles_model))

#Note that our x-axis is gender and our grouping variable is alcohol
#In the left panel, the line represents the difference between males and females when alcohol=none (this corresponds to 6.250 in the table)
#In the right panel, the line represents the difference between females and males when alcohol=4 pints. We don't get a coefficient for this in our table. Rather, to get this line's slope, we subtract:
6.250-28.125
#-21.875


#This is a really easy plot to generate and gives you lots of information, such as interactions. 
#However, it arbitrarily chooses an x-axis variable and a grouping variable. 
#But the interaction coefficient is an adjustment on both main effects. 
#Thus, the difference between no alcohol and 4 pints when gender=male would be: 
-3.125-28.125
#-31.25


#It's easier to see this relationship if we put alcohol on the x-axis and use gender as the grouping variable

ggplot(Goggles, aes(alcohol, attractiveness)) + stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y=mean, geom="line", aes(group=1)) + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2, colour="Blue") + facet_wrap(~gender)


#means for each combination of factor levels
by(Goggles$attractiveness, list(Goggles$gender, Goggles$alcohol), stat.desc)
#female none: 60.625
#male none: 66.875
#female 4 pints: 57.5
#male 4 pints: 35.625

#See, the interaction-adjusted slopes are equal to the differences between the appropriate means
35.625-66.875

35.625-57.5



#Problem with dummy contrasts: Significance of coefficients can change depending on what categories you use as your reference categories.


drop1(goggles_model, test="F")

#Can't drop anything, but if we were going to, we would remove the interaction
goggles_model_2 <- lm(attractiveness ~ gender + alcohol, data=Goggles); summary(goggles_model_2)

drop1(goggles_model_2, test="F")


#Note that we don't need to use Anova() here to get p-values for our 2 categorical predictors because they are binary. Thus, the t-test that checks whether the slopes of each are different from 0 in the lm() output will provide the same p-value as the F-test run by Anova() [since t-tests are the same as F-tests when there are only 2 means]. You can see for yourself:
Anova(goggles_model_2, type="III")

#The final model’s formula was attractiveness ~ gender + alcohol + gender:alcohol. The main effects were not sigificant (p>0.5), though the interaction between gender and alcohol was significant (p<0.001). Specifically, compared to females, males showed a significant decrease in the attractiveness of the people they talked to as they moved from no beers to 4 beers. Put another way, compared to no beers, attractiveness of the people talked to when 4 beers were consumed decreased from females to males. The model was highly significant overall (F(2,29)=11.26, p<0.001) with a high degree of explained variance (mult. R2=0.4371, adj. R2=0.3983). All regression coefficients, as well as their standard errors, t scores, and p-values, are provided in the appendix. Checking of model assumptions revealed no problems.






###Random Intercepts

PressureData <- read.table("BloodPressure.csv", sep="\t", header=T, comment.char="", quote="")
head(PressureData)
#Subjects measured their blood pressure during the week leading up to an election. We expect blood pressure to increase throughout week (due to stress).


ggplot(PressureData, aes(Hour, BloodPressure)) + geom_point() + geom_smooth(method="lm")

#What is the slope and intercept of the line of best fit?
summary(lm(BloodPressure~Hour, data=PressureData))


#Problem: violation of independence assumption! (each subject provides multiple data points)
#Therefore, variance is shared across datapoints; some people may just have higher/lower baseline blood pressure.
#To account for this, we can give each subject their own intercept (and thus their own regression line).


#Run a mixed model with random intercepts
library(lme4)
library(lmerTest)

m2 <- lmer(BloodPressure ~ Hour + (1|Subject), data=PressureData)
summary(m2)

#Calculate r2
library(MuMIn)
r.squaredGLMM(m2) #R2m (marginal r squared) is the one we want--it tells us how much variance we would expect to be explained in the population

#you can get the random intercepts for each participant like this
coef(m2)

#We can plot each participant's own regression, as well as the global regression line that is calculated by averaging over the individual participant lines
ggplot(PressureData, aes(Hour, BloodPressure, color=Subject)) + geom_point() + geom_smooth(method="lm") + geom_abline(intercept=65.72, slope=.59, color="red") + geom_abline(intercept=89.86, slope=.59, color="red") + geom_abline(intercept=101.13, slope=.59, color="red") + geom_abline(intercept=156.29, slope=.59, color="red") + geom_abline(intercept=128.95, slope=.59, color="red") + geom_abline(intercept=108.39, slope=.59, color="green")


#Can you only use random intercepts for subjects? NO!
#items are often also treated as a random effect
#e.g., 20 pictures that participants must look at in an MRI experiment
#so, if you had a column with repeated items in it, you could also at (1|Item) to your linear model equation!

