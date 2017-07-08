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

rt_model_planned <- lm(RT ~ FREQUENCY + FAMILIARITY + FREQUENCY:FAMILIARITY, data=RTs)
summary(rt_model_planned)


#Helmert contrasts do the same thing automatically! But, they do it in opposite order: compare group 1 to group 2, then groups 1 & 2 to group 3...

#First, we want to make "lo" the last factor level (group 3) since we want this compared to the other 2 groups
levels(RTs$FAMILIARITY)
RTs$FAMILIARITY <- factor(RTs$FAMILIARITY, levels(RTs$FAMILIARITY)[c(2,3,1)])
levels(RTs$FAMILIARITY)

#Set helmert contrasts
contrasts(RTs$FAMILIARITY) <- contr.helmert(3)
contrasts(RTs$FAMILIARITY)

#The results are the same as the manual planned contrasts!
rt_model_helmert <- lm(RT ~ FREQUENCY + FAMILIARITY + FREQUENCY:FAMILIARITY, data=RTs)
summary(rt_model_helmert)


###What about the post-hoc test option?
pairwise.t.test(RTs$RT, RTs$FAMILIARITY, p.adjust.method="BH")
#This actually tells us more useful information that the planned comparisons

###Model selection: Can we drop any predictors?
drop1(rt_model, test="F")

####What about p-values for whole predictors?
Anova(rt_model, type="III")



###check model assumptions

#no autocorrelation
durbinWatsonTest(rt_model)

#homoscedasticity/homogeneity of variance, normality, and high influence points
par(mfrow=c(2,2))
par(mar=c(4.25,4.25,4.25,4.25))
plot(rt_model)
par(mfrow=c(1,1))

plot(rt_model)


#Could also check homogeneity of variance separately for our nominal predictor
leveneTest(RTs$RT, RTs$FAMILIARITY, center=median)

#Multicollinearity
vif(rt_model)                   
#again, don't worry about interaction. Also, it doesn't matter if vif is high for FAMLIARITY since it is a nominal variable. But FREQUENCY (a numeric variable) is worrying. 

#Perhaps it is collinear with FAMILIARITY. Any idea what we can do to check?


ggplot(RTs, aes(FREQUENCY, FAMILIARITY)) + geom_point()



