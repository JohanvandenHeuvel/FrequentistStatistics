#########################
#########Attributions
#########################

#Datafile: clause_order_data.csv
clause_order <- read.table("clause_order_data.csv", comment.char="", quote="", header=T, sep="\t")

clause_order <- read.table("clause_order_data_CORRECTED.csv", comment.char="", quote="", header=T, sep="\t")

#In this experiment, participants read stories about different people and then indicate how favorable they feel about these people (the normV variable). 
#The last sentence of a story is something like "Jay climbed through a window and stole the jewels, police said." 
#This sentence can either be positive or negative (in the Exp column, "Pos" or "Neg"). 

#Furthermore, the story itself can overall be a mixture of positive and negative actions (storyType=positive) or it can agree with the tone of the final sentence (storyType=accordant); 
#that is, a positive story can go with a positive final sentence and a negative story can go with a negative final sentence. 

#Finally, the "attribution phrase" (i.e., "police said") in the sentence can come either at the beginning of the sentence or the end: 
#"Jay climbed through a window and stole the jewels, police said" versus "Police said that Jay climbed through the window and stole the jewels." 
#Attribution phrases are known to weaken statements when placed first. Therefore, if a sentence is negative, then having "police said" at the beginning will make the statement less severe, 
#and we expect the participants to rate Jay more positively. Similarly, if a sentence is positive, having "police said" at the end will weaken the positive statement less, 
#and we expect the participants to rate Jay more positively. Both of these situations represent the bias=positiveBias condition. 

#On the other hand, placing "police said" at the end of a negative sentence weakens the sentence less, and we expect people to rate Jay more negatively. 
#Similarly, placing "police said" at the beginning of a positive sentence weakens the sentence more, and we expect people to rate Jay more negatively. 
#Both of these situations represent the bias=negativeBias condition.

#Analyze the data to try to determine which of the variables discussed (and their interactions) predict participants' favorability ratings of the people in the stories. 
#In addition, we are also interested in age and gender, although you don't need to include these as interactions with other variables.

#Note: the data in columns "situation" and "person" were used to determine whether the participant was paying attention. 
#If they were, there should be a value of "burglary" or "PSA" in the "situation" column, and "WJ" in the person column. All other data is not valid and should be excluded.

clause_order <- clause_order[clause_order$person == "WJ",]
clause_order <- clause_order[clause_order$situation == "burglary" | clause_order$situation == "PSA",]
clause_order <- data.frame(clause_order[c(1,10,11,12,13,14)])

head(clause_order)

levels(clause_order$bias)
levels(clause_order$Exp)
levels(clause_order$storyType)

clause_order


clause_order_model <- lm(normV ~ Exp * bias * storyType + age + sex, data = clause_order)
drop1(clause_order_model, test = 'F')
"Drop Exp:bias:storyType, AIC decreases from 1833.8 to 1831.9 and the p-value = 0.911998"

clause_order_model <- lm(normV ~ Exp + Exp:bias + Exp:storyType + 
                           bias + bias:storyType +
                           storyType +
                           age + 
                           sex, data = clause_order)
drop1(clause_order_model, test = 'F')
"Drop Exp:bias, AIC decreases from 1831.9 to 1829.9 and the p-value = 0.905586"

clause_order_model <- lm(normV ~ Exp + Exp:storyType + 
                           bias + bias:storyType +
                           storyType +
                           age + 
                           sex, data = clause_order)
drop1(clause_order_model, test = 'F')
"Drop storyType:bias, AIC decreases from 1829.9 to 1828.4 and the p-value = 0.463487"

clause_order_model <- lm(normV ~ Exp + Exp:storyType + 
                           bias +
                           storyType +
                           age + 
                           sex, data = clause_order)
drop1(clause_order_model, test = 'F')

###Assumptions###
#Autocorrelation#
durbinWatsonTest(clause_order_model)
"D-W Statistic = 2.217541, p-value = 0.056"

par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
plot(clause_order_model)

#Normality of residuals#
"The residuals seem normal distributed"

#Homoscedasticity#
"The assumption of homscedasticty does not seem to be violated"

#Outliers#
sum(rstandard(clause_order_model) > 3.29) #1
sum(rstandard(clause_order_model) < -3.29) #2


sum(cooks.distance(clause_order_model) > 1) #0


mean(hatvalues(clause_order_model)) #0.02020202
sum(hatvalues(clause_order_model) > 2*0.02020202) #0
sum(hatvalues(clause_order_model) > 3*0.02020202) #0


#Multicollinearity#
vif(clause_order_model)


#Linearity"
"????????"


plot(allEffects(clause_order_model))
plot(Effect(c("Goggles", "Flippers"), clause_order_model))

