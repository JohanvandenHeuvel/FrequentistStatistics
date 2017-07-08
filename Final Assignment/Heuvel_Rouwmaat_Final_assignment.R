

###FINAL ASSIGNMENT

setwd("~/Documents/RU/Sem2/Statistics/Final Assignment")

library(ggplot2)
library(effects)
library(car)

#random intercepts
library(lme4)
library(lmerTest)

#squaredGLMM
library(MuMIn)

#Some general tips:

#remember you need plots of your effects. If you have many predictors, there can be too many graphs for use of the allEffects() function. 
#In this case, it is better to plot the effects individually. 
#For main effects, you can do this by using plot(Effect("name of effect", name_of_model_object))
#For interactions, you can use plot(Effect(c("name of effect 1","name of effect 2"), name_of_model_object))
#If your interaction is between 2 numerical predictors, it's better to use the contour/elevation plots we went over a couple of times.

#When checking for multicollinearity, note that vif values for interactions and single predictors that participate in interactions can be quite high. 
#This is nothing to worry about. Single predictors that do not participate in interactions, however, should have vif values below 10.

#If you violate an assumption, use a different method if we've learned about one. 
#However, if not, just note the assumption violation when you do the reporting and what this means for the interpretation of the results.

#In addition to the standard reporting of the results, please give a brief discussion of the findings. 
#For example, if there is a significant interaction, talk about why this might be. What does the interaction actually mean logically? 
#It's ok to speculate a bit here, since this is a statistics course and not a course about any one particular field.

#You'll have to load the necessary libraries! I do not provide those lines of code here!

#Also, when using drop1(), you must use the Chisq test instead of the F test when you have created a model using random intercepts.

#It may be necessary to remove rows that contain missing data!


###################
#####Swimming Data
###################

#datafile: swimming.csv
swimming <- read.table("swimming.csv", comment.char="", quote="", header=T, sep="\t")
head(swimming)

#In this study, the times it took for different participants to swim a lap in a swimming pool were measured. 
#Swimmers varied in terms of which end of the pool they started from (End variable), whether they wore goggles, whether they wore a shirt, 
#and whether they used flippers. 
#Perform an analysis and consider up to 2-way interactions between predictors. 

###Model Selection###
model_swimming <-lm(Time ~ 
                      Shirt + Shirt:Goggles + Shirt:Flippers + Shirt:End + 
                      Goggles + Goggles:Flippers + Goggles:End + 
                      Flippers +  Flippers:End +
                      End, data=swimming)
drop1(model_swimming, test = 'F')
"Drop Shirt:End, AIC decreases from 129.95 to 128.39 and the p-value = 0.54"

model_swimming <-lm(Time ~ 
                      Shirt + Shirt:Goggles + Shirt:Flippers+ 
                      Goggles + Goggles:Flippers + Goggles:End + 
                      Flippers +  Flippers:End +
                      End, data=swimming)
drop1(model_swimming, test = 'F')
"Drop Shirt:Goggles, AIC decreases from 128.39 to 126.67 and the p-value = 0.63"

model_swimming <-lm(Time ~ 
                      Shirt + Shirt:Flippers+ 
                      Goggles + Goggles:Flippers + Goggles:End + 
                      Flippers +  Flippers:End +
                      End, data=swimming)
drop1(model_swimming, test = 'F')
"Drop Goggles:End, AIC decreases from 126.67 to 125.21 and the p-value = 0.5"

model_swimming <-lm(Time ~ 
                      Shirt + Shirt:Flippers+ 
                      Goggles + Goggles:Flippers + 
                      Flippers +  Flippers:End +
                      End, data=swimming)
drop1(model_swimming, test = 'F')
"Drop Flippers:End, AIC decreases from 125.21 to 123.80 and the p-value = 0.47"

model_swimming <-lm(Time ~ 
                      Shirt + Shirt:Flippers+ 
                      Goggles + Goggles:Flippers + 
                      Flippers +
                      End, data=swimming)
drop1(model_swimming, test = 'F')
"Drop End, AIC decreases from 123.80 to 122.10 and the p-value = 0.6"

model_swimming <-lm(Time ~ 
                      Shirt + Shirt:Flippers+ 
                      Goggles + Goggles:Flippers + 
                      Flippers, data=swimming)
drop1(model_swimming, test = 'F')
"Model:
Time ~ Shirt + Shirt:Flippers + Goggles + Goggles:Flippers + 
Flippers
                  Df Sum of Sq    RSS    AIC F value    Pr(>F)    
<none>                        337.41 122.10                      
Shirt:Flippers    1    16.022 353.43 123.34  3.0391 0.0860857 .  
Flippers:Goggles  1    84.757 422.16 135.78 16.0769 0.0001615 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Shirt almost significant and dropping it raises the AIC value"

###Assumptions###
#Autocorrelation#
durbinWatsonTest(model_swimming)
"D-W Statistic = 1.92, p-value = 0.814
The D-w Statistic does return a value almost >2, however the p-value is not significant.
As the p-value is not near the 0.05 we cannot reject the null-hypothesis, which states that there is no autocorrelation.
The autocorrelation assumption is thus not violated"

par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
plot(model_swimming)

#Normality of residuals#
"The residuals seem normal distributed"

#Homoscedasticity#
"The assumption of homscedasticty does not seem to be violated"

#Outliers#
sum(rstandard(model_swimming) > 3.29) #0
sum(rstandard(model_swimming) < -3.29) #0

sum(cooks.distance(model_swimming) > 1) #0

mean(hatvalues(model_swimming)) #0.08571429
sum(hatvalues(model_swimming) > 2*0.08571429) #0
sum(hatvalues(model_swimming) > 3*0.08571429) #0
"There do not seem to be any outliers, influential points, or high-leverage points"

#Multicollinearity#
vif(model_swimming)
"There does not seem to be a problem with multicollinearity"


###Discussion###

model_swimming <-lm(Time ~ 
                      Shirt + Shirt:Flippers+ 
                      Goggles + Goggles:Flippers + 
                      Flippers, data=swimming)
summary(model_swimming)
"Call:
lm(formula = Time ~ Shirt + Shirt:Flippers + Goggles + Goggles:Flippers + 
Flippers, data = swimming)

Residuals:
Min      1Q  Median      3Q     Max 
-4.4159 -1.4009 -0.1529  1.2655  4.9850 

Coefficients:
                        Estimate Std. Error t value Pr(>|t|)    
(Intercept)             21.7892     0.6288  34.652  < 2e-16 ***
Shirtyes                 2.4641     0.7261   3.394 0.001188 ** 
Gogglesyes              -2.8177     0.7261  -3.881 0.000249 ***
Flippersyes             -4.2903     0.9136  -4.696 1.45e-05 ***
Shirtyes:Flippersyes    -2.0014     1.1480  -1.743 0.086086 .  
Flippersyes:Gogglesyes   4.6032     1.1480   4.010 0.000162 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.296 on 64 degrees of freedom
Multiple R-squared:  0.4891,	Adjusted R-squared:  0.4491 
F-statistic: 12.25 on 5 and 64 DF,  p-value: 2.407e-08"

"The models formula is Time ~ Shirt + Shirt:Flippers + Goggles + Goggles:Flippers + Flippers 
All the main effects were significant (p < 0.005), except Shirt:Flippers (p-value = 0.086).
The model is significant overall (F 5,64 = 12.25, p=2.407e-08) and has a high variance explanation (mult.R-squared= 0.4891,adj.R-squared= 0.4491)
All regression coefficients, as well as their SE, t-scores and p-values are provided in the appendix."

"Three of the four orginal predictors are still in the model. The one that is not significant is the 'End' predictor. This makes sense as the end side of
a swimming pool does not make sense as having an effect on swimming time. 
For the other predictors: swimming with a Shirt on makes you slower, swimming with Goggles or Flippers makes you faster. These relations sound very obvious.
There are two interactions: Shirt:Flippers and Flippers:Goggles. If we plot these we can see the relation very clear. As one might expect: swimming with a flippers makes
swimming with a shirt faster. Swimming with flippers abd goggles makes you actually slower then swimming with flippers and without goggles. 
This is a counter-intuitive, both goggles and flippers makes you faster, and combining then makes you slower. 
Something we need to consider is that there is a relation we did not measure. For example physical fitness or swimming skill. Maybe all the swimming amateurs swim in shirts 
and the pros without, then the shirt is not the actual predictor. 

Of all of the predictors is Flippers the strongest, then Goggles and then Shirt (which is positive instead of negative, like the other two). The interaction between 
Shirt:Flippers is almost of the same strength as Shirt, only this effect is negative. Flippers:Goggles is a little bit stronger then Flippers, and here the interaction is
positive where Flippers is a negative relation."

plot(allEffects(model_swimming))
plot(Effect('Shirt',model_swimming))
plot(Effect('Goggles',model_swimming))
plot(Effect('Flippers',model_swimming))

##########################
##########Reaction Times
##########################

#datafile: reaction_times.csv
reaction_times <- read.table("reaction_times.csv", comment.char="", quote="", header=T, sep="\t")
head(reaction_times)

#In this study, participants had to make a decision on whether a sequence of letters presented to them on a computer screen was a word or not. 
#How long it took for them to make this decision was recorded (their reaction time). This is called a lexical decision task. 

#In this kind of task, many different factors can influence reaction times, including how frequent the word is, 
#how familiar the word is, and how "imageable" it is--that is, how easy it is to imagine a picture of the word's meaning. 
#Investigate whether, for this data set, this is true. Consider up to the 3-way interaction. 

#Note that reaction times are averages from across participants, so although this was originally a within-subject experiment, 
#participant-level variance is not shared across data points.

###Model Selection###

"Cases Whale, Strawberry, Shark, Moose, Lettuce and Cucumber do not have an entry for FREQUENCY. We remove these rows"
reaction_times <- data.frame(reaction_times[-c(16,26,30,39,44,48), ])

levels(reaction_times$FAMILIARITY)
reaction_times$FAMILIARITY <- relevel(reaction_times$FAMILIARITY, "lo"); levels(reaction_times$FAMILIARITY)
levels(reaction_times$IMAGEABILITY)
reaction_times$IMAGEABILITY <- relevel(reaction_times$IMAGEABILITY, "lo"); levels(reaction_times$IMAGEABILITY)

model_reaction_times <- lm(RT ~ FREQUENCY:FAMILIARITY:IMAGEABILITY 
                           , data=reaction_times)

model_reaction_times <- lm(RT ~ FREQUENCY + FAMILIARITY + IMAGEABILITY 
                           + FREQUENCY:FAMILIARITY 
                           + FREQUENCY:IMAGEABILITY
                           + FAMILIARITY:IMAGEABILITY
                           + FREQUENCY:FAMILIARITY:IMAGEABILITY, data=reaction_times)

summary(model_reaction_times)
"Call:
lm(formula = RT ~ FREQUENCY + FAMILIARITY + IMAGEABILITY + FREQUENCY:FAMILIARITY + 
FREQUENCY:IMAGEABILITY + FAMILIARITY:IMAGEABILITY + FREQUENCY:FAMILIARITY:IMAGEABILITY, 
data = reaction_times)

Residuals:
Min      1Q  Median      3Q     Max 
-81.949 -33.601  -1.978  29.513  86.814 

Coefficients:
                                        Estimate Std. Error t value Pr(>|t|)    
(Intercept)                              689.05      60.14  11.458  3.2e-13 ***
FREQUENCY                                -30.89      48.72  -0.634    0.530    
FAMILIARITYhi                            -52.33      67.72  -0.773    0.445    
IMAGEABILITYhi                            35.32      94.12   0.375    0.710    
FREQUENCY:FAMILIARITYhi                   17.53      51.00   0.344    0.733    
FREQUENCY:IMAGEABILITYhi                 -17.86      65.85  -0.271    0.788    
FAMILIARITYhi:IMAGEABILITYhi             -35.27     102.35  -0.345    0.733    
FREQUENCY:FAMILIARITYhi:IMAGEABILITYhi    17.42      68.18   0.256    0.800    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 45.83 on 34 degrees of freedom
Multiple R-squared:  0.2433,	Adjusted R-squared:  0.0875 
F-statistic: 1.562 on 7 and 34 DF,  p-value: 0.1805"

drop1(model_reaction_times, test = 'F')
"Drop FREQUENCY:FAMILIARITY:IMAGEABILITY, AIC decreases from 328.42 to 326.50 and the p-value = 0.80"

model_reaction_times <- lm(RT ~ FREQUENCY + FAMILIARITY + IMAGEABILITY 
                           + FREQUENCY:FAMILIARITY 
                           + FREQUENCY:IMAGEABILITY
                           + FAMILIARITY:IMAGEABILITY, data=reaction_times)
drop1(model_reaction_times, test = 'F')
"Drop FREQUENCY:IMAGEABILITY, AIC decreases from 326.50 to 324.51 and the p-value = 0.92"

model_reaction_times <- lm(RT ~ FREQUENCY + FAMILIARITY + IMAGEABILITY 
                           + FREQUENCY:FAMILIARITY 
                           + FAMILIARITY:IMAGEABILITY, data=reaction_times)
drop1(model_reaction_times, test = 'F')
"Drop FAMILIARITY:IMAGEABILITY, AIC decreases from 324.51 to 322.63 and the p-value = 0.75"

model_reaction_times <- lm(RT ~ FREQUENCY + FAMILIARITY + IMAGEABILITY 
                           + FREQUENCY:FAMILIARITY , data=reaction_times)
drop1(model_reaction_times, test = 'F')
"Drop IMAGEABLITY, AIC decreases from 322.63 to 320.64 and the p-value = 0.93"

model_reaction_times <- lm(RT ~ FREQUENCY + FAMILIARITY 
                           + FREQUENCY:FAMILIARITY , data=reaction_times)
drop1(model_reaction_times, test = 'F')
"Drop FREQUENCY:FAMILIARITY, AIC decreases from 320.64 to 319.30 and the p-value = 0.44"

model_reaction_times <- lm(RT ~ FREQUENCY + FAMILIARITY, data=reaction_times)
drop1(model_reaction_times, test = 'F')
"Model:
RT ~ FREQUENCY + FAMILIARITY
            Df Sum of Sq   RSS    AIC F value  Pr(>F)  
<none>                   72928 319.30                  
FREQUENCY    1    8942.8 81871 322.16  4.7824 0.03482 *
FAMILIARITY  1    5221.8 78150 320.21  2.7925 0.10272  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

FAMILIARITY almost significant and dropping it raised the AIC value"



###Assumptions###
#Autocorrelation#
durbinWatsonTest(model_reaction_times)
"D-W Statistic = 2.42, p-value = 0.124
The p-value is close to 0.05 and the D-W Statistic >2, so it is very close to violating the assumption of autocorrelation.
In this case we can still use the model, as the values are just above the boundary. There might be a small violation."

par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
plot(model_reaction_times)

#Normality of residuals#
"The residuals seem normal distributed"

#Homoscedasticity#
"The assumption of homscedasticty does seem to be violated a little bit"

#Outliers#
sum(rstandard(model_reaction_times) > 3.29) #0
sum(rstandard(model_reaction_times) < -3.29) #0

sum(cooks.distance(model_reaction_times) > 1) #0

mean(hatvalues(model_reaction_times)) #0.07142857
sum(hatvalues(model_reaction_times) > 2*0.07142857) #8
sum(hatvalues(model_reaction_times) > 3*0.07142857) #0
"There do not seem to be any outliers, or influential points. There are 8 high-leverage points."

#Multicollinearity#
vif(model_reaction_times)
"There does not seem to be a problem with multicollinearity"

###Discussion###

model_reaction_times <- lm(RT ~ FREQUENCY + FAMILIARITY, data=reaction_times)
summary(model_reaction_times)
"Call:
lm(formula = RT ~ FREQUENCY + FAMILIARITY, data = reaction_times)

Residuals:
    Min      1Q  Median      3Q     Max 
-82.226 -34.370  -2.089  30.445  82.133 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)    671.959     18.664  36.002   <2e-16 ***
FREQUENCY      -15.113      6.911  -2.187   0.0348 *  
FAMILIARITYhi  -31.884     19.080  -1.671   0.1027    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 43.24 on 39 degrees of freedom
Multiple R-squared:  0.2273,	Adjusted R-squared:  0.1877 
F-statistic: 5.736 on 2 and 39 DF,  p-value: 0.006552
"

"The models formula is RT ~ FREQUENCY + FAMILIARITY
FREQUENCY is significant (p-value = 0.035), FAMILIARITY is almost significant (p-value = 0.1).
The model is significant overall (F 2,39 = 5.736, p=2.407e-08) and has a medium variance explanation (mult.R-squared= 0.2273 ,adj.R-squared= 0.1877)
All regression coefficients, as well as their SE, t-scores and p-values are provided in the appendix."

"Both of the predictors make sense: The more frequent the word is the faster you can recognize it. Also for familiarity we have the same kind of relation. 
A low familiarity results in a slower reaction time then a high familiarity. It makes sense that the more you see a word the faster you can recognize it.

Both FREQUENCY and FAMILIARITY have a negative relation to the RT, so the higher these values the lower the reaction time. In the plot we can see that the strength
of the relation in roughly in the same order."

plot(allEffects(model_reaction_times))

#########################
#########Housing Data
#########################

#Datafile: housing_data.csv

#In this study, researchers collected data on crime rates for different city blocks in and around Boston in the U.S. 

#You are interested in determining which variables predict crime rate. 

#You are particularly interested in how economic status and ethnic diversity interact with each other as well as how each one interacts with the other predictors. 
#Perform a proper analysis. The column information is below. (Note: because towns are repeated, we are actually violating the independence assumption. 
#Normally, we would want to assign random intercepts to each town, but because we do not have a "town" variable, this is difficult...so we'll just ignore this here :-)


#1. CRIM      per capita crime rate by block
#2. INDUS     proportion of non-retail business acres per town
#3. CHAS      whether property is next to the Charles River or not
#4. NOX       nitric oxides concentration (parts per 10 million)
#5. RM        average number of rooms per dwelling
#6. AGE       proportion of owner-occupied units built prior to 1940
#7. DIS       weighted distances to five Boston employment centres
#8. RAD       index of accessibility to radial highways
#9. PTRATIO   pupil-teacher ratio by town
#10. B        measure of ethnic diversity
#11. LSTAT    % lower economic status of the population
#12. MEDV     Median value of owner-occupied homes in $1000's

housing_data <- read.table("housing_data.csv", comment.char="", quote="", header=T, sep="\t")
head(housing_data)

model_housing_data <- lm(CRIM ~ INDUS + CHAS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:INDUS + B:CHAS + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + B:PTRATIO + B:MEDV + 
                           LSTAT:INDUS + LSTAT:CHAS + LSTAT:NOX + LSTAT:RM + LSTAT:AGE + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Drop LSTAT:CHAS, AIC decreases from 1766.9 to 1764.9 and the p-value = 0.89"

model_housing_data <- lm(CRIM ~ INDUS + CHAS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:INDUS + B:CHAS + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + B:PTRATIO + B:MEDV + 
                           LSTAT:INDUS + LSTAT:NOX + LSTAT:RM + LSTAT:AGE + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Drop LSTAT:RM, AIC decreases from 1764.9 to 1764.7 and the p-value = 0.2"

model_housing_data <- lm(CRIM ~ INDUS + CHAS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:INDUS + B:CHAS + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + B:PTRATIO + B:MEDV + 
                           LSTAT:INDUS + LSTAT:NOX + LSTAT:AGE + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Drop CHAS:B, AIC decreases from 1764.7 to 1764.6 and the p-value = 0.18"

model_housing_data <- lm(CRIM ~ INDUS + CHAS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:INDUS + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + B:PTRATIO + B:MEDV + 
                           LSTAT:INDUS + LSTAT:NOX + LSTAT:AGE + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Drop CHAS, AIC decreases from 1764.6 to 1762.7 and the p-value = 0.82"

model_housing_data <- lm(CRIM ~ INDUS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:INDUS + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + B:PTRATIO + B:MEDV + 
                           LSTAT:INDUS + LSTAT:NOX + LSTAT:AGE + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Drop NOX:B, AIC decreases from 1762.7 to 1762.2 and the p-value = 0.22"

model_housing_data <- lm(CRIM ~ INDUS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:INDUS + B:RM + B:AGE + B:DIS + B:RAD + B:PTRATIO + B:MEDV + 
                           LSTAT:INDUS + LSTAT:NOX + LSTAT:AGE + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Drop PTRATIO:B, AIC decreases from 1762.2 to 1762.0 and the p-value = 0.2"

model_housing_data <- lm(CRIM ~ INDUS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:INDUS + B:RM + B:AGE + B:DIS + B:RAD + B:MEDV + 
                           LSTAT:INDUS + LSTAT:NOX + LSTAT:AGE + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Drop MEDV:B, AIC increases from 1762.0 to 1762.2 and the p-value = 0.15"

model_housing_data <- lm(CRIM ~ INDUS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:INDUS + B:RM + B:AGE + B:DIS + B:RAD + 
                           LSTAT:INDUS + LSTAT:NOX + LSTAT:AGE + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Model:
CRIM ~ INDUS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + 
    MEDV + B:LSTAT + B:INDUS + B:RM + B:AGE + B:DIS + B:RAD + 
    LSTAT:INDUS + LSTAT:NOX + LSTAT:AGE + LSTAT:DIS + LSTAT:RAD + 
    LSTAT:PTRATIO + LSTAT:MEDV
              Df Sum of Sq   RSS    AIC F value    Pr(>F)    
<none>                     14977 1762.2                      
B:LSTAT        1   1089.05 16066 1795.7 35.0484 6.115e-09 ***
INDUS:B        1    276.15 15253 1769.4  8.8871  0.003017 ** 
RM:B           1    985.54 15963 1792.5 31.7169 3.034e-08 ***
AGE:B          1    674.16 15651 1782.5 21.6961 4.138e-06 ***
DIS:B          1    299.16 15276 1770.2  9.6278  0.002029 ** 
RAD:B          1    266.80 15244 1769.1  8.5862  0.003548 ** 
INDUS:LSTAT    1     88.22 15065 1763.2  2.8393  0.092633 .  
NOX:LSTAT      1    116.17 15093 1764.1  3.7386  0.053754 .  
AGE:LSTAT      1    141.37 15118 1765.0  4.5496  0.033431 *  
DIS:LSTAT      1    320.72 15298 1770.9 10.3216  0.001403 ** 
RAD:LSTAT      1    122.66 15100 1764.3  3.9475  0.047508 *  
PTRATIO:LSTAT  1    134.54 15112 1764.7  4.3298  0.037978 *  
LSTAT:MEDV     1   1694.18 16671 1814.4 54.5226 6.803e-13 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Dropping INDUS:LSTAT would increase the AIC value and this interaction is almost significant (p-value = 0.092)"


###Assumptions###
#Autocorrelation#
durbinWatsonTest(model_housing_data)
"D-W Statistic = 1.716264, p-value = 0.012
The D-W Statistic does return a value close >2, and the p-value is significant.
As the p-value is significant we reject the null-hypothesis, which states that there is no autocorrelation."

par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
plot(model_housing_data)

#Normality of residuals#
"The residuals seem normal distributed."

#Homoscedasticity#
"The assumption of homscedasticty does seem to be violated a bit, most of the points are centered around the same place. And there is a bit of a fan shape"

#Outliers#
sum(rstandard(model_housing_data) > 3.29) #5
sum(rstandard(model_housing_data) < -3.29) #2
"There seem to be 7 outliers"

sum(cooks.distance(model_housing_data) > 1) #0

mean(hatvalues(model_housing_data)) #0.04743083
sum(hatvalues(model_housing_data) > 2*0.04743083) #56
sum(hatvalues(model_housing_data) > 3*0.04743083) #25
"There seem to be quite alot of high-leverage points"

"We do not have a good reason to remove the outliers, and there are no influential points so the outliers do not have a huge effect on the model"

#Multicollinearity#
vif(model_housing_data)
"There does not seem to be a problem with multicollinearity"

###Discussion###
model_housing_data <- lm(CRIM ~ INDUS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:INDUS + B:RM + B:AGE + B:DIS + B:RAD + 
                           LSTAT:INDUS + LSTAT:NOX + LSTAT:AGE + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
summary(model_housing_data)
"Call:
lm(formula = CRIM ~ INDUS + NOX + RM + AGE + DIS + RAD + PTRATIO + 
    B + LSTAT + MEDV + B:LSTAT + B:INDUS + B:RM + B:AGE + B:DIS + 
    B:RAD + LSTAT:INDUS + LSTAT:NOX + LSTAT:AGE + LSTAT:DIS + 
    LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV, data = housing_data)

Residuals:
    Min      1Q  Median      3Q     Max 
-18.972  -1.423  -0.304   0.837  71.719 

Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
(Intercept)   54.4364532 25.2132620   2.159 0.031340 *  
INDUS         -1.8061361  0.7007466  -2.577 0.010249 *  
NOX            9.4666935 11.5315899   0.821 0.412089    
RM            -9.4933631  1.8255728  -5.200 2.95e-07 ***
AGE            0.5893792  0.1259192   4.681 3.72e-06 ***
DIS           -7.5872026  2.8302323  -2.681 0.007597 ** 
RAD            0.8111988  0.2255723   3.596 0.000356 ***
PTRATIO        0.3185023  0.3461060   0.920 0.357905    
B             -0.2261893  0.0561948  -4.025 6.61e-05 ***
LSTAT          2.7828455  1.0601252   2.625 0.008940 ** 
MEDV           0.1046929  0.0706667   1.482 0.139126    
B:LSTAT        0.0033849  0.0005718   5.920 6.11e-09 ***
INDUS:B        0.0050938  0.0017087   2.981 0.003017 ** 
RM:B           0.0296672  0.0052678   5.632 3.03e-08 ***
AGE:B         -0.0014332  0.0003077  -4.658 4.14e-06 ***
DIS:B          0.0218638  0.0070463   3.103 0.002029 ** 
RAD:B         -0.0015527  0.0005299  -2.930 0.003548 ** 
INDUS:LSTAT   -0.0197383  0.0117140  -1.685 0.092633 .  
NOX:LSTAT     -1.4748708  0.7627807  -1.934 0.053754 .  
AGE:LSTAT     -0.0051375  0.0024086  -2.133 0.033431 *  
DIS:LSTAT     -0.1521068  0.0473452  -3.213 0.001403 ** 
RAD:LSTAT      0.0136886  0.0068897   1.987 0.047508 *  
PTRATIO:LSTAT -0.0618258  0.0297123  -2.081 0.037978 *  
LSTAT:MEDV    -0.0522432  0.0070752  -7.384 6.80e-13 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.574 on 482 degrees of freedom
Multiple R-squared:  0.5991,	Adjusted R-squared:   0.58 
F-statistic: 31.32 on 23 and 482 DF,  p-value: < 2.2e-16"

"The models formula is CRIM ~ INDUS + NOX + RM + AGE + DIS + RAD + PTRATIO + 
    B + LSTAT + MEDV + B:LSTAT + B:INDUS + B:RM + B:AGE + B:DIS + 
    B:RAD + LSTAT:INDUS + LSTAT:NOX + LSTAT:AGE + LSTAT:DIS + 
    LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV

All predictors and interactions are significant (p-value < 0.05), except NOX, PTRATIO, and MEDV these are not sigificant. INDUS:LSTAT and NOX:LSTAT are almost significant.
The model is significant overall (F 23,482 = 31.32, p-value: < 2.2e-16) and has a high variance explanation (mult.R-squared= 0.5991 ,adj.R-squared= 0.58)
All regression coefficients, as well as their SE, t-scores and p-values are provided in the appendix."

"Predictor meanings:
CRIM      per capita crime rate by block
INDUS     proportion of non-retail business acres per town
CHAS      whether property is next to the Charles River or not
NOX       nitric oxides concentration (parts per 10 million)
RM        average number of rooms per dwelling
AGE       proportion of owner-occupied units built prior to 1940
DIS       weighted distances to five Boston employment centres
RAD       index of accessibility to radial highways
PTRATIO   pupil-teacher ratio by town
B        measure of ethnic diversity
LSTAT    % lower economic status of the population
MEDV     Median value of owner-occupied homes in $1000's

Possible explanations for predictors and interactions:"

plot(allEffects(model_housing_data))

plot(Effect('INDUS', model_housing_data))
"Indus has a negative effect on crime. Non-retail business acres means jobs for people, its is plausible
that this reduces crime."
plot(Effect('NOX', model_housing_data))
"Nox is not significant"
plot(Effect('RM', model_housing_data))
"RM has a positive relation with crime. So more rooms in a dwelling means more crime. This can caused by the fact
that dwellings with a lot of rooms are usually costly. So for criminals these dwellings are a good oppeturnity to steal from."
plot(Effect('AGE', model_housing_data))
"Age has a positive relation with crime. This means that places with older buildings have more crime. Older buildings can be an indication of 
areas that do not have a lot of development, an indication of being poor. Poor can mean more crime."
plot(Effect('DIS', model_housing_data))
"Higher distance to employment center means more crime. Unenmployment causes crime"
plot(Effect('RAD', model_housing_data))
"RAD has a positive relation with crime. The less developed areas are usually further away from highways, or have a bad infrastructire."
plot(Effect('PTRATIO', model_housing_data))
"PTRATIO is not significant"
plot(Effect('B', model_housing_data))
"B has a positive relation with crime. So a higher ethnic diversity means more crime. Usually are white people more priviliged then other ethnic groups.
This can mean that those other groups do not have the education, money, or jobs that white people have. This can result in more crime."
plot(Effect('LSTAT', model_housing_data))
"LSTAT has a negative relation with crime. A lower economic static means less wealthy. Being poor results sooner in crime"
plot(Effect('MEDV', model_housing_data))
"MEDV is not significant"

library(scatterplot3d)
scatterplot3d(housing_data$CRIM, housing_data$LSTAT, housing_data$INDUS)
scatterplot3d(housing_data$CRIM, housing_data$LSTAT, housing_data$B)
scatterplot3d(housing_data$CRIM, housing_data$LSTAT, housing_data$NOX)
scatterplot3d(housing_data$CRIM, housing_data$LSTAT, housing_data$AGE)
scatterplot3d(housing_data$CRIM, housing_data$LSTAT, housing_data$DIS)
scatterplot3d(housing_data$CRIM, housing_data$LSTAT, housing_data$RAD)
scatterplot3d(housing_data$CRIM, housing_data$LSTAT, housing_data$PTRATIO)
scatterplot3d(housing_data$CRIM, housing_data$LSTAT, housing_data$MEDV)

scatterplot3d(housing_data$CRIM, housing_data$B, housing_data$INDUS)
scatterplot3d(housing_data$CRIM, housing_data$B, housing_data$RM)
scatterplot3d(housing_data$CRIM, housing_data$B, housing_data$AGE)
scatterplot3d(housing_data$CRIM, housing_data$B, housing_data$DIS)
scatterplot3d(housing_data$CRIM, housing_data$B, housing_data$RAD)

"More information can be seen in the plots, and the strengts of the relations can be seen in the appendix"

###############################
#########Idiom Experiment
###############################

#Datafile: idiom_lexical_decision.csv
idiom <- read.table("idiom_lexical_decision.csv", comment.char="", quote="", header=T, sep="\t")
head(idiom)
summary(idiom)

#In this experiment, participants were shown Dutch idioms such as "de koe bij de hoorns vatten" (grab the cow by the horns), 
#which hase a figurative meaning of "to take control of a situation." After each idiom, they were shown either a word or a nonword and had to decide whether 
#it was a word or not. The words were related either to the figurative meaning ("Fig" Condition) or to the literal meaning of the last word ("hoorns", i.e. the "Lit" Condition). 
#The experimenter measured how much time, in milliseconds, it took for the participants to make the decision (this is called a lexical decision task), 
#and then she took the log of these reaction times. 

#If reading the idiom activated its figurative meaning in the mind of the subject, 
#then they will be quicker to decide that a figuratively related word ("Fig") is indeed a word. If reading the idioms activated its literal meaning, 
#then the subject will be quicker to make a decision on a literally related words. 

#Other factors might affect reaction times as well. These include: 
#the frequency of the word, 
#the literal plausibility (LP) of the idiom (how easy is it to interpret literally, e.g., "staan voor aap" = "stand for ape" is not literally plausible!)
#the transparency of the idiom (how related are the figurative and literal meanings, e.g., "tegen de lamp lopen" = "walk into the lamp" is not very related to figurative meaning "get caught")
#How well the subject knows the idiom (Knowledge)

#Your job is to conduct an analysis of this data and report your results. Consider all 2-way interactions. 

#Note: there are multiple data points per subject and per idiom, so both of the variables cause a violation of the independence assumption of regression. 
#You will thus need to give both of these variables random intercepts. (you can just add a second random intercept term to your model equation like you add a first one)

#Note: You only need to check the assumptions of homoscedasticity and normally distributed residuals. 
#Plotting the model will only give you the first diagnostic plot, which you can use to check homoscedasticity. 
#To check for normal residuals, you can first get the residuals by passing the model object to the residuals() function, and then plotting the output.

"random intercepts for subject and idiom"
levels(idiom$Condition) #"fig" is base

idiom_model <- lmer(log_RTs ~ Condition + Condition:Freqs + Condition:LP + Condition:Transparency + Condition:Knowledge + 
                      Freqs + Freqs:LP + Freqs:Transparency + Freqs:Knowledge + 
                      LP + LP:Transparency + LP:Knowledge + 
                      Transparency + Transparency:Knowledge + 
                      Knowledge +
                      (1|Subject)+
                      (1|Idiom), data = idiom)
summary(idiom_model)


drop1(idiom_model, test = "Chisq")
"Drop Freqs:LP, AIC decreases from -947.32 to -949.13 and the p-value = 0.66"

idiom_model <- lmer(log_RTs ~ Condition + Condition:Freqs + Condition:LP + Condition:Transparency + Condition:Knowledge + 
                      Freqs + Freqs:Transparency + Freqs:Knowledge + 
                      LP + LP:Transparency + LP:Knowledge + 
                      Transparency + Transparency:Knowledge + 
                      Knowledge +
                      (1|Subject)+
                      (1|Idiom), data = idiom)
drop1(idiom_model, test = "Chisq")
"Drop Freqs:Knowledge, AIC decreases from -949.13 to -950.92 and the p-value = 0.65"

idiom_model <- lmer(log_RTs ~ Condition + Condition:Freqs + Condition:LP + Condition:Transparency + Condition:Knowledge + 
                      Freqs + Freqs:Transparency + 
                      LP + LP:Transparency + LP:Knowledge + 
                      Transparency + Transparency:Knowledge + 
                      Knowledge +
                      (1|Subject)+
                      (1|Idiom), data = idiom)
drop1(idiom_model, test = "Chisq")
"Drop Transparenct:Knowledge, AIC decreases from -950.92 to -952.62 and the p-value = 0.59"

idiom_model <- lmer(log_RTs ~ Condition + Condition:Freqs + Condition:LP + Condition:Transparency + Condition:Knowledge + 
                      Freqs + Freqs:Transparency + 
                      LP + LP:Transparency + LP:Knowledge + 
                      Transparency + 
                      Knowledge +
                      (1|Subject)+
                      (1|Idiom), data = idiom)
drop1(idiom_model, test = "Chisq")
"Drop Condition:Freqs, AIC decreases from -952.62 to -954.28 and the p-value = 0.56"

idiom_model <- lmer(log_RTs ~ Condition + Condition:LP + Condition:Transparency + Condition:Knowledge + 
                      Freqs + Freqs:Transparency + 
                      LP + LP:Transparency + LP:Knowledge + 
                      Transparency + 
                      Knowledge +
                      (1|Subject)+
                      (1|Idiom), data = idiom)
drop1(idiom_model, test = "Chisq")
"Drop Condition:Transparency, AIC decreases from -954.28 to -955.85 and the p-value = 0.52"

idiom_model <- lmer(log_RTs ~ Condition + Condition:LP + Condition:Knowledge + 
                      Freqs + Freqs:Transparency + 
                      LP + LP:Transparency + LP:Knowledge + 
                      Transparency + 
                      Knowledge +
                      (1|Subject)+
                      (1|Idiom), data = idiom)
drop1(idiom_model, test = "Chisq")
"Drop Freqs:Transparency, AIC decreases from -955.85 to -957.60 and the p-value = 0.62"

idiom_model <- lmer(log_RTs ~ Condition + Condition:LP + Condition:Knowledge + 
                      Freqs + 
                      LP + LP:Transparency + LP:Knowledge + 
                      Transparency + 
                      Knowledge +
                      (1|Subject)+
                      (1|Idiom), data = idiom)
drop1(idiom_model, test = "Chisq")
"Drop LP:Knowledge, AIC decreases from -957.60 to -958.75 and the p-value = 0.36"

idiom_model <- lmer(log_RTs ~ Condition + Condition:LP + Condition:Knowledge + 
                      Freqs + 
                      LP + LP:Transparency + 
                      Transparency + 
                      Knowledge +
                      (1|Subject)+
                      (1|Idiom), data = idiom)
drop1(idiom_model, test = "Chisq")
"Drop LP:Transparency, AIC decreases from -958.75 to -958.78 and the p-value = 0.16"

idiom_model <- lmer(log_RTs ~ Condition + Condition:LP + Condition:Knowledge + 
                      Freqs + 
                      LP + 
                      Transparency + 
                      Knowledge +
                      (1|Subject)+
                      (1|Idiom), data = idiom)
drop1(idiom_model, test = "Chisq")
"Drop Transparency, AIC decreases from -958.78 to -960.14 and the p-value = 0.42"

idiom_model <- lmer(log_RTs ~ Condition + Condition:LP + Condition:Knowledge + 
                      Freqs + 
                      LP + 
                      Knowledge +
                      (1|Subject)+
                      (1|Idiom), data = idiom)
drop1(idiom_model, test = "Chisq")
"Drop Condition:LP, AIC decreases from -960.14 to -960.16 and the p-value = 0.16"

idiom_model <- lmer(log_RTs ~ Condition + Condition:Knowledge + 
                      Freqs + 
                      LP + 
                      Knowledge +
                      (1|Subject)+
                      (1|Idiom), data = idiom)
drop1(idiom_model, test = "Chisq")
"Drop LP, AIC decreases from -960.16 to -961.41 and the p-value = 0.39"

idiom_model <- lmer(log_RTs ~ Condition + Condition:Knowledge + 
                      Freqs + 
                      Knowledge +
                      (1|Subject)+
                      (1|Idiom), data = idiom)
drop1(idiom_model, test = "Chisq")

"Model:
log_RTs ~ Condition + Condition:Knowledge + Freqs + Knowledge + 
(1 | Subject) + (1 | Idiom)
Df     AIC     LRT   Pr(Chi)    
<none>                 -961.41                      
Freqs                1 -933.02 30.3913 3.531e-08 ***
Condition:Knowledge  1 -959.91  3.4981   0.06144 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Dropping Condition:Knowledge would result in a higher AIC value and this interaction is almost signigicant (p-value = 0.06144)"

###Assumptions###
#Normality of residuals#
qq.plot(residuals(idiom_model))
"Data seems normal, only at the end of the line there is a deviation from normality"

#Homoscedasticity#
plot(idiom_model)
"The data seems to not violate the homoscedasticity assumption"


###Discussion###
idiom_model <- lmer(log_RTs ~ Condition + Condition:Knowledge + 
                      Freqs + 
                      Knowledge +
                      (1|Subject)+
                      (1|Idiom), data = idiom)
summary(idiom_model)
"Linear mixed model fit by REML 
t-tests use  Satterthwaite approximations to degrees of freedom ['lmerMod']
Formula: log_RTs ~ Condition + Condition:Knowledge + Freqs + Knowledge +      (1 | Subject) + (1 | Idiom)
   Data: idiom

REML criterion at convergence: -938.1

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-4.5101 -0.6363 -0.0750  0.5201  6.3402 

Random effects:
 Groups   Name        Variance Std.Dev.
 Subject  (Intercept) 0.019986 0.14137 
 Idiom    (Intercept) 0.001549 0.03936 
 Residual             0.033423 0.18282 
Number of obs: 2066, groups:  Subject, 44; Idiom, 25

Fixed effects:
                         Estimate Std. Error         df t value Pr(>|t|)    
(Intercept)             6.568e+00  4.373e-02  2.724e+02 150.214  < 2e-16 ***
ConditionLit           -5.022e-02  2.673e-02  2.005e+03  -1.879   0.0605 .  
Freqs                  -6.432e-02  1.163e-02  2.477e+02  -5.533 8.01e-08 ***
Knowledge              -1.031e-02  3.693e-03  1.720e+03  -2.792   0.0053 ** 
ConditionLit:Knowledge  8.552e-03  4.577e-03  2.003e+03   1.868   0.0619 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
            (Intr) CndtnL Freqs  Knwldg
ConditionLt -0.321                     
Freqs       -0.704  0.042              
Knowledge   -0.406  0.562 -0.085       
CndtnLt:Knw  0.215 -0.943  0.090 -0.601"

"The models formula is log_RTs ~ Condition + Condition:Knowledge + Freqs + Knowledge + (1 | Subject) + (1 | Idiom)

All predictors and interactions are significant (p-value < 0.05), except Condition and Condition:Knowledge which are almost significant (p-value = 0.6)
All regression coefficients, as well as their SE, t-scores and p-values are provided in the appendix. The variance explanation is (R2m = 0.024 ,R2c = 0.4)"

"For Freqs there is a negative relation to reaction time. This is to be expected, the more frequent a word the more you 
get used to it and thus recognize it faster. When the literal condition is used reaction time is lower then then when the condition is used figurative. So it seems that the 
literal meaning does activate the asciociation with other words more then then the figurative condtion does. Knowledge also has a negative relation with reaction time, again
a word you already know is recognized faster. 

The interaction Condition:Knowledge has a positive relation. This means that when the literal condition is combined with
high knowledge the reaction time is higher. This does not make much sense, unless we look at it from the other way. 
The other way the relation if obviously negative. What this means is that when we combine the figurative condtion with 
high knowledge the reaction time is higher then without knowledge. While the difference in time for the condition, without knowledge,
is considerable, when combined with a high knowledge this difference is almost 0."
r.squaredGLMM(idiom_model)

plot(allEffects(idiom_model))
plot(Effect(c("Condition","Knowledge"), idiom_model))
plot(Effect(c("Condition"), idiom_model))
plot(Effect(c("Freqs"), idiom_model))
plot(Effect(c("Knowledge"), idiom_model))

"More information can be seen in the plots, and the strengts of the relations can be seen in the appendix"


#########################
#########Attributions
#########################

#Datafile: clause_order_data.csv
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
"Model:
normV ~ Exp + Exp:storyType + bias + storyType + age + sex
              Df Sum of Sq    RSS    AIC F value    Pr(>F)    
<none>                     133642 1828.4                      
bias           1    2067.7 135710 1831.0  4.4868  0.035008 *  
age            1    1759.2 135402 1830.3  3.8173  0.051687 .  
sex            1    3397.1 137040 1833.9  7.3715  0.007024 ** 
Exp:storyType  1   23073.2 156716 1873.7 50.0682 1.119e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Dropping age would result in a higher AIC value and the p-value = 0.051 is almost significant"

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

###Discussion###
clause_order_model <- lm(normV ~ Exp + Exp:storyType + 
                           bias +
                           storyType +
                           age + 
                           sex, data = clause_order)
summary(clause_order_model)
"Call:
lm(formula = normV ~ Exp + Exp:storyType + bias + storyType + 
    age + sex, data = clause_order)

Residuals:
    Min      1Q  Median      3Q     Max 
-93.211 -10.899   3.186  12.747  69.239 

Coefficients:
                      Estimate Std. Error t value Pr(>|t|)    
(Intercept)           -13.5849     5.0106  -2.711  0.00710 ** 
ExpPos                 55.9614     3.6627  15.279  < 2e-16 ***
biaspositiveBias        5.2984     2.5014   2.118  0.03501 *  
storyTypemixed         14.6510     3.1405   4.665 4.71e-06 ***
age                    -0.1986     0.1016  -1.954  0.05169 .  
sexMale                -7.0485     2.5961  -2.715  0.00702 ** 
ExpPos:storyTypemixed -37.0445     5.2353  -7.076 1.12e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 21.47 on 290 degrees of freedom
Multiple R-squared:  0.4871,	Adjusted R-squared:  0.4765 
F-statistic: 45.91 on 6 and 290 DF,  p-value: < 2.2e-16

All predictors and interactions are significant (p-value < 0.05), except age which is almost significant (p-value = 0.051).
The model is significant overall (F 6,290 = 45.91, p-value: < 2.2e-16) and has a high variance explanation (mult.R-squared= 0.4871 ,adj.R-squared= 0.4765)
All regression coefficients, as well as their SE, t-scores and p-values are provided in the appendix."

"As one might expect, people with a negative biases give a lower rating then the ones with a positive bias. 
An relation that is a bit stereotypical is that females gives a higher rating then males. Age has a negative relationship, so 
older people give a worse rating. Exp is the sentence the policeman says, again like bias a negative expression results in a lower ratig
and positive expression in a higher rating. Also for a agreeing storyType results in a lower score then one mixed with positive elements.

The interaction between Exp:Story is that the ratings for both conditions become more extreme. In the mixed story the ratings have less absolute difference
for ExpPos and ExpNeg then for Story type accordant. This makes sense, if the story is disagreeing with the epression people put less
trust in it, and put more trust in it when it is consistent."

plot(allEffects(clause_order_model))
plot(Effect("storyType",clause_order_model))
plot(Effect("Exp",clause_order_model))
plot(Effect("bias",clause_order_model))
plot(Effect("age",clause_order_model))
plot(Effect("sex",clause_order_model))
plot(Effect(c("storyType","Exp"),clause_order_model))

"More information can be seen in the plots, and the strengts of the relations can be seen in the appendix"
