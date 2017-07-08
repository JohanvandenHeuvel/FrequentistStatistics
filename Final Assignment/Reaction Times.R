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
The D-w Statistic does return a value >2, however the p-value is not significant.
As the p-value is near the 0.05 we reject the null-hypothesis, which state that there is no autocorrelation.
However the p-value is close to 0.05 and the D-W Statistic >2, so it is very close to violating the assumption of autocorrelation.
In this case we can still use the model."

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
summary.aov(model_reaction_times)
"            Df Sum Sq Mean Sq F value  Pr(>F)   
FREQUENCY    1  16230   16230   8.679 0.00541 **
FAMILIARITY  1   5222    5222   2.792 0.10272   
Residuals   39  72928    1870                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"

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

Both FREQUENCY and FAMILIARITY have a negative relation to the RT, so the higher these values the lower the reaction time. In the plot we can see that the strenght
of the relation in roughly in the same order."

plot(allEffects(model_reaction_times))