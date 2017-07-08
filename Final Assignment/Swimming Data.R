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

###Model selection###
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
sum(hatvalues(model_swimming) > 2*0.05731225) #10
sum(hatvalues(model_swimming) > 3*0.05731225) #0
"There seem to be quite alot of high-leverage points"
"There do not seem to be any outliers or influential points. There are 10 high-leverage points"

#Multicollinearity#
vif(model_swimming)
"There does not seem to be a problem with multicollinearity"


###Discussion###

model_swimming <-lm(Time ~ 
                      Shirt + Shirt:Flippers+ 
                      Goggles + Goggles:Flippers + 
                      Flippers, data=swimming)

summary.aov(model_swimming)
"                 Df Sum Sq Mean Sq F value   Pr(>F)    
Shirt             1   60.6   60.64  11.501 0.001196 ** 
Goggles           1   19.5   19.50   3.699 0.058909 .  
Flippers          1  153.2  153.19  29.058 1.08e-06 ***
Shirt:Flippers    1    4.9    4.87   0.923 0.340279    
Flippers:Goggles  1   84.8   84.76  16.077 0.000162 ***
Residuals        64  337.4    5.27                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"

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
For the other predictors: swimming with a Shirt on makes you slower, swimming with Goggles or Flippers makes you faster. These relations make sound very obvious.
There are two interactions: Shirt:Flippers and Flippers:Goggles. If we plot these we can see the relation very good. As one might expect: swimming with a flippers makes
swimming with a shirt faster. Swimming with flippers abd goggles makes you actually slower then swimming with flippers and without goggles. 
This is a counter-intuitive, both goggles and flippers makes you faster, and combining then makes you slower. A possible explanation is that by combining them the weight slows you down."

plot(allEffects(model_swimming))
plot(Effect("Goggles", model_swimming))