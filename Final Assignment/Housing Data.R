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
                           B:LSTAT + B:CRIM + B:INDUS + B:CHAS + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + B:PTRATIO + B:MEDV + 
                           LSTAT:CRIM + LSTAT:INDUS + LSTAT:CHAS + LSTAT:NOX + LSTAT:RM + LSTAT:AGE + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Drop LSTAT:CHAS, AIC decreases from 379.69 to 377.75 and the p-value = 0.81"

model_housing_data <- lm(CRIM ~ INDUS + CHAS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:CRIM + B:INDUS + B:CHAS + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + B:PTRATIO + B:MEDV + 
                           LSTAT:CRIM + LSTAT:INDUS + LSTAT:NOX + LSTAT:RM + LSTAT:AGE + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Drop LSTAT:INDUS, AIC decreases from 377.75 to 375.92 and the p-value = 0.70"

model_housing_data <- lm(CRIM ~ INDUS + CHAS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:CRIM + B:INDUS + B:CHAS + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + B:PTRATIO + B:MEDV + 
                           LSTAT:CRIM + LSTAT:NOX + LSTAT:RM + LSTAT:AGE + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Drop LSTAT:AGE, AIC decreases from 375.92 to 374.31 and the p-value = 0.54"

model_housing_data <- lm(CRIM ~ INDUS + CHAS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:CRIM + B:INDUS + B:CHAS + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + B:PTRATIO + B:MEDV + 
                           LSTAT:CRIM + LSTAT:NOX + LSTAT:RM + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Drop B:CHAS, AIC decreases from 374.31 to 373.29 and the p-value = 0.34"

model_housing_data <- lm(CRIM ~ INDUS + CHAS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:CRIM + B:INDUS + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + B:PTRATIO + B:MEDV + 
                           LSTAT:CRIM + LSTAT:NOX + LSTAT:RM + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Drop CHAS, AIC decreases from 373.29 to 371.33 and the p-value = 0.85"

"#################"
model_housing_data <- lm(CRIM ~ INDUS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:CRIM + B:INDUS + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + B:PTRATIO + B:MEDV + 
                           LSTAT:CRIM + LSTAT:NOX + LSTAT:RM + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Drop INDUS:B, AIC increases from 371.33 to 372.06 and the p-value = 0.11"

model_housing_data <- lm(CRIM ~ INDUS + NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:CRIM + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + B:PTRATIO + B:MEDV + 
                           LSTAT:CRIM + LSTAT:NOX + LSTAT:RM + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Drop INDUS, AIC increases from 372.06 to 370.17 and the p-value = 0.75"

model_housing_data <- lm(CRIM ~ NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:CRIM + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + B:PTRATIO + B:MEDV + 
                           LSTAT:CRIM + LSTAT:NOX + LSTAT:RM + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Drop PTRATIO:B, AIC increases from 370.17 to 370.73 and the p-value = 0.12"

model_housing_data <- lm(CRIM ~ NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:CRIM + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + B:MEDV + 
                           LSTAT:CRIM + LSTAT:NOX + LSTAT:RM + LSTAT:DIS + LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')
"Model:
CRIM ~ NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
B:LSTAT + B:CRIM + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + 
B:MEDV + LSTAT:CRIM + LSTAT:NOX + LSTAT:RM + LSTAT:DIS + 
LSTAT:RAD + LSTAT:PTRATIO + LSTAT:MEDV
Df Sum of Sq    RSS     AIC   F value    Pr(>F)    
<none>                      953.7  370.73                        
B:LSTAT        1    346.63 1300.4  525.60  174.8154 < 2.2e-16 ***
CRIM:B         1    288.16 1241.9  502.32  145.3270 < 2.2e-16 ***
NOX:B          1     62.11 1015.8  400.65   31.3230 3.675e-08 ***
RM:B           1     21.75  975.5  380.14   10.9717 0.0009948 ***
AGE:B          1     75.80 1029.5  407.43   38.2293 1.344e-09 ***
DIS:B          1     73.48 1027.2  406.28   37.0574 2.346e-09 ***
RAD:B          1     24.29  978.0  381.45   12.2495 0.0005087 ***
B:MEDV         1     81.73 1035.5  410.33   41.2172 3.272e-10 ***
CRIM:LSTAT     1   2931.06 3884.8 1079.37 1478.2241 < 2.2e-16 ***
NOX:LSTAT      1     11.66  965.4  374.88    5.8802 0.0156796 *  
RM:LSTAT       1     30.81  984.5  384.82   15.5377 9.288e-05 ***
DIS:LSTAT      1      5.72  959.5  371.76    2.8864 0.0899744 .  
RAD:LSTAT      1    226.16 1179.9  476.40  114.0590 < 2.2e-16 ***
PTRATIO:LSTAT  1     14.48  968.2  376.36    7.3042 0.0071226 ** 
LSTAT:MEDV     1     28.58  982.3  383.67   14.4146 0.0001654 ***"

model_housing_data <- lm(CRIM ~ NOX + RM + AGE + DIS + RAD + PTRATIO + B + LSTAT + MEDV + 
                           B:LSTAT + B:CRIM + B:NOX + B:RM + B:AGE + B:DIS + B:RAD + B:MEDV + 
                           LSTAT:CRIM + LSTAT:RM + LSTAT:RAD + LSTAT:MEDV
                         , data=housing_data)
drop1(model_housing_data, test = 'F')

summary(model_housing_data)

###Assumptions###
#Autocorrelation#
durbinWatsonTest(model_housing_data)
"D-W Statistic = 1.86487, p-value = 0.046
The D-W Statistic does return a value close >2, and the p-value is significant.
As the p-value is significant we reject the null-hypothesis, which states that there is no autocorrelation.
As the D-W Statistic is close to >2, and the p-value is <0.05 we can assume that the Autocorrelation assumption is violated"

par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
plot(model_housing_data)

#Normality of residuals#
"The residuals seem normal distributed in the middle, however in the start and the end there is a very strong deviation from the normal distribution"

#Homoscedasticity#
"The assumption of homscedasticty does seem to be violated a little bit"

#Outliers#
sum(rstandard(model_housing_data) > 3.29) #6
sum(rstandard(model_housing_data) < -3.29) #7
"There seem to be 13 outliers"

sum(cooks.distance(model_housing_data) > 1) #5
"There are five points with a cooks distance of >1, there seem to be 5 influential cases"

mean(hatvalues(model_housing_data)) #0.05731225
sum(hatvalues(model_housing_data) > 2*0.05731225) #58
sum(hatvalues(model_housing_data) > 3*0.05731225) #33
"There seem to be quite alot of high-leverage points"

#Multicollinearity#
vif(model_housing_data)
"There does not seem to be a problem with multicollinearity"

#Linearity"
"????????"


plot(allEffects(model_reaction_times))
plot(Effect("Flippers", model_reaction_times))






model_housing_data <- lm(CRIM ~ INDUS + INDUS:CHAS + INDUS:NOX + INDUS:RM + INDUS:AGE + INDUS:DIS + INDUS:RAD + INDUS:PTRATIO + INDUS:B + INDUS:LSTAT + INDUS:MEDV + 
                           CHAS + CHAS:NOX + CHAS:RM + CHAS:AGE + CHAS:DIS + CHAS:RAD + CHAS:PTRATIO + CHAS:B + CHAS:LSTAT + CHAS:MEDV + 
                           NOX + NOX:RM + NOX:AGE + NOX:AGE + NOX:DIS + NOX:RAD + NOX:PTRATIO + NOX:B + NOX:LSTAT + NOX:MEDV +
                           RM + RM:AGE + RM:DIS + RM:RAD + RM:PTRATIO + RM:B + RM:LSTAT + RM:MEDV +
                           AGE + AGE:DIS + AGE:RAD + AGE:PTRATIO + AGE:B + AGE:LSTAT + AGE:MEDV +
                           DIS + DIS:RAD + DIS:PTRATIO + DIS:B + DIS:LSTAT + DIS:MEDV +
                           RAD + RAD:PTRATIO + RAD:B + RAD:LSTAT + DIS:MEDV +
                           PTRATIO + PTRATIO:B + PTRATIO:LSTAT + PTRATIO:MEDV +
                           B + B:LSTAT + B:MEDV +
                           LSTAT + LSTAT:MEDV +
                           MEDV, data = housing_data)
summary(model_housing_data)