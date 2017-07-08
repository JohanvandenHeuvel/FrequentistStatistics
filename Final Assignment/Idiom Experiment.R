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

idiom_model <- lmer(log_RTs ~ Condition + Condition:Knowledge + 
                      Freqs + 
                      Knowledge +
                      (1|Subject)+
                      (1|Idiom), data = idiom)



r.squaredGLMM(idiom_model)

par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
plot(idiom_model)

plot(residuals(idiom_model))

coef(idiom_model)

ggplot(PressureData, aes(Hour, BloodPressure, color=Subject)) 
  + geom_point() + geom_smooth(method="lm") 
  + geom_abline(intercept=65.72, slope=.59, color="red") 
  + geom_abline(intercept=89.86, slope=.59, color="red") 
  + geom_abline(intercept=101.13, slope=.59, color="red") 
  + geom_abline(intercept=156.29, slope=.59, color="red") 
  + geom_abline(intercept=128.95, slope=.59, color="red") 
  + geom_abline(intercept=108.39, slope=.59, color="green")

library(coefplot2)
coefplot2(idiom_model)

###Assumptions###
#Autocorrelation#


#Normality of residuals#


#Homoscedasticity#


#Outliers#


#Multicollinearity#


#Linearity"
"????????"


plot(allEffects(idiom_model))
plot(Effect("Flippers", idiom_model))

