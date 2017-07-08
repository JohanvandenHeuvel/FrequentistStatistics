library(ggplot2)
library(car)
library(rms)

###In both the units on simple regression and multiple regression, one of the assumptions was that our output variable was interval or ratio. 
#Sometimes, however, we may want to predict a variable of another type. 
#In this practical, you are going to see how to adapt what you have learned about regression to the case of when you have a binary outcome variable. 
#The technique we will be using is called "Binary Logistic Regression." 

#It is called this because instead of fitting a straight line to your data points, R will fit an S-shaped curve to your data, called a logistic function. 
#The equation for the logistic function that R uses is:
#Y = 1 / (1 + e^-(b0 + b1 * X))

#Notice that this function is a bit more complex than the equation for a simple linear regression, but it still has a beta for slope, a beta for intercept, X, and Y. 
#There is in fact an entire chapter on logistic regression in your textbook (chapter 8), but we do not have time to do an in-depth study of it in this course. 
#Instead, you will get a "light" tutorial on it here.

#A real-world category that is often treated as binary in many cultures is gender. 
#Imagine a situation in which you wished to predict a person's gender on the basis of how much hair they lose after the age of 40. 
#This would be a situation where you could use a binary logistic regression. We are going to work through this small example to see how this technique works.

#1 First, load in the data from hair_data.csv. There are three columns:
#Hair_loss: the amount of hair loss an individual
#Gender: "m" or "f"
#Gender_Bool: a boolean representation of gender: this means females are coded as 0, and men are coded as 1

hair_data <- read.table(file.choose(), sep="\t", header=T, comment.char="", quote="")
head(hair_data)

#2 We're going to create a scatterplot with men in blue and women in red. 
#Hair_loss will be on the x-axis and Gender_Bool on the y-axis. Obviously, Gender_Bool and Gender give the same information, just in a different format. 
#The reason I have included Gender_Bool is because we are going to be thinking about probabilities (which range between 0 and 1) in a few minutes, and Gender_Bool will make it easier to do. 
#Run the following code:
ggplot(hair_data, aes(Hair_loss, Gender_Bool, col=Gender)) + geom_point()

#As you can see, the hair loss rate for women is lower (and less disperse) 
#than it is for men.


#3 Now, we're going to run our binary logistic regression, 
#predicting Gender as a function of Hair_loss. 
#Run the code below. We are using a new function lrm(), 
#which stands for logistic regression modeling.

#First, we have to create an object that summarizes 
#information about our variables using the datadist() function
dd <- datadist(hair_data)
options(datadist="dd")

#Then we create our model object and print it to the screen
model_1 <- lrm(Gender~Hair_loss, data=hair_data); model_1

#In the second column under "Model Likelihood Ratio Test", 
#there is a p-value (Pr(> chi2)) for the model. 
#It is less than 0.05, so it is significant.

#In the third column under "Discrimination Indexes", there is an R2 value. 
#This is calculated a bit differently from R2 that we've used before. 
#However, it still ranges between 0 and 1 with large values indicating better model fit. 
#The fact that our value here is only 0.407 indicates a fairly weak effect size.

#The table on bottom indicates that the Hair_loss predictor is significant at 0.0003. 
#It has a positive slope of 0.0615, indicating a positive relationship in the data. 
#THIS MEANS THAT AS HAIR_LOSS INCREASES, 
#THE MODEL PREDICTS THE CATEGORY 1 (MALE) WITH MORE AND 
#MORE PROBABILITY AND THE CATEGORY 0 (FEMALE) WITH LESS AND LESS PROBABILITY.


#4 We're now going to see what this means by plotting the 
#actual logistic curve onto our scatterplot. 
#First we need to use our model to get predicted values of
#our outcome variable (gender) for every value of our predictor (hair loss). 
#Run the following code to do this:
Predicted_gender <- predict(model_1)
head(Predicted_gender)

#You can see a problem. The predict() function doesn't simply give us a 
#vector of predicted m's and f's. 
#Instead, it gives us numerical values. 
#Why??? What do these values mean??? 
#Well, they specify how much the model believes each Hair_loss 
#value belongs to category 1 (male). 
#High values indicate that the model 
#really believes that the corresponding amount of hair loss is likely to belong to a man, 
#while low values indicate that the model 
#really believes that the corresponding amount of hair loss is likely to belong to a woman. 


#5 These predicted values are in the "log odds" scale, but we want them as probabilities. 
#To convert log odds values to probabilities, 
#we first need to define a function that will perform a kind of 
#transformation known as "ilogit". 
#Then we use it to transform our predicted values and 
#then we place the transformed values back in our spreadsheet
ilogit <- function(x) {1/(1+exp(-x))}

hair_data$Predicted_gender <- ilogit(Predicted_gender)
head(hair_data)

#Now, our predicted values indicate how probable the model 
#believes it is that a particular Hair_loss value belongs to a man. 
#Let's take the first row as an example.
#The model believes that there is a probability of 0.82 (82%) that a 
#Hair_loss value of 64.7 belongs to a man.


#6 We can add a line to our scatterplot that represents our regression: 
#on the x-axis is Hair_loss, and we use the new predicted probabilities 
#as our y-variable for the line. 
#Run the following code.

ggplot(hair_data, aes(Hair_loss, Gender_Bool, col=Gender)) + geom_point() + geom_line(aes(Hair_loss, Predicted_gender), colour="Green")

#The new green line shows us the logistic function that lrm() has fit to the data. 
#Again, it tells us how probable "male" is, given hair loss. 
#So, if the hair loss rate is 75, 
#the model predicts the "male" category with a probability of about 0.9 (90%), 
#since this is the y-value of the line when x=75.

#On the other hand, if the hair loss rate is only 25, 
#then the model predicts the "male" category with a probability of about 0.25. 
#Of course, this also means that the model predicts 
#the "female" category with a probability of about 0.75.



#7 Another analysis we can do is see how many times our 
#model correctly predicts male versus female for the different values of hair loss. 
#We will use a probability of 0.5 as a cutoff: for a given 
#hair loss value, if it predicts "male" with a probability greater than 0.5,
#we will say that it is predicting "male." 
#If it predicts "male" with a probability of less than 0.5, 
#we will say that it is predicting "female."

#First, we need to know how many rows (i.e., data points) we have
number_of_rows <- dim(hair_data)[1]; number_of_rows

#Then, we create a vector that will track how many correct 
#predictions our model makes. We start it by filling it in entirely with "incorrect" values.
correctness <- rep("incorrect",number_of_rows)

#We then run a for-loop that goes through each row and 
#checks if the predicted gender matches the actual gender and, 
#if it does, we change the corresponding element in our "correctness" 
#vector from "incorrect" back to "correct".
for (row_number in seq(number_of_rows)) {
  predicted_prob_of_man <- hair_data[row_number,"Predicted_gender"]
  actual_gender <- hair_data[row_number,"Gender"]
  if (predicted_prob_of_man > 0.5 & actual_gender == "m") {
    correctness[row_number] <- "correct"
  } else if (predicted_prob_of_man < 0.5 & actual_gender == "f") {
    correctness[row_number] <- "correct"
  }
}

#8 Finally, let's count how many correct and incorrect predictions our model made. 
#We can do this using table().

table(correctness)

#46 correct; 14 incorrect; which means that 47/60 = 76.67% of 
#hair loss values are classified correctly. 
#Since we have an even number of male and female data points, 
#our baseline would be an expected accuracy of 50%. 
#(That is, we we always just chose "male", we'd be right 50% of the time).







#9 Now, it's your turn! Load in the data from  voice.csv. 
#This spreadsheet contains data on the average pitch (meanfreq) of the voices 
#of men and women.
#Create a scatterplot to visualize the data 
#(actually a boxplot would be better here, but let's use a scatterplot to be consistent). 
voice_data <- read.table(file.choose(), sep="\t", header=T, comment.char="", quote="")
head(voice_data)

ggplot(voice_data, aes(meanfreq, label, col=label)) + geom_point()

#10 Now run a binary logistic regression predicting gender. 
#There's no boolean column anymore, 
#but you can continue to think of male as the "1" category and female as the "0" category 
#(as you can see in your scatterplot, mean is "above" and female is "below"). 
#Is the model significant? What is the R^2 value? 
#What direction does the relationship go in?
dd <- datadist(voice_data)
options(datadist="dd")
model_2 <- lrm(label~meanfreq, data=voice_data); model_2

#The model is signifticant as the p-value for Pr (> chi2) is lower then 0.05
#The R^2 value is 0.154 so there is a very weak correlation
#The meanfreq predictor is significant as the significant is <0.0001 and there is a 
#negative correlation of -17.80


#11 How often does our model correctly predict male versus female for the 
#different values of meanfreq? (Don't worry about plotting the regression line)
Predicted_gender <- predict(model_2)
head(Predicted_gender)

ilogit <- function(x) {1/(1+exp(-x))}

voice_data$Predicted_gender <- ilogit(Predicted_gender)
head(voice_data)

number_of_rows <- dim(voice_data)[1]; number_of_rows
correctness <- rep("incorrect",number_of_rows); 


for (row_number in seq(number_of_rows)) {
  predicted_prob_of_man <- voice_data[row_number,"Predicted_gender"]
  actual_gender <- voice_data[row_number,"label"]
  if (predicted_prob_of_man > 0.5 & actual_gender == "male") {
    correctness[row_number] <- "correct"
  } else if (predicted_prob_of_man < 0.5 & actual_gender == "female") {
    correctness[row_number] <- "correct"
  }
}

table(correctness)



####RAISE YOUR HAND TO HAVE A TA CHECK YOU OFF.