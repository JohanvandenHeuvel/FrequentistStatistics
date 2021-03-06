#Intructions: In Part 1 below, you and your partner will find a list of programming instructions. For each set of instructions, you must type up the code that will complete the described task. For the most part, a set of instructions will require only 1-3 lines of code, but a couple of sets of instructions may require longer scripts. You may refer to the U2_Lecture_Vectors_and_Factors.r code file (which I went over in lecture) for help with how to execute various programming tasks in R. Note that we did not have time to cover factors, but I have added some additional desciptive text regarding factors in the lecture code file, so you should be able to complete the factor instructions below. 

#In Part 2, you will find some short response questions that address material that we covered in this week's (and last week's ) lectures. Please answer each one in a paragraph or so.

#When you and your partner have completed this activity, you should save this name this code file with the last names of the two partners as well as the unit number (here, unit 2). For example: Wahl_Smith_U2.r   You can then submit your completed assignment to Blackboard.


###Part I: Vectors & Factors

#Clear all memory
rm(list=ls(all=TRUE)) 
setwd("~/Documents/RU/Sem2/Statistics")

#Load in the vector Reaction_Times.txt and save it to a variable.
a_numeric_vector <- scan(file = "reaction_times.txt", sep="\n") 

#Check the length of the vector
length(a_numeric_vector)

#Print the first 10 elements of the vector (use two different ways to do this). 
#The values of the vector represent the amount of time (in milliseconds) that a single participant in a psychological study took to respond to different stimuli (words).
a_numeric_vector[1:10]
a_numeric_vector[c(1,2,3,4,5,6,7,8,9,10)]

#We realize that we made some mistakes when we originally created this vector. We need to fix those.
#First, we left out some values. Create a vector of the values 345, 452, 671, 555, and 903 after the 5th element of the vector
vectorfix <- c(345,452,671,555,903)
a_numeric_vector <- append(a_numeric_vector,vectorfix,5) 

#The stimuli in this study are words. Let's assign the words that correspond to each reaction time as names of the reaction time vector you've created. 
#The words are in Words.txt. Load them in as another vector and assign them as names of the reaction time vector.
names(a_numeric_vector) <- scan(file = "words.txt", what = character(), sep="\n")

#You've discovered that there was a problem in the computer program used to collect the experimental data in the study. 
#Every even-numbered word in the vector is slow by 15 milliseconds. Add 15 milliseconds to each even-numbered word.
a_numeric_vector[seq(0,length(a_numeric_vector),2)] <- a_numeric_vector[seq(0,length(a_numeric_vector),2)] + 15

#Now order the values from smallest to largest and save to the variable data_ordered.
data_ordered <- sort(a_numeric_vector)

#You change your mind. Remove data_ordered. You decide you actually want to randomize the order of the items. 
#Hint: if you can generate a random vector of numbers the same length as the reaction times vector, you can use this as an index to reorder the reaction times.
rm(data_ordered)
set.seed(42); a_numeric_vector = sample(a_numeric_vector)

#You believe that any reaction times below 300 milliseconds or above 700 milliseconds are "outliers"--that is, 
#unrealistic data points that should not be included in your final analysis. Find the indices of all times that fit this criterion 
#and save them to a variable named "outlier_indices". Then print the outlier reaction times themselves to the screen.
outlier_indices <- a_numeric_vector[a_numeric_vector<300 | a_numeric_vector>700]
outlier_indices

#Create a new vector that is the same length as the reaction time vector and indicates whether or not each value is an outlier. 
#The possible values of the vector should be "outlier" and "not outlier". Do not simply use c("not outlier","not outlier","outlier",...).
outlier <- ifelse(a_numeric_vector<300 | a_numeric_vector>700,"outlier","not outlier")  

#Convert this vector to a factor.
a_factor <- factor(outlier)

#Print to the screen all reaction times that are within 100 milliseconds of the mean. Look up help on the "mean" function to see how to use it.
help(mean)
reactionTimes <- a_numeric_vector[a_numeric_vector > mean(a_numeric_vector) - 100 & a_numeric_vector < mean(a_numeric_vector) + 100]
reactionTimes

#You realize the you made a typo when you entered the reaction time 452. It should have been 542 In a single line of code, 
#find and replace this incorrect value with the correct one.
a_numeric_vector[match(452,a_numeric_vector)] <- 542

#Load another vector from Frequencies.txt. This contains the text frequency per million words for each word. You will need to 
#use the same random indices used above to reorder the reaction times here to get the frequencies in the proper order.
frequencies <- scan(file = "frequencies.txt", sep="\n") 
set.seed(42); frequencies = sample(frequencies)

#Round these frequencies to a single decimal place. You have not learned how to do this yet. Look up the help for the "round" function.
help(round)
frequencies <- round(frequencies,digits = 1)

#You are curious how many different frequencies there are in the vector. How do you figure this out in a single line of code?
length(unique(frequencies))

#You want to relevel your factor. Instead of just having a single "outlier" category, you want two categories: "too small" and "too big" for outliers that are below 300 and 
#above 700, respectively. Add a new level to the factor, then use a for-loop to go through and change each instance of "outlier" to one of the two new levels.
outlier[which(a_numeric_vector<300 | a_numeric_vector>700)] <- ifelse(outlier_indices<300,"too small","too big")
outlier


#Part II

#Explain the difference between ordinal, interval, and ratio variables using examples.
#Ordinal: First place, second place or third place in a competition
#Interval: Degrees celcius, 0 degrees is not not heat energie
#Ratio: Degress kelvin, 0 degrees is abosulte 0


#Describe an example of Tertium Quid
#Tertium Quid: Ice cream aten and murders in the summer, connected by more people outside and the heat


#What is counterbalancing?
#Counterbalancing: When you have a within-test you split the group and then start experiment A with group 1 
#and experiment B with group 2, after then you switch

#Can you think of two situations where you have a distribution of values from a numerical variable, 
#but the mode would not be a particularly good measure of central tendency?
#1: Income, the income with the highest frequency does not say much about how the poorest people live
#2: Grades, the mode will not say anything about the low or high grades


