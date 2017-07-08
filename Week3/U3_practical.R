

####This practical activity is on R dataframes. Complete as much as you can during the session. The TAs will give you credit for making progress on the activity, but it's not necessary that you get every answer correct. However, the more progress you can make on this activity, the better prepared you will be to complete the homework.
rm(list=ls(all=TRUE))
setwd("~/Documents/RU/Sem2/Statistics/Week3")
#1 You must load the dataframe "reaction_times.csv". This contains data similar to the data you worked with in the practical session last week. Now, however, there are reaction times from 3 different study participants. When you load in the dataframe, please note that the cell separator used in this spreadsheet is a tab, so you should set this argument to "\t"
reaction_times <- read.table(file = "reaction_times.csv", header=TRUE, sep="\t", quote="", comment.char="", fill=TRUE)


#2 Inspect the contents of the dataframe, but don't print the entire dataframe to the screen!
str(reaction_times)             

#3 Print the last ten elements of the column titled REACTION_TIMES to the screen
col <- reaction_times$REACTION_TIMES
col <- tail(col,10)
col

#4 Change the element in the 4th column, 6th row to 545
reaction_times[4,6] = 545

#5 Print the 10th row to the screen
reaction_times[10,] = 545

#6 Add a column that indicates the gender of the participants. The first two participants are female and the third one is male.
gender_col <- rep(c('Female','Female','Male'), times = (60/3))
reaction_times$NEW_COLUMN <- gender_col

#7 Print a subset to the screen of all items that are outliers and are from participant 3
subset_reaction_times <- subset(reaction_times, PARTICIPANT==3 & OUTLIER_STATUS == "outlier")
subset_reaction_times

#8 Save a subset that contains only those rows with complete cases. Use this subset from now on. How many rows and columns does this new subset have?
dim(reaction_times)
complete_cases <- subset(reaction_times, complete.cases(reaction_times))
dim(reaction_times)

#9 Sort the dataframe by outlier status, then by word, then (descending) by reaction time.
reaction_times <- reaction_times[order(reaction_times$OUTLIER_STATUS,reaction_times$WORD,-reaction_times$REACTION_TIMES),]

#10 Note that the row names (numbers) on the left side of the dataframe do not correspond to the row indices you just created. This is because the removal of incomplete cases deleted certain row names, and then the sorting put the row names in a different order. Assign new row names to the rows. Use the integer values from 1 to the length of the columns.
rownames(reaction_times) <- 1:nrow(reaction_times)              


#11 Change all of the comma decimal characters in the frequency column to dots.
frequencies <- reaction_times$FREQUENCIES
frequencies <- gsub(",", ".", frequencies)
reaction_times$FREQUENCIES <- frequencies


#12 Now, use the function as.numeric() to convert the frequencies to numeric values.
frequencies <- reaction_times$FREQUENCIES
frequencies <- as.numeric(frequencies)
reaction_times$FREQUENCIES <- frequencies
reaction_times

#13 Create a vector of the row indices that contain frequencies between 50 and 100 in frequencies column. Then print this vector to the screen.
frequencies_readtimes <- subset(reaction_times, FREQUENCIES > 50 & FREQUENCIES < 100)
subset_frequencies <- which(reaction_times$FREQUENCIES > 50 & reaction_times$FREQUENCIES < 100)

#14 Now, use this index vector with bracket-based subsetting to print to the screen the corresponding rows of the dataframe. 
reaction_times[subset_frequencies,]

#15 Save the dataframe to the file "reaction_times_MODIFIED.csv". Use tab as the separator and set quote and row names to false.
write.table(reaction_times, "reaction_times_MODIFIED.csv", quote=FALSE, sep="\t", row.names=FALSE)

