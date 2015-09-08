# RANKALL: Function that returns the hospital names associated with the x ranked hospital for a named
# 30-day disease mortality rate as collected by Dept. of Health & Human Services
# Author: Brandon Hoeft
# Date: September 7, 2015

setwd("~/Documents/R Files/R Programming (Coursera 2015)/Programming Assignment 3")

# Dataset background: The data for this assignment come from the Hospital Compare web site 
# (http://hospitalcompare.hhs.gov) run by the U.S. Department of Health and Human Services.

# Objective:
# Write a function called rankall that takes 2 arguments:  an outcome name (outcome), and the 
# ranking of a hospital for that outcome (num). The function reads the outcome-of-care-measures.csv
# file and returns a 2-column dataframe containing the hospital in each state that has the ranking
# specified in num argument. Example: rankall ("heart attack", "best") would return the name of the 
# hospital in each state with the best (lowest) 30-day mortality rate for heart attacks. 

rankall <- function (outcome, num = "best") {
    # read the HHS outcomes data file and columns needed.
    temp <- read.csv("outcome-of-care-measures.csv", colClass = "character")[, c(2, 7, 11, 17, 23)]
    names(temp)[3:5] <- c("heart attack", "heart failure", "pneumonia") # rename disease columns
    
    # Check argument validity. Stop function if argument inputs invalid
    if (all(names(temp)[3:5] != outcome)) {
        stop("invalid outcome")
    } 
    
    # Suppress the warning for coercing non-numerics to NAs during as.numeric
    temp[, outcome] <- suppressWarnings(as.numeric(temp[, outcome]))
    
    # remove hospital rows with NA values and keep Hospital, State, and specified outcome variables
    # by default, NAs for outcome are ordered last in temp3, so no need to drop NAs from the dataset.
    temp <- temp[, c("Hospital.Name", "State", outcome)]  
    
    # split temp into 54 subset dataframes based on state. these subsets are stored in a new list 
    temp2 <- split(x = temp, f = temp$State)
    
    # over each dframe element in the list, reorder by mortality rate then ascend hospital name for ties
    temp3 <- lapply(temp2, function(x) { x[order(x[, outcome], x[,1]), ] }) 
    
    # if "num" argument is called with character "best" or "worst" ranking.
    if (num == "best") { num <- 1}
    #if (num == "worst") {num <- }
    
    # subset out only the specified "num" argument ranked record from each data frame in the list
    temp4 <- lapply(temp3, function(x) { x[num, 1:2]})
    
    # write a for() loop to extract name and state from temp3 list elements, store values in new 
    # vectors, then bind them together in a new data frame that will be the rankall function output.
    hospital <- c()
    state <- c()
    
    for (i in 1:length(temp4)) {
        hospital[i] <- temp4[[i]][,1] # select the hospital name from each element of temp4
        state[i] <- temp4[[i]][,2] # select the state of hospital from each element of temp4
    }
    
    final.dframe <- data.frame(hospital, state)
    
    final.dframe
}



# Test out the function
head(rankall("heart attack", 20), 10)
rankall("pneumonia")
tail(rankall("heart failure"), 10)
