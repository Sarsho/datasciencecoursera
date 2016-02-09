## Coursera R-Programming wk 4 Assignment 3
## Austin Overman


## setting up the directory where the data are and saving the original working
## directory to reset when done.

owd <- getwd()
setwd(paste(getwd(), "/", "R_program_assignment_4","/", sep = ""))

## reading in the data
outcomes_df <- read.table("outcome-of-care-measures.csv", header = TRUE, sep = ",",
           na.strings = "Not Available", stringsAsFactors = FALSE)

## doing a little exploration
nrow(outcomes_df)
ncol(outcomes_df)
names(outcomes_df)

summary(outcomes_df)
str(outcomes_df)

outcomes_df[,11] <- as.numeric(outcomes_df[,11])
hist(outcomes_df[,11])
head(outcomes_df[,11],100)
summary(outcomes_df[,11])
names(outcomes_df[11])

outcomes_df[,13] <- as.numeric(outcomes_df[,13])
hist(outcomes_df[,13])
head(outcomes_df[,13],100)
summary(outcomes_df[,13])
names(outcomes_df[13])

outcomes_df[,14] <- as.numeric(outcomes_df[,14])
hist(outcomes_df[,14])
head(outcomes_df[,14],100)
summary(outcomes_df[,14])
names(outcomes_df[14])

outcomes_df[,2] <- as.factor(outcomes_df[,2])
unique(outcomes_df[,2][duplicated(outcomes_df[,2])])

outcomes_df[,1] <- as.factor(outcomes_df[,1])
unique(outcomes_df[,1][duplicated(outcomes_df[,1])])
str(outcomes_df[,1])

states <- unique(outcomes_df[,7][duplicated(outcomes_df[,7])])
outcomes <- c("heart attack", "heart failure", "pneumonia")

best <- function(state, outcome){
      if(is.element(state, states) == FALSE) 
            stop("invalid state")
      if(is.element(outcome, outcomes) == FALSE)
            stop("invalid outcome")
      
      c(state, outcome)
      
}

setwd(owd)