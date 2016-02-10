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

## best function takes arguments for the state name (abbreviation) and the mortality
## outcome type and returns the name of the state hospital with the lowest rate.
best <- function(state, outcome){
      states <- unique(outcomes_df[,7][duplicated(outcomes_df[,7])])
      outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
      
      if(is.element(state, states) == FALSE) 
            stop("invalid state")
      if(outcome %in% names(outcomes) == FALSE)
            stop("invalid outcome")
      
      outcomes_df <- outcomes_df[, c(2,7,outcomes[outcome])]
      outcomes_df <- outcomes_df[complete.cases(outcomes_df),]
      
      outcomes_df <- outcomes_df[order(outcomes_df[,2], outcomes_df[,3],
                                 outcomes_df[,1]),]
      
      outcomes_df <- subset(outcomes_df, outcomes_df[[2]]==state)
      outcomes_df[1,1]
}

setwd(owd)