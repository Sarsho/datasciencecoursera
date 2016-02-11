## Coursera R-Programming wk 4 Assignment 3, part 4
## Austin Overman


## setting up the directory where the data are and saving the original working
## directory to reset when done.

owd <- getwd()
setwd(paste(getwd(), "/", "R_program_assignment_4","/", sep = ""))

## reading in the data
outcomes_df <- read.table("outcome-of-care-measures.csv", header = TRUE, sep = ",",
           na.strings = "Not Available", stringsAsFactors = FALSE)

## rankall function takes arguments for the the mortality type and its rate
## rank and returns a list of hospital names and their states for the defined
## mortality type and ranked rate.
rankall <- function(outcome, num = "best"){
      
      ## sets up the validation criteria based on knowledge of the data
      outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
      
      ## detemines if the outcome argument is valid in the data
      if(outcome %in% names(outcomes) == FALSE)
            stop("invalid outcome")
      
      ## index: 2 = Hospital, 7 = State, outcomes[outcome] = rate
      outcomes_df <- outcomes_df[, c(2,7,outcomes[outcome])]
      outcomes_df <- outcomes_df[complete.cases(outcomes_df),]
      
      ## index: 1 = Hospital, 2 = State, 3 = rate
      outcomes_df <- outcomes_df[order(outcomes_df[,2], outcomes_df[,3],
                                 outcomes_df[,1]),]
     
      ## the ordered data are split peer state creating a vector of
      ## hospital names and the state for their specified rate/rank
      split_outcomes <- split(outcomes_df, outcomes_df$State)
      hospital_name <- unlist(lapply(split_outcomes, function(rank){
            if(num == "best") num <- 1
            if(num == "worst") num <- nrow(rank)
            rank[num,1]
            })) 
      state_name <- names(hospital_name)
      all_ranked <- data.frame(hospital=hospital_name, state=state_name, 
                               row.names=state_name)
      all_ranked
}


setwd(owd)