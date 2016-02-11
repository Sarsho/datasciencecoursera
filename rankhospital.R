## Coursera R-Programming wk 4 Assignment 3, part 3
## Austin Overman


## setting up the directory where the data are and saving the original working
## directory to reset when done.

owd <- getwd()
setwd(paste(getwd(), "/", "R_program_assignment_4","/", sep = ""))

## reading in the data
outcomes_df <- read.table("outcome-of-care-measures.csv", header = TRUE, sep = ",",
           na.strings = "Not Available", stringsAsFactors = FALSE)

## best function takes arguments for the state name (abbreviation) and the mortality
## outcome type and returns the name of the state hospital with the lowest rate.
rankhospital <- function(state, outcome, num){
      
      ## sets up the validation criteria based on knowledge of the data
      states <- unique(outcomes_df[,7][duplicated(outcomes_df[,7])])
      outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
      
      ## detemines if the state and outcome argument are valid in the data
      if(is.element(state, states) == FALSE) 
            stop("invalid state")
      if(outcome %in% names(outcomes) == FALSE)
            stop("invalid outcome")
      
      ## index: 2 = Hospital, 7 = State, outcomes[outcome] = rate
      outcomes_df <- outcomes_df[, c(2,7,outcomes[outcome])]
      outcomes_df <- outcomes_df[complete.cases(outcomes_df),]
      
      ## index: 1 = Hospital, 2 = State, 3 = rate
      outcomes_df <- outcomes_df[order(outcomes_df[,2], outcomes_df[,3],
                                 outcomes_df[,1]),]
      
      ## subsets outcomes to just the state function argument
      outcomes_df <- subset(outcomes_df, outcomes_df[[2]]==state)
      
      ## assigns index values for num if option is 'best' or 'worst'
      if(num == "best") num <- 1
      if(num == "worst") num <- nrow(outcomes_df)
      
      ## returns the name of the hospital in the defined state and rank
      outcomes_df[num,1]
     
}

setwd(owd)