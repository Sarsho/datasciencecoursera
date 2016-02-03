#########################################################################################
# Coursera Specialization Data Sccientist course: R_Programming, wk2 Assignment Part 2  #
# Austin Overman, January 30, 2016
# 
# Assignment:
# Write a function that reads a directory full of files and reports the number of 
# completely observed cases in each data file. The function should return a data frame 
# where the first column is the name of the file and the second column is the number of 
# complete cases.  

# found at: https://eventing.coursera.org/api/redirectStrict/0xFn9X7RqYEk0OItxheERVUWNO4
# pMq_Bgh8C2UwevUyI_WM-YhIMVQYR9ppgYti1xvEIZxkEHZGaeLvAb8udXw.ufDqfKBqZj-
# 6yGT88jLDoA.8IZTjm22XXo93Ga4W0Yf5AwtSeslFgBiMjfEbp1qvSfTvnP5tVPHxu9tTkM2HFx65TzvLi
# l0DGhNi30WUQEoN9x-04Xi8prxtghyCpGWuZfixpQLvL6OEMZB0tvMtbZi5_JcIJYBLVlk94rRJ7SRwDyuSXX
# 8pGlIvZ3vSC2CwP7C4O_Epf2LQCYdKT4oXQAgpWcAJRQfcfUuVld1cFnrU3rsU_Bs1BZSFTdCx6x3KKGl8y
# REJ7x8JaCG35UQUENoBT4PsvMEv2s9rNYqcdnzsp1pFT5053GJnDRf79IyYLQR8sF7pjeh_OKlumSLnPgKgKa4
# -bgC9AJ78UvAKFtx3nWQwf9cvZKn2qgloDH9aB5Tt_B2TVqgl6ZqnfV9C15E
########################################################################################

## setting a seed for reproducibility, initilizing librarys and saving original 
## working directory to return to initial state
set.seed(1001)
library(stringr)
owd <- getwd()

## completed function takes the 'directory' variable and sets the working directory to 
## the user defined directory where the data are; and the variable 'id' as a vector of 
## monitor files to be evaluated. The data frame 'completed' is initiated with size
## defined by the length of the vector 'id'. The for loop of length 'id' extracts each
## file by the 'id' number and and writes the monitor id and calculates the number of 
## compelte cases in each monitor 'id' file then the function returns the data frame
## of the monitor id and complete cases in each.
complete <- function(directory, id) {
      
      setwd(paste(getwd(), "/", directory,"/", sep = ""))
      completed <- data.frame(x = numeric(length(id)), y = numeric(length(id)))

      for(i in 1:length(id)){
            file <- paste(str_pad(id[i], 3, pad = "0"), ".csv", sep = "")
            monitor <- read.csv(file)
            monitor <- subset(monitor, select = c(sulfate, nitrate))
            total <- na.omit(monitor)
            completed[i,] <- list(id[i], nrow(total))
      }

      colnames(completed) <- c("id", "nobs")
      setwd(owd)
      completed
      
}
