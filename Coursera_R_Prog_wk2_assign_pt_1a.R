#########################################################################################
# Coursera Specialization Data Sccientist course: R_Programming, wk2 Assignment Part 1  #
# Austin Overman, January 29, 2016
# 
# Assignment:
# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate
# or nitrate) across a specified list of monitors. The function 'pollutantmean' takes 
# three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers,
# 'pollutantmean' reads that monitors' particulate matter data from the directory 
# specified in the 'directory' argument and returns the mean of the pollutant across 
# all of the monitors, ignoring any missing values coded as NA. 

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

## the function pullotantmean calculates the mean as described above. The function takes
## the variable 'directory' and re-assignes the working directory to the location where
## the data files are stored, initializes the variables to calculate the mean. The files
## are iterativly opened in the for loop and the total for the 'pollutant' variable over 
## all files in the sequence of 'id' is calculated as well as counting the total number 
## of real values in all files. The loop drops out to reset the working directory
## and print out a formated mean for 'pollutant' 
pollutantmean <- function(directory, pollutant, id = 1:332) {
      
      setwd(paste(getwd(), "/", directory,"/", sep = ""))
      total <- 0
      n <- 0
      
      for(i in id){
            file <- paste(str_pad(i, 3, pad = "0"), ".csv", sep = "")
            monitor <- read.csv(file)
            total <- total + sum(monitor[[pollutant]], na.rm = TRUE)
            n <- n + sum(!is.na(monitor[[pollutant]]))
      }
      
      setwd(owd)
      print(signif(total/n,4))
      
}



