#########################################################################################
# Coursera Specialization Data Sccientist course: R_Programming, wk2 Assignment Part 3  #
# Austin Overman, January 30, 2016
# 
# Assignment:
# Write a function that takes a directory of data files and a threshold for complete cases
# and calculates the correlation between sulfate and nitrate for monitor locations where 
# the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.  
# 
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
library(stats)
owd <- getwd()

## corr function takes the 'directory' variable and sets the the working directory to the 
## used defined name where the data are. The number of files is calculated to bound the 
## the data frame and the for loop, and the data frame is initiated. The for loop uses
## the count of files, and iterates through all calculating the number of compelte
## observations and the correlation between the two variable, and fills the data frame 
## with the results for all monitors. The function returns a vector of the correlations
## for those monitors that had a pair wise complete set of data greater than the 
## user defined threshold.
corr <- function(directory, threshold = 0) {
      
      setwd(paste(getwd(), "/", directory,"/", sep = ""))
      list <- length(list.files(getwd()))
      completed <- data.frame(x = numeric(list), 
                              y = numeric(list),
                              z = numeric(list))

      for(i in 1:length(list.files(getwd()))){
            file <- paste(str_pad(i, 3, pad = "0"), ".csv", sep = "")
            monitor <- read.csv(file)
            monitor <- subset(monitor, select = c(sulfate, nitrate))
            correlation <- cor(monitor, use = "pairwise.complete.obs")
            total <- na.omit(monitor)
            completed[i,] <- list(i, nrow(total), correlation[1,2])
      }
      
      colnames(completed) <- c("id", "nobs", "corr")
      completed <- subset(completed, completed$nobs > threshold)
     
      setwd(owd)
      cr_vec <- completed$corr
      cr_vec
      
}

# quiz # 8
cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

# quiz # 9
cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

# quiz #10
cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
