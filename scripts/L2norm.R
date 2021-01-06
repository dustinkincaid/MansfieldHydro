### L2norm.R
### Script to normalize data
### 28 Jan 2016
### Kristen Underwood

# This function performs an L2 normalization of data to scale values between 0 and 1
# (xi - min(x))/ (max(x)-min(x))

# Input:
# x = matrix of numerical data

# Output: 
# y = matrix of numerical data that have been scaled (by column) between 0 and 1


# ------------- Housekeeping -------------------------------------------------

# set working directory
# setwd("C:/R SOM/SedRegimeData")

# ---------------- Preprocess Input Data -----------------------------------------
### Load and visualize data set
# myData <- read.csv("SedRegDataV1a.csv",na.strings=".", header=T)
# Data set for this version script is Phase 1 and 2 SGA data for class project
# watersheds including transport reaches and wetlands reaches.
# 146 obs of 13 variables 

# #subset the variables that are numeric (lop off Obs # and Reach #)
# myData_num <- myData[ ,3:15]  
# head(myData_num)
# x <- myData_num[1:9, ]  #create a small data set to work with
# class(x)  #[1] "data.frame"

# ----------- L2 Normalization Function ----------------------------------------
L2norm <- function (x) {

      x_L2 <- matrix(0, nrow = nrow(x) , ncol = ncol(x))  #initialize a new matrix to store normalized values
      x_L2 <- as.data.frame(x_L2)   #coerce to data.frame class
      colnames(x_L2) <- names(x)    #copy column headings from original data set
      
      # loop through cells of the matrix, computing normalized values by column
      for (col in 1:ncol(x)) {
            r <- range(x[ ,col])
            for (j in 1:nrow(x)) {
                  xi <- x[j,col]
                  x_L2[j,col] <- (xi-r[1])/(r[2]-r[1])
            } #end of j loop
      }  #end of col loop
      
      return(as.matrix(x_L2))
      
}  # end of function
# ------------------------------------------------------------------------------


