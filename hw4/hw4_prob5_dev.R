
rm(list = ls())
library(dplyr)

# This function takes in a data frame (dataf) and an integer n
# and it returns a dataframe of n randomly selected rows from
# the dataframe.
subset_rows <- function(dataf, n){
  set.seed(100)
  len <- group_size(dataf)
  rownums <- sample(seq(1:len),n, replace = T)
  print(rownums)
  return(dataf[rownums,])
}

grouped_iris <- group_by(iris, Species)
print(group_size(grouped_iris))
new_frame <- do(grouped_iris, subset_rows(.,10))

#Use group_by() to group the iris data frame by the Species column,
#and then do() along with your subset_rows() function to generate 
#a data frame consisting of 10 random rows per species.