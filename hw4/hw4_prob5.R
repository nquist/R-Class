# grade
# Excellent!

rm(list = ls())
library(dplyr)

# This function takes in a data frame (dataf) and an integer n
# and it returns a dataframe of n randomly selected rows from
# the dataframe.
subset_rows <- function(dataf, n){
  len <- group_size(dataf)
  rownums <- sample(seq(1:len),n, replace = T)
  return(dataf[rownums,])
}

grouped_iris <- group_by(iris, Species)
new_frame <- do(grouped_iris, subset_rows(.,10))

print("Answer for #5:")
print(new_frame)
