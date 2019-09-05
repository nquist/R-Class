# grade: 10/10
# Good job. For part 6, try to use $ operator. We can find p-value of t.test by using test$p.value.

rm(list = ls())

set.seed(10) # set the random seed to something for reproducable randomness

# a random sample of 1000 numbers
nums <- rexp(1000, rate = 4)

# In the following line, the function seq creates a vector that contains integers 
# from 1 to 1000 in ascending order. This vector is then passed to sample, which 
# "randomly" (not really random because we chose the seed) choses 50 of those numbers
# which are stored in a new vector called random_indicies. This vector is used as 
# the indexing vector for the next line. Each element in the random_indices vector
# is used as the index in nums and the value of that index is changed to NA.
random_indices <- sample(seq(1,1000), 50)
nums[random_indices] <- NA 

# Work for problem #2. I found the indexes that weren't NA, then created
# a new vector without the NA's. Used that vector to find the mean, and 
# then used length and logical opperator to find the number of values 
# lower than the mean.
not_na_indexes <- !is.na(nums)
no_na_nums <- nums[not_na_indexes]
nums_mean <- mean(no_na_nums)
lower_mean_number <- length(no_na_nums[no_na_nums < nums_mean])

print("Answer for #2:")
print(lower_mean_number)

# Work for problem #3. Same process as number #2, but use median instead
# of mean and use > instead of <.
nums_median <- median(no_na_nums)
upper_mean_number <- length(no_na_nums[no_na_nums > nums_median])

print("Answer for #3:")
print(upper_mean_number)

# Work for problem #4. Similar to number 3 and 2, but use the AND
# operator to find the subsection that falls into both categories
between_mean_and_median <- length(no_na_nums[(no_na_nums > nums_median) & (no_na_nums < nums_mean)])

print("Answer for #4:")
print(between_mean_and_median)

# Work for problem #5. This function takes a single numeric vector and 
# finds the coefficient of variation, which is the standard deviation
# divided by the mean. It returns a single numeric. 
coeff_of_var <- function(numbs){
  result <- sd(numbs, na.rm = T)/mean(numbs, na.rm = T)
  return(result)
}

print("Answer for #5:")
print(coeff_of_var(nums))


# Work for problem #6. This is a function that takes two numeric vectors
# (nums1, nums2) and does a t-test on them, and then returns the p-value
# of the two functions, as a single numeric.
ttest_pval <- function(nums1, nums2){
  test <- t.test(nums1, nums2)
  return(test[[3]])
}

samp1 <- rnorm(100, mean = 4.5, sd = 2)
samp2 <- rnorm(100, mean = 5, sd  = 2)
print("Answer for #6:")
print(ttest_pval(samp1, samp2))
