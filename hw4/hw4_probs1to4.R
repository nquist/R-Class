# grade: 10/10
# Excellent!

rm(list = ls())

# Work for Problem 1
# This function takes in five numbers: n, mean1, mean2, sd1, and 
# sd2. It then creates two vectors by pulling n numbers from a
# normal distribution with means of mean1 and mean2 and standard
# deviations of sd1 and sd2, respectively. It then does a t.test 
# on these two vectors and returns the p-value.
ttest_sim <- function(n, mean1, mean2, sd1, sd2){
  sample1 <- rnorm(n, mean1, sd1)
  sample2 <- rnorm(n, mean2, sd2)
  result <- t.test(sample1, sample2)
  return(result$p.value)
}

print("Answer for #1:")
set.seed(100) # set seed for repeatable random samples

sim_pval <- ttest_sim(100, 10, 10, 2, 2)
print(sim_pval)

sim_pval <- ttest_sim(100, 10, 12, 2, 2)
print(sim_pval)


# Work for Problem 2
# This function takes in five numbers: n, mean1, mean2, sd1, and 
# sd2. It then creates two vectors by pulling n numbers from a
# normal distribution with means of mean1 and mean2 and standard
# deviations of sd1 and sd2, respectively. It then does a t.test 
# on these two vectors and returns the p-value. It also takes a
# dummy variable so that this function can be iterated over 
# using lapply.
ttest_sim2 <- function(dummy, n, mean1, mean2, sd1, sd2){
  sample1 <- rnorm(n, mean1, sd1)
  sample2 <- rnorm(n, mean2, sd2)
  result <- t.test(sample1, sample2)
  return(result$p.value)
}

print("Answer for #2:")
set.seed(100) # set seed for repeatable random samples

sim_pval <- ttest_sim2(1, 100, 10, 10, 2, 2)
print(sim_pval)

sim_pval <- ttest_sim2(2, 100, 10, 12, 2, 2)
print(sim_pval)


# Work for Problem 3
dummy_vals_list <- as.list(seq(1, 10000))
pvals_list <- lapply(dummy_vals_list, ttest_sim2, n = 100,
                     mean1 = 10, mean2 = 10, sd1 = 2, sd2 = 2)

pvals_vec <- unlist(pvals_list)

pdf("Hw4_answer3.pdf") # tell the interpreter we're going to plot to a pdf
hist(pvals_vec) # make a plot
dev.off() # close the pdf file

# Work for Problem 4

# When we change mean2 so that it isn't quite the same as mean1,
# the resulting distribution of p-values gets shifted toward the
# left (higher counts in the lower p-value bins of the histogram).
# This makes sense because as the mean is shifted, the two sets of
# numbers that are being compared are not as similar as they were
# before.

# This effect is the most pronounced with high n values. As the n
# value decreases, the bins of the p-value distribution become 
# more balanced. This makes sense because the n value indicates
# how many numbers are chosen from the distributions. When n is
# smaller, there are fewer numbers used to compare the two
# distributions and therefore it is more likely to get false
# negatives.