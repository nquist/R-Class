
rm(list = ls())
library(stringr) # import the stringr library to use in problem 6

# Import the states text file
states <- read.table(file = "~/hw3/states.txt", header = T, sep = '\t', 
                     stringsAsFactors = F, comment.char = "#")

# Work for Problem #2
median_income <- median(states$income)
median_grad <- median(states$hs_grad)
states_gradincome_high <- subset(states, income > median_income | hs_grad > median_grad)
states_gradincome_high <- states[states$income > median_income | states$hs_grad > median_grad,]

print("Answer for #3:")
print(states_gradincome_high)

# Work for Problem #3
states_gradincome_low <- states[!(states$name %in% states_gradincome_high$name),]
print("Answer for #3:")
print(states_gradincome_low)

# Work for Problem #4
test <- factor(states$region, levels = c("North Central", "Northeast", "South", "West"))
print(test)
#ordered(states$region, levels = c("North Central", "Northeast", "South", "West"))
states$region <- test
print("Answer for #4:")
print(states$region)

# Work for Problem #5
annotations <- read.table("~/hw3/PZ.annot.txt", sep = "\t", stringsAsFactors = F)
colnames(annotations) <- c("ID", "GO number", "descriptive name")
annotations_with_suffix <- annotations[grep("_", annotations$ID),]
annotations_without_suffix <- annotations[!(annotations$ID %in% annotations_with_suffix$ID),]

print("Answer for #5:")
print(head(annotations_with_suffix))
print(nrow(annotations_with_suffix))

print(head(annotations_without_suffix))
print(nrow(annotations_without_suffix))

# Work for Problem 6
suffix_split <- as.data.frame(str_split_fixed(annotations_with_suffix$ID, "_", 2))
colnames(suffix_split) <- c("base_id", "suffix")
annotations_with_suffix <- cbind(annotations_with_suffix, suffix_split)
print("Answer for #6:")
print(head(annotations_with_suffix, n=10))
