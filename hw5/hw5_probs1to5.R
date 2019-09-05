# grade:10/10
# Excellent!
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)

# Work for Problem 1:
CO2_wide <- spread(CO2, conc, uptake)
print("Answer for #1")
print(head(CO2_wide))


# Work for Problem 2:
CO2_recreated <- gather(CO2_wide, conc, uptake, -Plant, -Type, -Treatment)
print("Answer for #2")
print(head(CO2_recreated))


# Work for Problem 3:
# this function takes a dataframe and finds the mean for
# the values of a specific colum and returns that as a
# data frame.
do_mean <- function(df){
  ret_df <- data.frame("mean" = mean(df$uptake))
  return(ret_df)
}
grouped_co2 <- group_by(CO2, Type, Treatment, conc)
mean_uptake <- spread(do(grouped_co2, do_mean(.)), Treatment, mean)
colnames(mean_uptake)[3] <- "nonchilled_mean_uptake"
colnames(mean_uptake)[4] <- "chilled_mean_uptake"

print('Answer to #3')
print(head(mean_uptake))

# Work for Problem 4
strms <- storms
strms$latitude_bin <- cut(strms$lat, 10)

strms_groups <- group_by(strms, latitude_bin)
# This function takes and expression and a data frame and
# and it applies lm and anova to find a relationship
# between variables as specified by the expression and
# it returns a data frame with the number of unique storms,
# p value and coefficient of the fit.
do_lm <- function(expression, dt){
  lms <- lm(expression, dt)
  nov <- anova(lms)
  vals <- data.frame("year_pval" = nov$"Pr(>F)"[1], 
                     "year_coefficient" = lms$coefficients[2], 
                     "n_storms" = length(unique(dt$name)))
  return(vals)
}

lm_strms <- do(strms_groups, do_lm(wind ~ year, .))

print("Answer for #4:")
print(head(lm_strms))

# From the fits, the data has the highest coefficient for the 
# trend would indicate the largest changes. The latitude with
# that has the biggest coefficient and therefore the most
# effected is the (20.6, 25.1], which is the region of the 
# tropics of cancer and capricorn.

# Work for Problem 5
plt <- ggplot(lm_strms, aes(latitude_bin, year_coefficient)) +
  geom_point() +
  theme_classic()
ggsave("storms_vis.pdf", plt)

# It's not an exciting plot, but I plotted the data.