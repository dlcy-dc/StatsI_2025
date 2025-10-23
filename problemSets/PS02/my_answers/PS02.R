#####################
# PS02
# Luka De Lacey
# 14-10-2025
#####################

rm(list=ls())

setwd("C:/Users/lukad/OneDrive/Documents/GitHub/StatsI_2025/problemSets/PS02/my_answers")

#Task 1 - create matrix

df <- matrix(NA, nrow = 2, ncol = 3)
rownames(df) <- c("upper class", "lower class")
colnames(df) <- c("not stopped", "bribe requested", "stopped/given warning")
df[1, ] <- c(14, 6, 7)
df[2, ] <- c(7, 7, 1)

# Find expected values - first calculate needed values for formulas
colsum <- colSums(x = df)
rowsum <- rowSums(x = df)
total <- sum(df)

#expected values matrix creation
dfe <- outer(rowsum, colsum)/total

#Chi_2 by hand
chi_2 <- sum((df - dfe)^2/dfe)
chi_2

#Chi_2 via command
chi123 <- chisq.test(df)

#chi squared is 3.791 in both cases

# Task 2 - p values

# find Degrees of freedom
dfr <- (nrow(df)-1)*(ncol(df)-1)

pchisq(chi_2, dfr, lower.tail = FALSE) 
#H0 - variables are statistically independent

# p value is 0.1502306 - larger than 0.1 so we do not reject it - we cannot prove they are not statically independent therefore we assume they are independent.

# Task 3 - Standardised residuals - available in Chi_squared

chi123$stdres

# Task 4 - Standardised residuals - what do they show?
# how good the fit of the model is - identifies relevant outliers - identifies cells which contribute the most to Chi-Squared test statistic. 

#####
# Problem 2
####

# Task 1
# Null Hypothesis - If a village has been reserved for a female council head, the will be no difference in the number of new or repaired drinking water facilities than non-reserved villages.
# Alternative Hypothesis - If a village has been reserved for a female council head, the number of new or repaired drinking water facilities will be higher than non-reserved villages.

data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
head(data)

# Task 2
reg <- lm(data$water~data$reserved)
summary(reg)

# Task 3
# Cor coefficient of 9.252 suggests that alternative hypothesis has validity - the null hypothesis is rejected due to p value less than 0.05
