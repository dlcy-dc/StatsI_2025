#####################
# load libraries
# set wd
# clear global .envir
#####################
setwd("C:/Users/lukad/OneDrive/Documents/GitHub/StatsI_2025/problemSets/PS01/my_answers")

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#####
# Question 1, P1

# Find a 90% confidence interval for the average student IQ in the school.
#####

# code

  z90 <- qnorm((1 - 0.90) / 2, lower.tail = FALSE)
  class_mean <- mean(y, na.rm = TRUE)
  class_sd <- sd(y, na.rm = TRUE)
  n <- length(y)
  class_se <- class_sd / sqrt(n)
  
  # constructing the confidence interval
  lower_90 <- class_mean - (z90 * class_se)
  upper_90 <- class_mean + (z90 * class_se)
  confint90 <- c(lower_90, upper_90)

# Answer
  # This constructed 90% confidence interval demonstrates a range of 94.1 to 102.7.

#####
# Question 2, P1

# Next, the school counselor was curious whether the average student IQ in her school 
# is higher than the average IQ score (100) among all the schools in the country.
# Using the same sample, conduct the appropriate hypothesis test with α = 0.05.
#####

# code  

  # hypothesis testing - null hypothesis is that mean IQ in school is not higher than the average IQ in other schools
  # H0: class_mean <= 100 
  # H1: class_mean > 100

  # basic t-test
  t.test(y, mu = 100, alternative = 'greater')
  
  # constructing by hand
  t <- (class_mean - 100) / class_se
  pt(q = t, df = 24, lower.tail = FALSE)
  
# Answer
  
  # Results: t = -0.59574, df = 24, p-value = 0.7215
  # the p-value is not less than 0.05, hence we cannot reject H0/null hypothesis

#####################
# Problem 2
#####################

### Data explained

# Y per capita expenditure on shelters/housing assistance in state
# X1 per capita personal income in state
# X2 Number of residents per 100,000 that are ”financially insecure” in state
# X3 Number of people per thousand residing in urban areas in state
# Region 1=Northeast, 2= North Central, 3= South, 4=West

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
summary(expenditure)
head(expenditure)

#####
# Question 1, P2

# Please plot the relationships among Y, X1, X2, and X3? What are the correlations among them 
# (you just need to describe the graph and the relationships among them)?
#####

# code

library(tidyverse)
library(plotly)
library(GGally)

plot_rel <- ggpairs(expenditure, columns = 2:5)
ggplotly(plot_rel)

# Answer

  # I use ggpairs over pairs here as the plot produced is more more legible, clear and easier to read.

  # Most graphs show - to some degree - a positive association.
  # notably X1~X3 demonstrate quite a strong association, with some outliers.
  # X2~X3, X1~X2 do not clearly demonstrate a positive associations
  # while Y~X2 appears to show some positive directionality a clear gap exists which make such conclusions unclear.
  # Y~X3 shows a positive and somewhat strong relationship between both variables.
  # however it is worth noting that the correlation coficcients suggest that Y~X1, Y~X2, Y~X3 and X3~X1 show 
  # statistically significant positive coefficients, suggesting relatively strong association in such cases.


#####
# Question 2, P2

# Please plot the relationship between Y and Region? On average, 
# which region has the highest per capita expenditure on housing assistance?
#####

# code
  expenditure$Region <- factor(expenditure$Region, 
                                  levels = c(1, 2, 3, 4),
                                  labels = c("North-East", "North Central", "South", "West"))
  
  ggplot(expenditure, aes(x=Region, y=Y, fill = Region)) + 
    geom_boxplot() +
    labs(y = "p/c expenditure on shelters/housing assistance", title = "p/c expenditure on shelters/housing assistance by Region")

# Answer

  # Based off the box plot results, the West has on average (via the median) the highest per capita expenditure on housing assistance.

#####
# Question 3, P2

# Please plot the relationship between Y and X1? Describe this graph and the relationship.
# Reproduce the above graph including one more variable Region and display
# different regions with different types of symbols and colors.
#####

# code
  
  # first, simple plot with of Y~X1
  ggplot(expenditure, aes(x=X1, y=Y)) +
    geom_point() +
    geom_smooth( method=lm, se=FALSE) +
    labs(x = "p/c personal income in state", y = "p/c expenditure on housing assistance", title = "p/c expenditure on housing assistance by p/c personal income in state")
  
  # complex 
  ggplot(expenditure, aes(x=X1, y=Y, shape= Region, colour = Region)) +
    geom_point() +
    labs(x = "p/c personal income in state", y = "p/c expenditure on shelters/housing", title = "p/c expenditure on housing assistance by p/c personal income in state") +
    geom_smooth( method=lm, se=FALSE)
  
# Answer
  # The simple plot on the relationship between Y and X1 would 
  # demonstrate a relatively strong, positive and linear association between both variables.
  # Outliers are minimal, and still fit with the strong directionality bar one value.
  
  # The more complex scatter plot, inclusive of region, demonstrates regional variation 
  # in association between both variables. The North-East in particular demonstrates quite a strong association, with observations quite close to the line.
  # While the West - despite a clear positive linear association - contains the most outliers
  # and the South appears to demonstrate the weakest association - though still positive. Notably its values skew heavily to the left.
  
  