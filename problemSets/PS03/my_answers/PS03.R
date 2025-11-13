#####################
# load libraries
# set wd
# clear global .envir
#####################

#set Wd
setwd("C:/Users/lukad/OneDrive/Documents/GitHub/StatsI_2025/problemSets/PS03/my_answers")

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
library(ggplot2)
library(stargazer)

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")

#check data
head(inc.sub)
str(inc.sub)

###################################################################
# Problem 1
###################################################################

#regress outcome var on explanatory var
reg1 <- lm(voteshare ~ difflog, data=inc.sub)
reg1
summary(reg1) # inspect results
stargazer(reg1, title = "Difference in campaign spending on incumbent vote share")

#Interpretation of regression: The intercept is 0.579031, the regression coefficient with difflog is 0.041666, representing a positive relationship between both variables, it indicates that as difference in campaign spending increases, so does the incumbent's vote share. The p-value is <2e-16, which suggests the probability of obtaining results at least as extreme as those observed, with such a small p-value, we can reject the null hypothesis that there is no relationship between difference in campaign spending an incumbent vote share.

# task 2

ggplot(data=inc.sub, aes(x = difflog, y = voteshare)) +
  geom_point(size = 2, shape = 1, color = "navy") +
  geom_smooth(method = lm, color = "red3") +
  labs(title = "Difference in campaign spending on incumbent vote share",
       x = "Difference in campaign spending",
       y = "Incumbent vote share") +
  theme_bw()

#Interpretation of Figure 1 scatter plot: reflects the findings of the previous regression, a linear, strong, positive relationship between both variables. It reflects a positive correlation between both variables.

#task 3
res1 <- resid(reg1)
res1

#### task 4 - prediction equation: ####
#Find coefficient
reg1

#y = a +bc
#y = intercept +(slope*x)
# incumb voteshare = intercept + slope*difflog
# inc.sub$voteshare = 0.579031 + 0.041666*inc.sub$difflog

###################################################################
# Problem 2
###################################################################

# task 1

#regress outcome var on explanatory var
reg2 <- lm(presvote ~ difflog, data=inc.sub)
reg2
summary(reg2) # inspect results
stargazer(reg2, title = "Difference in campaign spending on incumbent's party's presidential candidate vote share")

#Interpretation of regression: The intercept is 0.507583, the regression coefficient with difflog is 0.023837, representing a positive relationship between both variables, it indicates that as difference in campaign spending increases, so does the incumbent party's presidential candidate vote share. The p-value is <2e-16, which suggests the probability of obtaining results at least as extreme as those observed, with such a small p-value, we can reject the null hypothesis that there is no relationship between difference in campaign spending an incumbent party's presidential candidate's vote share.

# task 2

ggplot(data=inc.sub, aes(x = difflog, y = presvote)) +
  geom_point(size = 2, shape = 2, color = "deeppink4") +
  geom_smooth(method = lm, color = "blue") +
  labs(title = "Difference in campaign spending on incumbent's party's presidential candidate vote share",
       x = "Difference in campaign spending",
       y = "Incumbent's party's presidential candidate vote share") +
  theme_bw()

# Interpretation of Figure 2 scatter plot: reflects the findings of the previous regression, a positive linear relationship between both variables. It reflects a positive correlation between both variables. However, the linear relationship appears to be less strong than in Figure 1 - therefore it is not very strong but a correlation exists.

# task 3 residuals
res2 <- resid(reg2)
res2

#### task 4 prediction equation: ####
#Find coefficient

#y = a +bc
#y = intercept +(slope*x)
# incumb presidential party voteshare = intercept + slope*difflog
# inc.sub$presvote = 0.507583 + 0.023837*inc.sub$difflog

###################################################################
# Problem 3
###################################################################

#regress outcome var on explanatory var
reg3 <- lm(voteshare ~ presvote, data=inc.sub)
reg3
summary(reg3)# inspect results
stargazer(reg3, title = "Vote share of incumbent's party's presidential candidate on on incumbent's electoral success")

# task 2 - make scatter

ggplot(data=inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point(size = 2, shape = 1) +
  geom_smooth(method = lm, color = "red3") +
  labs(title = "Vote share of incumbent's party's presidential candidate on incumbent's electoral success",
       x = "Incumbent's party's presidential candidate vote share",
       y = "Incumbent vote share") +
    theme_bw()

#Interpretation of Figure 3 scatter plot: reflects the findings of the previous regression, a strong positive linear relationship between both variables. It reflects a positive correlation between both variables. The linear relationship appears to similar to that of Figure 1, however, similar to Figure 2, observations appear to cluster around the centre and skew slightly to the left.

#### task 3 - prediction equation: ####

#y = a +bc
#y = intercept +(slope*x)
# incumb voteshare = intercept + slope*pres party voteshare
# inc.sub$voteshare = 0.579031 + 0.041666*inc.sub$presvote

###################################################################
# Problem 4
###################################################################

# res1 = a, res2 = b

#task 1
#create residual dataframes
res_data <- data.frame(res1 = res1, res2 = res2)
reg4 <- lm(res1 ~ res2)
summary(reg4)
stargazer(reg4, title = "Residuals Q2 on residuals of Q1")

#Interpretation of regression: The intercept is -5.934e-18, the regression coefficient of residuals from Q2 is 2.569e-01, representing a positive relationship between both variables, it indicates that as the residuals from Q2 increase, so do those from Q1. As residuals capture the difference between the predicted values of each regression and the observed values, the differences represent confounding variables which on average further explain the values of both vote shares.The p-value is <2e-16, which suggests the probability of obtaining results at least as extreme as those observed, with such a small p-value, we can reject the null hypothesis that there is no between both sets of residuals.

#scatterplot
ggplot(data=res_data, aes(x = res2, y = res1)) +
  geom_point(size = 2, shape = 1) +
  geom_smooth(method = lm, color = "red3") +
  labs(title = "Residuals Q2 on residuals of Q1",
       x = "residuals 2",
       y = "residuals 1") +
  theme_bw()

#Interpretation of Figure 4 scatter plot: It shows a strong positive linear relationship between both residuals. It reflects a positive correlation between both residuals. Figure 4 strongly resembles Figure 3 - this may indicate a relationship between the residuals of Figure 1 and 2 and Figure 3 - i.e. the difference between the expected values and observed values of both vote shares and difference in campaign expenditure can be explained by the relationship between both vote shares - this is tested in question 5

#The formula for the prediction equation is: y = intercept +(slope*x). Therefore, using the data from the regression, the formula for this question is: Residuals 1 = -5.934e-18 + (2.569e-01*Residuals 2).

###################################################################
# Problem 5
###################################################################

# task 1

# X = voteshare
# y2 = difflog; y2 = presvote

reg5 <- lm(voteshare ~ difflog + presvote, data=inc.sub)
summary(reg5)
stargazer(reg5, title = "difflog and presvote on voteshare")

#Interpretation of regression: The intercept is 0.4486442, the regression coefficient of difflog is 0.0355431 and for presvote is 0.2568770 , representing a positive relationship between both explanatory variables on the outcome variable, it indicates that as the difference in campaign expenditure increases and vote share of incumbent's party's presidential candidate increases, so does the incumbent's vote share. However, the relationship appears much stronger between presvote and voteshare.The p-value is <2e-16 for both explanatory variables, which suggests the probability of obtaining results at least as extreme as those observed, with such a small p-value, we can reject the null hypotheses that there is no between both variables and incumbent's vote share.

# task 2

# The formula for the prediction equation is: y = intercept + (slope1*x1) + (slope2*x2). Therefore, using the data from the regression, the formula for this question is: Incumbent vote share = 0.4486442 + (0.0355431*difference in campaign spending) + (0.2568770*Incumbent's party's presidential candidate vote share).

# task 3

summary(reg4)
summary(reg5)
stargazer(reg4, reg5, title = "Results regression 4 and 5")

#coeffcient of res2 is similar to presvote - residuals give everythin not explained by first 2 regressions - so it accounts for variation outside of spending difference - accounting for both - we see how much presvote influences voteshare when accounting for difflog