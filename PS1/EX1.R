setwd("C:/Users/Lorena Tassone/Desktop/UZH/Master/HS 21/Empirical Methods/Problem_Sets/PS1")
getwd()

# package installation
install.packages("stargazer")
install.packages("ggplot2")
install.packages("AER")
install.packages("car")
install.packages("descr")
install.packages("vtable")
install.packages("dplyr")

### (a) ------------------------------------------------------------------------
# importing data
library(AER)
data("TeachingRatings", package = "AER")
TeachingRatings
dim(TeachingRatings)


### (b) ------------------------------------------------------------------------
## summary of mean, sd, min and max
library(stargazer)
stargazer(as.data.frame(TeachingRatings[c("eval","beauty","age","allstudents")]),
          type = "latex", 
          title = "Table 1: Summary Statistics with Variables of Interest",
          omit.summary.stat = c("p25", "p75"))

## cross-tabulating gender and minority
# TODO: make title smaller & drop percentage
library(vtable)
sub1 <- subset(TeachingRatings, select = c("gender", "minority"))
st(sub1, group = "minority", title = "Cross-tabulation of 'gender' and 'minority' ")

## count number of missing values 
# creating subset only with needed variables
sub2 <- subset(TeachingRatings, select = c("eval", "beauty", "age", "allstudents", "gender", "minority"))

# creating matirx with number of missing values per variable
na_matrix = matrix(0, 6, 1)
rownames(na_matrix) <- colnames(sub2)
for (v in 1:ncol(sub2)){
  colname = colnames(sub2)[v]
  na_matrix[v,1] <- sum(is.na(sub2$colname)) 
}
colnames(na_matrix) <- "Missing Values"
na_matrix


### (c) ------------------------------------------------------------------------
# creating a histogram with the distribution of 'eval'
library(ggplot2)
ggplot(TeachingRatings) + geom_histogram(aes(x = eval), binwidth = 0.05) + xlim(1,5) + ggtitle("Distribution of overall course evaluation") + labs(x="Course Evaluation", y="Count")

# creating a scatterplot showing the joint distribution of 'eval' and 'beauty'
ggplot(TeachingRatings, aes(x = beauty, y = eval)) + 
  geom_point() + 
  ylim(1,5) + xlim(-2,2) +
  ggtitle("Joint distribution of course evaluation \nand professor's physical appearance") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x="Professor's physical appearance", y="Course Evaluation") + 
  geom_smooth(color = 'red', method = 'loess', formula = y~x)


### (d) ------------------------------------------------------------------------
## OLS coefficient using the formulas
# beta 2 (coefficient of beauty)
beauty <- TeachingRatings$beauty
eval <- TeachingRatings$eval
beta_2 <- cov(beauty, eval)/var(beauty)
beta_2 # 0.1330014

# beta 1 (intercept)
beta_1 <- mean(eval)-beta_2*mean(beauty)
beta_1 # 3.998272

# OLS coefficient using regression model
reg <- lm(eval~beauty, data = TeachingRatings)
stargazer(reg, type = "text", title = "Regression output")


### (e) ------------------------------------------------------------------------
## regression output without intercept
reg_nointercept <- lm(eval ~ 0 + beauty, data = TeachingRatings)
stargazer(reg_nointercept, type = "latex", title = "Regression output (without intercept)")

# scatterplot & regression line with and without intercept
ggplot(TeachingRatings, aes(x = beauty, y = eval)) + 
  ggtitle("Joint distribution of course evaluation \nand professor's physical appearance") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x="Professor's physical appearance", y="Course Evaluation") + 
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE, aes(colour = "With intercept")) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ 0 + x, se=FALSE, aes(colour = "Without intercept")) +
  scale_colour_manual(name="Legend", values=c("red", "blue"))


### (f) ------------------------------------------------------------------------
# create dummy variable for female_minority
library(dplyr)
TeachingRatings <-  mutate(TeachingRatings, 
                           female_minority = factor((gender == "female" & minority == "yes"), labels = c(0,1)))
TeachingRatings$female_minority = as.numeric(TeachingRatings$female_minority==1)

# regression
age <- TeachingRatings$age
allstudents <- TeachingRatings$allstudents
gender <- TeachingRatings$gender
minority <- TeachingRatings$minority
female_minority <- TeachingRatings$female_minority

reg2 <- lm(eval ~ beauty + age  + I(age^2) + log(allstudents) + gender + minority + female_minority, data = TeachingRatings)
stargazer(reg2, type = "latex", title = "Regression Output")

# an increase of the instructor's beauty rating by one standard deviation
# beta2
0.157

# the instructor being a non-minority male as opposed to non-minority female
# beta6
-0.184

# the course having 10% more students
(reg2)$coefficients[5]/100*10 

# 1 the instructor being 60 as opposed to 50 years old
((reg2)$coefficients[3] + 2*(reg2)$coefficients[4]*50)*10 # -0.0469647 

# Solution: -0.08625375 
((reg2)$coefficients[3]*60 + (reg2)$coefficients[4]*(60^2)) - ((reg2)$coefficients[3]*50 + (reg2)$coefficients[4]*(50^2))

### (g) ------------------------------------------------------------------------
# transforming categorical variables into numerical variables with 1 and 0 
# 1: female, minority, 0: male, non-minority
TeachingRatings$gender2 = as.numeric(TeachingRatings$gender=="female")
TeachingRatings$minority2 = as.numeric(TeachingRatings$minority=="yes")

# calculating means
meanbeauty<-mean(TeachingRatings$beauty)
meanage<-mean(TeachingRatings$age)
meanallstudents<-mean(TeachingRatings$allstudents)
meanminority<-mean(TeachingRatings$minority2)
meangender<-mean(TeachingRatings$gender2)
meanfemale_minority<-mean(TeachingRatings$female_minority)

# predicted course teaching score with means
y_pred_means <- (reg2)$coefficients[1] + (reg2)$coefficients[2]*meanbeauty +
  (reg2)$coefficients[3]*meanage + (reg2)$coefficients[4]*meanage^2 +
  (reg2)$coefficients[5]*log(meanallstudents) + (reg2)$coefficients[6]*meangender +
  (reg2)$coefficients[7]*meanminority + (reg2)$coefficients[8]*meanfemale_minority

y_pred_means # 3.996632


### (k) ------------------------------------------------------------------------
# get residuals
residuals<-residuals(reg2)

#covariance between beauty and residuals
c <- cov(beauty, residuals)
c
format(round(x = c, digits = 6), nsmall = 6)


#plot of residuals against ln(allstudents)
p <- qplot(data = ((TeachingRatings)), log(allstudents), residuals)
p1 <- p + geom_smooth(method = "lm", size = 1, col="#1F78B4") +
  ggtitle("Residuals vs. ln(allstudents)") + theme_minimal() +
  theme(text=element_text(size=16, family="Arial")) +
  labs(x="ln(allstudents)", y="Residuals") 
p1
  
#density plot for residuals compared to normal distribution
ggplot(TeachingRatings, aes(x=residuals)) + geom_density() +
  stat_function(fun = dnorm, args = list(mean = mean(residuals),
                                         sd = sd(residuals)), col="#1F78B4") +
  ggtitle("Distribution of Residuals vs. Normal Distribution") +
  theme_minimal() + theme(text=element_text(size=16, family="Arial")) +
  labs(x="Residuals", y="Count") 
  
#QQ-Plot
p <- ggplot(TeachingRatings, aes(sample=residuals))
p + stat_qq() + stat_qq_line() + theme_minimal() +
  ggtitle("QQ Plot of Residuals") +
  theme(text=element_text(size=16, family="Arial")) +
  labs(x="Theoretical Quantiles", y="Sample Quantiles")
  
#Shapiro-Wilk normality test
  shapiro.test(residuals)
  
# Breusch Pagan Test
install.library("lmtest")
library(lmtest)
  

bptest(reg2)
  

  