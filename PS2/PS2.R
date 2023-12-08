setwd("C:/Users/Lorena Tassone/Desktop/UZH/Master/HS 21/Empirical Methods/Problem_Sets/PS2")
getwd()

install.packages("stargazer")
library("stargazer")
install.packages("car")
library("car")



### a)
# import data
school_data <- read.csv("class_size_pset.csv")
dim(school_data)

# create dummy variable big_school
school_data$big_school <- ifelse(school_data$n_classes>2, 1, 0)

## a.i)
# regression of grammar scores on class size
reg1 <- lm(mrkgrm ~ classize, data=school_data)
stargazer(reg1, type = "latex", title = "Regression Grammar Scores on Class Size")

## a.ii)
# regression of grammar scores on class size and big_school
reg2 <- lm(mrkgrm ~ classize + big_school, data=school_data)
stargazer(reg2, type = "latex", title = "Regression Grammar Scores on Class Size and Big School")



### b)
# dropping big_school
school_data$big_school <- NULL

# regression of grammar scores on class size and pct_dis
reg3 <- lm(mrkgrm ~ classize + pct_dis, data=school_data)
stargazer(reg3, type = "latex", title = "Regression Grammar Scores on Class Size and Percentage of Disadvantaged Kids in Class (lin-lin Model)")

# regression of log of grammar scores on class size and pct_dis
school_data$ln_mrkgrm <- log(school_data$mrkgrm)
reg4 <- lm(ln_mrkgrm ~ classize + pct_dis, data=school_data)
stargazer(reg4, type = "latex", title = "Regression Grammar Scores on Class Size and Percentage of Disadvantaged Kids in Class (log-lin Model)")

mean(school_data$mrkgrm)
# [1] 72.70666



### c)
# create dummy small_size
school_data$small_size <- ifelse(school_data$classize<=10, 1, 0)

# regression of grammar scores on small_size and pct_dis
reg5 <- lm(mrkgrm ~ small_size + pct_dis, data=school_data)
stargazer(reg5, type = "latex", title = "Regression Grammar Scores on Small Size and Percentage of Disadvantaged Kids in Class")

## c.i)
# beta2 / average test score
(reg5)$coefficients[2]/mean(school_data$mrkgrm) # 0.03520556 

## 1-sided test
res <- summary(reg5)
pvalues <- pt(coef(res)[,3], reg5$df, lower=FALSE)
pvalues[2] # 0.1008177

## By hand
# t-statistic
tstatistic <- res$coefficients[2,1]/res$coefficients[2,2]
tstatistic # 1.277334
# degrees of freedom
df <- nrow(school_data)-3
# critical value from t distribution 
qt(0.95, df=df) # 1.64563

## c.ii)
# Partition for small_size
partition <- lm(small_size ~ pct_dis, data=school_data)
school_data$e1 <- resid(partition)
# Partitioned for regression
parti.reg <- lm(mrkgrm ~ e1, data=school_data)
beta2 <- round(coef(parti.reg)[2], digits=4)
beta2

## c.iii)
# compute mean of all variables
mrkgrm_mean<- mean(school_data$mrkgrm)
small_size_mean <- mean (school_data$small_size)
pct_dis_mean <- mean(school_data$pct_dis)

# insert betas of independent variables
beta_smallsize <- coef(reg5)[2]
beta_pctdis <- coef(reg5)[3]

# Compute beta_1 with  given formula
beta_1 <- mrkgrm_mean - small_size_mean * beta_smallsize - pct_dis_mean * beta_pctdis
beta_1

## c.iv)
# Regression ln_mrkgrm on small_size and pct_dis
reg6 <- lm(ln_mrkgrm ~ small_size + pct_dis, data = school_data)
stargazer(reg6, type = "latex", title = "Regression of Log of Grammar Scores on Small Size and Percentage of Disadvantaged Kids in Class")

# interpretation of small_size
(exp(coef(reg6)[2])-1)*100 # 3.72102



### d)
# creating dummy many_dis
school_data$many_dis <- ifelse(school_data$pct_dis>10, 1, 0)

# Regression model with the interaction term
reg7 <- lm(mrkgrm ~ classize + many_dis + many_dis*classize, data = school_data)
stargazer(reg7, type = "latex", title = "Regression Grammar Scores on Class Size and Many Disadvantaged Pupils in Class")

## d.i)
# Joint hypothesis testing
lh <- linearHypothesis(reg7, c("many_dis=0", "classize:many_dis=0"))
lh
F_stat <- lh$F[2] # 401.8476
F_stat


# Define the regressions (restricted and unrestricted)
restricted <- lm(mrkgrm ~ classize, data = school_data)
unrestricted <- lm(mrkgrm ~ classize + many_dis + many_dis*classize, data = school_data)

# Compute F-statistic manually
r2_restricted <- summary(restricted)$r.squared
r2_unrestricted <- summary(unrestricted)$r.squared
df <- summary(unrestricted)$df[2]
q <- 2
f_statistic <- ((r2_unrestricted-r2_restricted)/q) / ((1-r2_unrestricted)/df)
f_statistic # 401.8476



## e)


