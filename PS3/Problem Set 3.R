setwd('C:\Users\Lorena Tassone\Desktop\UZH\Master\HS 21\Empirical Methods\Problem_Sets\PS3')
getwd()

install.packages("stargazer")
library("stargazer")
install.packages("car")
library("car")
install.packages("ivreg")
library("ivreg")
install.packages("dplyr")
library("dplyr")
install.packages("lmtest")
library("lmtest")
install.packages("sandwich")
library("sandwich")


### a)
# import data
swing_crossdata <- read.csv("EM_PS3_swingcrossdata.csv")
dim(swing_crossdata)

# summary statistics for SWING, thresh, heavysh, distmanuf, and enclosures
stargazer(as.data.frame(swing_crossdata[c("SWING","thresh","heavysh","distmanuf","enclosures")]),
          type = "latex", 
          title = "Table 1: Summary Statistics with Variables of Interest",
          omit.summary.stat = c("p25", "p75"))

### c)
## i. 
# regress SWING on thresh
reg1 <- lm(SWING ~ thresh, data = swing_crossdata)

## ii.
# regress SWING on thresh and the log of density
reg2 <- lm(SWING ~ thresh + log(density), data = swing_crossdata)

## iii.
# regress SWING on thresh and all controls that the authors include in column 1 of table 2
reg3 <- lm(SWING ~ thresh + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews), data = swing_crossdata)

# table of reg1, re2, reg3
stargazer(reg1, reg2, reg3, type = "latex", 
          title = "Regression output",
          covariate.labels = c("Threshers", "log 1801 density", "Share of agricultural workers in 1801", "log 1801 sex ratio", "log distance to Elham", "log distance to newspaper"),
          header = FALSE)

stargazer(reg1, reg2, reg3, type = "text", 
          title = "Regression output",
          header = FALSE)

### d)
# create dummies for four of the five regions in REGION
swing_crossdata <- mutate(swing_crossdata, region1 = 1*(REGION == "South-West"))
swing_crossdata <- mutate(swing_crossdata, region2 = 1*(REGION == "South-East"))
swing_crossdata <- mutate(swing_crossdata, region3 = 1*(REGION == "North-West"))
swing_crossdata <- mutate(swing_crossdata, region4 = 1*(REGION == "North-East"))



# regress SWING on thresh, all controls and dummies for region
reg4 <- lm(SWING ~ thresh + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews) + region1 + region2 + region3 + region4, data = swing_crossdata)
# table of reg4
stargazer(reg4, type = "text", title = "Regression output")

### h)
# regress instrument on thresher
reg5 <- lm(thresh ~ heavysh, data = swing_crossdata)
# table of reg5
stargazer(reg5, type = "latex", title = "Regression output")


### i)
# First stage regression for the IV estimation
FS_reg1 <- lm(thresh ~ heavysh + cer + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews), data = swing_crossdata)
FS_reg1_region <- lm(thresh ~ heavysh + cer + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews) + region1 + region2 + region3 + region4, data = swing_crossdata)

stargazer(FS_reg1, FS_reg1_region , type = "latex",
          title = "First stage of IV",
          dep.var.labels = "Threshing Machines",
          column.labels = c("Normal", "Region FE"),
          header = FALSE)

# Computing F-value
(F_stat <- (summary(FS_reg1)$coefficients[2,1]/summary(FS_reg1)$coefficients[2,2])^2)
# [1] 17.57757
(F_stat <- (summary(FS_reg1_region)$coefficients[2,1]/summary(FS_reg1)$coefficients[2,2])^2)
# [1] 15.9339


### j)
## 2SLS estimates by hand
# regressing RHS endogenous variable on instrument and all controls
TSLS_reg1 <- lm(thresh ~ heavysh + cer + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews), data = swing_crossdata)
TSLS_reg1_region <- lm(thresh ~ heavysh + cer + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews) + region1 + region2 + region3 + region4, data = swing_crossdata)

# creating fitted value of endogenous variable
thresh_fit <- fitted(TSLS_reg1)
thresh_fit_region <- fitted(TSLS_reg1_region)

# regressing SWING on fitted value and controls
TSLS_reg2 <- lm(SWING ~ thresh_fit + cer + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews), data = swing_crossdata)
TSLS_reg2_region <- lm(SWING ~ thresh_fit_region + cer + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews) + region1 + region2 + region3 + region4, data = swing_crossdata)

stargazer(TSLS_reg2, TSLS_reg2_region, type = "latex",
          title = "2SLS estimates (by hand)",
          dep.var.labels = "Number of Swing Riots",
          column.labels = c("Normal", "Region FE"),
          header = FALSE)


### k) 
library("ivreg")
library("lmtest")
library("sandwich")

## 2SLS IV regression
TSLS_reg3 <- ivreg(SWING ~ thresh + cer + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews) | .-thresh + heavysh, data = swing_crossdata)
TSLS_reg3_region <- ivreg(SWING ~ thresh + cer + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews) + region1 + region2 + region3 + region4 | .-thresh + heavysh, data = swing_crossdata)

# coeftest to compute robust std. errors
TSLS_reg3 <- coeftest(TSLS_reg3, vcov = vcovHC(TSLS_reg3, type="HC1"))
TSLS_reg3_region <- coeftest(TSLS_reg3_region, vcov = vcovHC(TSLS_reg3_region, type="HC1"))

stargazer(TSLS_reg3, TSLS_reg3_region, 
          type = "latex",
          title = "2SLS estimates (with IV command) ",
          dep.var.labels = "Number of Swing Riots",
          column.labels = c("Normal", "Region FE"),
          header = FALSE)


### l)
# reduced form regression on all coefficients
reg6 <- lm(SWING ~ heavysh + cer + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews), data = swing_crossdata)
reg6_region <- lm(SWING ~ heavysh + cer + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews) + region1 + region2 + region3 + region4, data = swing_crossdata)

reg6_c <- coeftest(reg6, vcov = vcovHC(reg6, type="HC1"))

stargazer(reg6_c, reg6_region, type = "text",
          title = "Reduced Form Regression",
          dep.var.labels = "Number of Swing Riots",
          column.labels = c("Normal", "Region FE"),
          header = FALSE)


# reduced form regression only on heavysh
reg6 <- lm(SWING ~ heavysh , data = swing_crossdata)
reg6_region <- lm(SWING ~ heavysh + region1 + region2 + region3 + region4, data = swing_crossdata)

stargazer(reg6, reg6_region, type = "latex",
          title = "Reduced Form Regression",
          dep.var.labels = "Number of Swing Riots",
          column.labels = c("Normal", "Region FE"),
          header = FALSE)

# bins version
install.packages("binsreg")
library("binsreg")

reg6_n <- binsglm(SWING, heavysh , data = swing_crossdata)
reg6_region_n <- binsglm(SWING, heavysh + region1 + region2 + region3 + region4, data = swing_crossdata)

stargazer(reg6_n, reg6_region_n, type = "text",
          title = "Reduced Form Regression",
          dep.var.labels = "Number of Swing Riots",
          column.labels = c("Normal", "Region FE"),
          header = FALSE)



### o)
## OLS regressions from figure 3
# creating subsets
(median_distmanuf = median(swing_crossdata$distmanuf))
[1] 73.52683
(median_distmanuf = round(median(swing_crossdata$distmanuf)))
[1] 74
ds_distant <- subset(swing_crossdata, distmanuf > median_distmanuf)
ds_close <- subset(swing_crossdata, distmanuf <= median_distmanuf)

(median_enclosures = swing_crossdata[swing_crossdata$PARISH == "ROMFORD", "enclosures"])
[1] 1
ds_highenclosure <- subset(swing_crossdata, enclosures > median_enclosures)
ds_lowenclosure <- subset(swing_crossdata, enclosures <= median_enclosures)

# creating regressions
reg_dist1 <- lm(SWING ~ thresh + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews), data = ds_distant)
reg_dist2 <- lm(SWING ~ thresh + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews), data = ds_close)
reg_dist1_region <- lm(SWING ~ thresh + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews) + region1 + region2 + region3 + region4, data = ds_distant)
reg_dist2_region <- lm(SWING ~ thresh + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews) + region1 + region2 + region3 + region4, data = ds_close)

reg_enc1 <- lm(SWING ~ thresh + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews), data = ds_highenclosure)
reg_enc2 <- lm(SWING ~ thresh + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews), data = ds_lowenclosure)
reg_enc1_region <- lm(SWING ~ thresh + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews) + region1 + region2 + region3 + region4, data = ds_highenclosure)
reg_enc2_region <- lm(SWING ~ thresh + log(density) + agri_share + log(sex_ratio) + log(distel) + log(distnews) + region1 + region2 + region3 + region4, data = ds_lowenclosure)

stargazer(reg_dist1, reg_dist2, reg_dist1_region, reg_dist2_region, type = "latex",
          title = "Aggravating circumstances: distance to closest industrial town",
          dep.var.labels = "Number of Riots",
          column.labels = c("Distant", "Close", "Distant (RFE)", "Close (RFE)"),
          header = FALSE)

stargazer(reg_enc1, reg_enc2, reg_enc1_region, reg_enc2_region, type = "text",
          title = " Aggravating circumstances: enclosures",
          dep.var.labels = "Number of Riots",
          column.labels = c("Distant", "Close", "Distant (RFE)", "Close (RFE)"),
          header = FALSE)


