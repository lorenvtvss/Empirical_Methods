setwd("C:\Users\Lorena Tassone\Desktop\UZH\Master\HS 21\Empirical Methods\Problem_Sets\PS4")
getwd()

mortality_temp <- read.csv("mortality_temp.csv", header=TRUE)
dim(mortality_temp)
library(stargazer)

# NOTE: throughout the whole exercise, bin_7 will be excluded, since it's the bin with 60-69 F, which is therefore our constant

### a)
# Pooled OLS regression of log mortality on the temperature bins
reg1 <- lm(lndrate ~ bin_1 + bin_2 + bin_3 + bin_4 + bin_5 + bin_6 + bin_8 + bin_9 + bin_10, data = mortality_temp)
# Stargazer table:
stargazer(reg1, type = "text",
          title = "Pooled OLS",
          dep.var.labels = "Log Mortality Rate",
          header = FALSE, single.row = TRUE)

### c)
# Estimated model with state FE and month FE
reg2 <- lm(lndrate ~ bin_1 + bin_2 + bin_3 + bin_4 + bin_5 + bin_6 + bin_8 + bin_9
           + bin_10 + factor(month) + factor(stfips), data = mortality_temp)
# Stargazer table:
stargazer(reg2, type = "text",
          title = "State and Month FE Model",
          dep.var.labels = "Log Mortality Rate",
          omit = c("stfips", "month"),
          omit.labels = c("State FE", "Month FE"),
          header = FALSE, single.row = TRUE)

### d)
# Estimated model with state-by-month FE
reg3 <- lm(lndrate ~ bin_1 + bin_2 + bin_3 + bin_4 + bin_5 + bin_6 + bin_8 + bin_9
           + bin_10 + factor(month) + factor(stfips) + factor(month)*factor(stfips), data = mortality_temp)
# Stargazer table:
stargazer(reg3, type = "text",
          title = "State by Month FE Model",
          dep.var.labels = "Log Mortality Rate",
          omit = c("stfips", "month", ":"),
          omit.labels = c("State FE", "Month FE", "State$*$Month FE"),
          header = FALSE, single.row = TRUE)

### f)
install.packages("ggplot2")
library("ggplot2")
# Generate the average number of days in hottest temperature bin (bin_10) per year 
bin_10_avg = aggregate(x = mortality_temp$bin_10, by = list(mortality_temp$year), FUN = mean)
plot_year = aggregate(x = mortality_temp$year, by = list(mortality_temp$year), FUN = mean)

# Plot 1
plot1 <- ggplot(bin_10_avg, aes(x = Group.1, y = x)) +
  geom_line() +
  ggtitle("Evolution of Average Number of Hottest Temperature per Year over Time") +
  labs(x = "Year", y = "Average days in hottest bin per year") + theme_bw()
plot1 + stat_smooth(color = "blue", method = "loess") 

# Generate average log mortality per year
avg_lndrate =aggregate(x = mortality_temp$lndrate, by = list(mortality_temp$year), FUN = mean)

# Plot 2
plot2 <- ggplot(avg_lndrate, aes(x = Group.1, y = x)) +
  geom_line() +
  ggtitle("Evolution of Average Number of log Mortality per Year over Time") +
  labs(x = "Year", y = "Average log mortality rate per year") + theme_bw()
plot2 + stat_smooth(color = "blue", method = "loess")


### g)
# Adding the two dummies for unusual precipitations and quadratic time trend to previous model (reg3)
reg4 <- lm(lndrate ~ bin_1 + bin_2 + bin_3 + bin_4 + bin_5 + bin_6 + bin_8 + bin_9
           + bin_10 + factor(month) + factor(stfips) + factor(month)*factor(stfips)
           + year + I(year^2) + devp25 + devp75, data = mortality_temp)
# Stargazer table:
stargazer(reg4, type = "latex",
          title = "State by Month FE Model (g)",
          dep.var.labels = "Log Mortality Rate",
          omit = c("stfips", "month", ":"),
          omit.labels = c("State FE", "Month FE", "State$*$Month FE"),
          header = FALSE, single.row = TRUE)

### h)
# Model from (g) with mortality due to motor-vehicle accidents as dependent variable
reg5 <- lm(lndrate_mva ~ bin_1 + bin_2 + bin_3 + bin_4 + bin_5 + bin_6 + bin_8 + bin_9
           + bin_10 + factor(month) + factor(stfips) + factor(month)*factor(stfips)
           + year + I(year^2) + devp25 + devp75, data = mortality_temp)
# Model from (g) with mortality due to cardiovascular diseases as dependent variable
reg6 <- lm(lndrate_cvd ~ bin_1 + bin_2 + bin_3 + bin_4 + bin_5 + bin_6 + bin_8 + bin_9
           + bin_10 + factor(month) + factor(stfips) + factor(month)*factor(stfips)
           + year + I(year^2) + devp25 + devp75, data = mortality_temp)
# Stargazer table:
stargazer(reg5, reg6, type = "latex",
          title = "State by Month FE Model (g)",
          dep.var.labels = c("lndrate-mva", "lndrate-cvd"),
          omit = c("stfips", "month", ":"),
          omit.labels = c("State FE", "Month FE", "State$*$Month FE"),
          header = FALSE, single.row = TRUE)

### i)
# beta of bin_10 from g)
(beta_10 <- coef(reg4)['bin_10'])
# std. deviation of lndrate and bin_10
(sd_m <- sd(mortality_temp$lndrate))
(sd_bin10 <- sd(mortality_temp$bin_10))
# % increase mortality
(mag_pct <- beta_10*sd_bin10*100)
# std. deviation increase of mortality rate
(mag_sd <- beta_10*sd_bin10/sd_m)










