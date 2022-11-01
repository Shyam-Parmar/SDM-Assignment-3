################################
# Assignment 3


################################
# Load libraries
pacman::p_load(dplyr, ggplot2, readxl, tidyverse, mlbench, caret, magrittr, mltools, PerformanceAnalytics, Hmisc, stargazer, fpp2, car, lmtest)

df <- read_excel('C:/Users/Shyam/Downloads/HealthInsurance.xlsx')

###############################
# Check for NA values
sum(is.na(df))

# Create new column based on other columns
df <- df %>% mutate(overall_health =
                      case_when(
                        df$verygood == 1 ~ 1,
                        df$good == 1 ~ 2,
                        df$fair == 1 ~ 3,
                        df$poor == 1 ~ 4,
                        df$verygood == 0 & df$good == 0 & df$fair == 0 & df$poor == 0 ~ 5
                      ))
table(df$overall_health) # Check the column values

# Drop columns
df <- select(df, -hisp, -black, -firmlocation, -educyr, -lowincome, -midincome, -vgh, -fph, -verygood, -good, -fair, -poor, -income)

###############################
# Analysis
hist(df$medexpense)
den <- density(df$medexpense)                       # Density function
plot(den, main="Kernel Density of Sales", col="red")
hist(df$medexpense, breaks=20, prob=T, main="Histogram of Medical Expense")
lines(den, col="red")

hist(df$logincome)

str(df)

chart.Correlation(df)
df$illnesses <- as.factor(df$illnesses)
df$illnesses  <- relevel(df$illnesses, 0)              # Set baseline for factor variables

###############################
# Model Creation
m1 <- lm(medexpense ~ healthins + healthins + age + female + blackhisp + educyr +
           illnesses + ssiratio + firmsize + private + married + poverty + firmlocation +
           msa + prioritylist + overall_health, data = df)
summary(m1)


m2 <- lm(medexpense ~ healthins + healthins + agesqrd + female + blackhisp + educyr +
           illnesses + ssiratio + firmsize + private + married + poverty + income +
           msa + prioritylist + overall_health, data = df)
summary(m2)


m3 <- lm(logmedexpense+1 ~ healthins + healthins + age + female + blackhisp + educyr +
           illnesses + ssiratio + firmsize + private + married + poverty + firmlocation +
           msa + income + prioritylist + overall_health, data = df)
summary(m3)

# Stargazer
stargazer(m1, m2, m3, type="text", single.row=TRUE)

plot(m3)

############################
# Assumption test
shapiro.test(m3$res)                        # Shapiro-Wilk's test of multivariate normality
plot(m3)
ks.test(norm, m3$res)
bartlett.test(list(m3$res, m3$fit))   
dwtest(m3)
vif(m3)

table(df$illnesses)


