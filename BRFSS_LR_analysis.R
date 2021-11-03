
# Read in output csv with 5 years' data (half a minute)
c5 <- read.csv('brfss_5y.csv', 
               header=T, na.strings=c(""," ", "NA"))

# Recode for stats, binary, with NA in place of Refused/Missing
# Original is all integers

# Poor mental health days (1 or more in last 30d = 1)
c5$ment14d_n <- NA
c5$ment14d_n[c5$ment14d == 1] <- 0
c5$ment14d_n[c5$ment14d %in% c(2,3)] <- 1
table(c5$ment14d, c5$ment14d_n, useNA='a')

# Sex (female = 1)
c5$sex_n <- NA
c5$sex_n[c5$sex == 1] <- 0
c5$sex_n[c5$sex == 2] <- 1
table(c5$sex, c5$sex_n, useNA='a')

# Age (younger=1)
c5$ageg_n <- NA
c5$ageg_n[c5$age.g %in% c(1, 2, 3)] <- 1
c5$ageg_n[c5$age.g %in% c(4, 5, 6)] <- 0
table(c5$age.g, c5$ageg_n, useNA='a')

# Race (white = 1)
c5$race_n <- NA
c5$race_n[c5$race == 1] <- 1
c5$race_n[c5$race %in% c(2:8)] <- 0
table(c5$race, c5$race_n, useNA='a')

# Education (college = 1)
c5$educag_n <- NA
c5$educag_n[c5$educag %in% c(1:3)] <- 0
c5$educag_n[c5$educag == 4] <- 1
table(c5$educag, c5$educag_n, useNA='a')

# Income ($75k or more = 1)
# Median household income in 2020 = 67,521
#https://www.census.gov/library/publications/2021/demo/p60-273.html
c5$income2_n <- NA
c5$income2_n[c5$income2 %in% c(1:7)] <- 0
c5$income2_n[c5$income2 == 8] <- 1
table(c5$income2, c5$income2_n, useNA='a')

# Employment status (employed/student/homemaker/retired = 1)
c5$employ1_n <- NA
c5$employ1_n[c5$employ1 %in% c(1,2,5,6,7)] <- 1
c5$employ1_n[c5$employ1 %in% c(3,4,8)] <- 0
table(c5$employ1, c5$employ1_n, useNA='a')

# Delete all variables except recoded and survey variables
c5a <- c5[! names(c5) %in% c("X", "state", "idate", "dispcode", "ageg5yr", "age65yr", "age.g",
                            "sex", "race", "physhlth", "phys14d", "menthlth", "ment14d", "educag",
                            "incomg", "income2", "employ1", "hcvu651", "hlthcvr1", "llcpwt")]

# Get rid of any row with NA; remaining = 1742245
cases <- complete.cases(c5a)
c5b <- c5a[cases, ]


# Logistic regression: None vs. some days ~ demographic

# create survey design
library(survey)
bdesign_c5b <- svydesign(id = ~psu, strata = ~ststr, nest = TRUE, survey.lonely.psu = "adjust", 
                         weights = ~finalwt, data = c5b)

# logistic regression, taking into account survey design
# beware this takes almost 2 hours, as does producing a summary
m3 <- svyglm(ment14d_n ~ ageg_n + sex_n + race_n + educag_n + income2_n + employ1_n, 
             data=c5b, design=bdesign_c5b, family=quasibinomial())

exp(cbind(OR = coef(m3), confint(m3)))

# Proportions of each predictor
svytable(~ageg_n, bdesign_c5b, Ntotal=1.0)
svytable(~sex_n, bdesign_c5b, Ntotal=1.0)
svytable(~race_n, bdesign_c5b, Ntotal=1.0)
svytable(~educag_n, bdesign_c5b, Ntotal=1.0)
svytable(~income2_n, bdesign_c5b, Ntotal=1.0)
svytable(~employ1_n, bdesign_c5b, Ntotal=1.0)


# Logistic regression: None vs. some days ~ year + sex

# Extract new subset of variables
c5 <- c5[, c("ststr", "psu", "finalwt", "year", "sex", "ment14d")]

# Center year, with 2018 as 0
c5$year_cent[c5$year == 2016] <- -2
c5$year_cent[c5$year == 2017] <- -1
c5$year_cent[c5$year == 2018] <- 0
c5$year_cent[c5$year == 2019] <- 1
c5$year_cent[c5$year == 2020] <- 2
table(c5$year, c5$year_cent)

# Any days = 1
c5$ment14d_n <- NA
c5$ment14d_n[c5$ment14d == 1] <- 0
c5$ment14d_n[c5$ment14d %in% c(2,3)] <- 1
table(c5$ment14d, c5$ment14d_n, useNA='a')

# Sex (female = 1)
c5$sex_n <- NA
c5$sex_n[c5$sex == 1] <- 0
c5$sex_n[c5$sex == 2] <- 1
table(c5$sex, c5$sex_n, useNA='a')

# Exclude missing data; n=2153744 remaining
c5b <- c5[complete.cases(c5) == TRUE, ]

# new survey design
options(survey.lonely.psu = "adjust")
bdesign <- svydesign(id = ~psu, strata = ~ststr, nest = TRUE, weights = ~finalwt, data = c5b)

# logistic regression
m4 <- svyglm(ment14d_n ~ year_cent*sex_n, data=c5b, design=bdesign, family=quasibinomial())
exp(cbind(OR = coef(m4), confint(m4)))


# check model performance with just year and sex
library(caret)
pred <- predict.glm(m4, newdata = c5b[, c('year_cent', 'sex_n')], type = 'response')
pred.binary <- ifelse(pred > 0.3618995, 1, 0)
confusionMatrix(data = as.factor(pred.binary), reference = as.factor(c5b$ment14d_n), positive = c("1"))

