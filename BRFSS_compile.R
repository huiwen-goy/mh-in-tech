
# Prep
#####
# setwd if needed

# Dealing with FIPS codes
# FIPS Source: https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696
codes <- read.csv("fips.csv", header=TRUE)

# Functions to translate state names into FIPS codes; input example: "Alaska, Idaho, Ohio"
get_fips <- function(states) {  
  states2 <- unlist(strsplit(states, split = ", "))
  fips_list <- c(length(states2))
    for (i in 1:length(states2)) {
      fips_list[i] <- codes[codes$state == states2[i], 3]
    }
  print(cbind(states2, fips_list))
  print(paste(fips_list, collapse = ", "))
}

# Function to translate FIPS codes into states; input: "2, 3, 4, 5"
get_states <- function(fips) {
  fips2 <- unlist(strsplit(fips, split = ", "))
  states_list <- c(length(fips2))
    for (i in 1:length(fips2)) {
      states_list[i] <- codes[codes$fips == as.integer(fips2[i]), 1]
    }
  print(cbind(fips2, states_list))
}

# Libraries
library(Hmisc)
library(psych)

# list of variable names
vars <- c(
'state', 
'idate', 
'dispcode',
'ageg5yr',
'age65yr',
'age.g',
'sex',
'race',
'physhlth',
'phys14d',
'menthlth',
'ment14d',
'educag',
'incomg',
'income2',
'employ1',
'hcvu651',
'hlthcvr1',
'llcpwt',
'ststr',
'psu')

#####


# 2020 Core
#####
# Source: https://www.cdc.gov/brfss/annual_data/annual_2020.html
# 401958 rows, 279 columns
c2020 <- sasxport.get("LLCP2020.XPT")

#remove .x in column names, introduced by _ in original
colnames(c2020) <- gsub("^x.", "", names(c2020))
#write.table(names(c2020), '~/Desktop/c2020.txt')

# Extract subset of variables
c2020sub <- c2020[, vars]
rm(c2020)

# Add new weight column
c2020sub$finalwt <- 0.1832094 * c2020sub$llcpwt

#####


# 2019 Core
#####
# Source: https://www.cdc.gov/brfss/annual_data/annual_2019.html
# 418268 rows, 342 columns
c2019 <- sasxport.get("LLCP2019.XPT")

#remove .x in column names, introduced by _ in original
colnames(c2019) <- gsub("^x.", "", names(c2019))
#write.table(names(c2019), '~/Desktop/c2019.txt')

# Extract subset of variables
c2019sub <- c2019[, vars]
rm(c2019)

# Add new weight column
c2019sub$finalwt <- 0.1906434 * c2019sub$llcpwt

#####


# 2018 Core
#####
# Source:
# 437436 rows, 275 columns
c2018 <- sasxport.get("LLCP2018.XPT")

#remove .x in column names, introduced by _ in original
colnames(c2018) <- gsub("^x.", "", names(c2018))
#write.table(names(c2018), '~/Desktop/c2018.txt')

# Extract subset of variables
# Note that 'sex' is 'sex1' (sex at birth, no calculated variable)
which(! vars %in% names(c2018))

c2018sub <- c2018[, c(
'state', 
'idate', 
'dispcode',
'ageg5yr',
'age65yr',
'age.g',
'sex1',
'race',
'physhlth',
'phys14d',
'menthlth',
'ment14d',
'educag',
'incomg',
'income2',
'employ1',
'hcvu651',
'hlthcvr1',
'llcpwt',
'ststr',
'psu')]

# Rename sex1 to sex
colnames(c2018sub)[colnames(c2018sub) == 'sex1'] <- "sex"
rm(c2018)

# Add new weight column
c2018sub$finalwt <- 0.19938 * c2018sub$llcpwt

#####


# 2017 Core
#####
# Source:
# 450016 rows, 358 columns
c2017 <- sasxport.get("LLCP2017.XPT")

#remove .x in column names, introduced by _ in original
colnames(c2017) <- gsub("^x.", "", names(c2017))
#write.table(names(c2017), '~/Desktop/c2017.txt')

# Extract subset of variables
c2017sub <- c2017[, vars]
rm(c2017)

# Add new weight column
c2017sub$finalwt <- 0.2051139 * c2017sub$llcpwt

#####


# 2016 Core
#####
# Source:
# 486303 rows, 275 columns
c2016 <- sasxport.get("LLCP2016.XPT")

#remove .x in column names, introduced by _ in original
colnames(c2016) <- gsub("^x.", "", names(c2016))
#write.table(names(c2016), '~/Desktop/c2016.txt')

# Extract subset of variables
c2016sub <- c2016[, vars]
rm(c2016)

# Add new weight column
c2016sub$finalwt <- 0.2216532 * c2016sub$llcpwt

#####


# Calculate proportions of each year's data, to adjust weights
#####
totalrows <- nrow(c2020sub) + nrow(c2019sub) + nrow(c2018sub) + nrow(c2017sub) + nrow(c2016sub) #2193981
nrow(c2020sub) / totalrows #0.1832094
nrow(c2019sub) / totalrows #0.1906434
nrow(c2018sub) / totalrows #0.19938
nrow(c2017sub) / totalrows #0.2051139
nrow(c2016sub) / totalrows #0.2216532
#####


# Stack and output 5-year dataset
#####
# Add a year variable to each year's data
c2016sub$year <- 2016
c2017sub$year <- 2017
c2018sub$year <- 2018
c2019sub$year <- 2019
c2020sub$year <- 2020

# Merge
c5 <- data.frame(rbind(c2016sub, c2017sub, c2018sub, c2019sub, c2020sub))
write.csv(c5, '~/Desktop/brfss_5y.csv')
str(c5)

rm(list = c('c2016sub', 'c2017sub', 'c2018sub', 'c2019sub', 'c2020sub'))

c5b <- read.csv('brfss_5y.csv', header=T, na.strings=c("","NA"))
c5b <- c5b[, -1]
str(c5b)

#####

