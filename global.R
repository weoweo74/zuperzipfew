library(dplyr)
library(zipcode)
library(RCurl)
library(zipcode)
library(ggthemes)

x <- getURL("https://raw.githubusercontent.com/weoweo74/zuperzipfew/master/superzipV2.csv")
allzips <- read.csv(text = x)


#allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
#allzips$treatment <- allzips$treatment * 100


allzips$zip <- clean.zipcodes(allzips$HD2013.ZIP.code)


row.names(allzips) <- allzips$unitid
#allzips$state.x <- allzips$state.x
#allzips$state.x <- ifelse(is.na(allzips$state.x) & allzips$city.x=='cross_river', 'cross_river', as.character(allzips$state.x))  # make DC a state so it is not missing from analysis

# transform variables 
allzips$l_enorllment <- log(allzips$DRVEF122013.12.month.full.time.equivalent.enrollment..2012.13 + 1)
allzips$ID <- allzips$zipcode

allzips$city.x <- as.character(allzips$city.x)

cleantable <- allzips %>%
  select(
    hospital = hospital.name, 
    City = city.x,
    State = state.x,
    Zipcode = zip,
    #Rank = rank,
    HospitalExpenses = centile,
    #Superzip = superzip,
    Enrollment = adultpop,
    Selectivity = treatment,
    Patients = income,
    Lat = latitude,
    Long = longitude, 
    ID = zipcode
  )


# handle missing data 
allzips$treatment <- ifelse(is.na(allzips$treatment), 101, allzips$treatment)  # missing admit rates to 101 so they work with adjustable selectivity 
allzips[is.na(allzips)] <- 0  # all other missing values to 0 
allzips <- na.omit(allzips) # rows that are still missing values are dropped 




# temporarily remove treatments with missing data 
# cleantable <- na.omit(cleantable)

