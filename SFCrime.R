#  Comments by Ruth Martin -----------------------------------------------------
# The code that follows was part of my final project for CIS 571 ("Data Mining 
# and Business Analytics") at Colorado State University and was inspired by 
# Kaggle's San Francisco Crime Data competition.  As I began this project after
# that competition had ended, I opted to download my data directly from the 
# original source (https://datasf.org/opendata/) so I could have access to a 
# wider range of variables and gain experience cleaning data before analysis. I 
# also diverged from the original intention of the Kaggle competition, which 
# asked participants to predict the category of crime, and instead I chose to 
# predict whether the police took action in relation to a specific incident.
#
# In addition, I opted to incorporate data from two additional sources: 
# precipitation rates for San Francisco from NOAA, and list of federal holidays 
# from the U.S. Office of Personnel Management (www.opm.gov). I chose to 
# include these additional sources partly out of curiosity about their potential
# effects, but primarily to gain experience cleaning data and merging data from
# multiple sources into a single dataset.
#
# My primary objectives for the project were to (1) explore the data, (2) practice
# using decision trees and logistic regression, and (3) gain experience using R
# for extraction, analysis, and visualization. 
#
# Ruth Martin (GitHub: https://github.com/historiscope)

# File Descriptions ------------------------------------------------------------
#
# San Francisco Crime Data for 2003 through 2016 downloaded from San Francisco
# Open Data (https://datasf.org/opendata/) on April 27, 2017 (n = 2,055,630; 
#  split evenly between test and validation data)
# 
# Precipitation amounts for San Francisco from 2003 through 2016 downloaded from 
# Climate Data Online (NOAA) (https://www.ncdc.noaa.gov/cdo-web/) on April 30,
# 2017
#
# Federal holiday schedule from 2003 through 2016, downloaded from www.opm.gov
# on April 27, 2017


# Library statements ----------------------------------------------------------
library(dplyr)
library(readr) # import data
library(ggplot2) # create plots
library(lubridate) # format dates
library(tidyr) # gather data
library(geosphere) # calculate distance between coordinates

# Other settings ---------------------------------------------------------------
options(scipen=999) #suppress scientific notation for charts



# Import San Francisco crime data ----------------------------------------------

dfCrime <- read_csv("sfcrime.csv", 
                    col_types = "cccccccccdd__")

# Combine date and time and convert to POSIX using Lubridate (original data uses 
#  San Francisco local time, including DST)
dfCrime$DateTime <- mdy_hm(paste(dfCrime$Date,dfCrime$Time)) 

# Reorder columns, drop original "Date" and "Time" chr columns, and rename columns
dfCrime <- dfCrime %>% 
  subset(select = c("IncidntNum", "DayOfWeek", "DateTime", "Category", "Descript", 
                    "Resolution", "X", "Y", "PdDistrict")) %>%
  rename(IncNum = IncidntNum)

# Getting rid of unhelpful data on Category and Resolution
# "Located" corresponds primarily to missing persons and things, "psychopath" to 
# non-criminal events, and "other offenses" primarily to vehicular and other 
# violations
ExcludedCat <-   c("NON-CRIMINAL", 
                   "OTHER OFFENSES", 
                   "WARRANTS", 
                   "SUSPICIOUS OCC", 
                   "MISSING PERSON", 
                   "RECOVERED VEHICLE", 
                   "RUNAWAY", 
                   "FAMILY OFFENSES", 
                   "SUICIDE", 
                   "GAMBLING", 
                   "PORNOGRAPHY/OBSCENE MAT",
                   "SECONDARY CODES",
                   "DISORDERLY CONDUCT",
                   "BRIBERY",
                   "EXTORTION",
                   "SEX OFFENSES, NON FORCIBLE",
                   "TREA",
                   "LOITERING") # noncriminal categories
  
ExcludedRes <- c("LOCATED", "PSYCHOPATHIC CASE") # noncriminal resolutions

Excluded <- dfCrime$Resolution %in% ExcludedRes | 
                (dfCrime$Category %in% ExcludedCat & 
                 dfCrime$Category != "DOMESTIC VIOLENCE")

dfCrime <- dfCrime[!Excluded, ]
remove(Excluded, ExcludedCat, ExcludedRes) # clean up global environment

# Use description to reclassify "Domestic Violence" as a crime with its own 
# category of "Domestic Violence"
dfCrime$Category <- ifelse(dfCrime$Descript == "DOMESTIC VIOLENCE", 
                           "DOMESTIC VIOLENCE", dfCrime$Category)


# Collapse Category values

DrugAlcohol <- c("DRUG/NARCOTIC",
              "DRUNKENNESS",
              "DRIVING UNDER THE INFLUENCE", 
              "LIQUOR LAWS")

FinancialDeception <- c("FRAUD", 
              "FORGERY/COUNTERFEITING", 
              "EMBEZZLEMENT", 
              "BAD CHECKS")

PersonalViolence <- c("ASSAULT", 
              "DOMESTIC VIOLENCE", 
              "SEX OFFENSES, FORCIBLE", 
              "KIDNAPPING",
              "ROBBERY")

RealEstate <- c("VANDALISM", 
              "TRESPASS",
              "ARSON",
              "BURGLARY")

OtherTheft <- c("LARCENY/THEFT",  
            "VEHICLE THEFT", 
            "STOLEN PROPERTY")

dfCrime$Category[dfCrime$Category %in% DrugAlcohol] <- "DrugAlcohol"
dfCrime$Category[dfCrime$Category %in% FinancialDeception] <- "Financial Deception"
dfCrime$Category[dfCrime$Category %in% PersonalViolence] <- "PersonalViolence"
dfCrime$Category[dfCrime$Category %in% RealEstate] <- "RealEstate"
dfCrime$Category[dfCrime$Category %in% OtherTheft] <- "OtherTheft"
remove(DrugAlcohol, FinancialDeception, 
       OtherTheft, PersonalViolence, RealEstate) # clean up global environment.

# Convert chr to factors and cleaning up dfCrime
dfCrime$Category <- factor(dfCrime$Category)
dfCrime$DayOfWeek <- factor(dfCrime$DayOfWeek, 
                           levels = c("Monday", "Tuesday", "Wednesday", 
                                      "Thursday","Friday","Saturday", "Sunday"), 
                           ordered = TRUE)
dfCrime$PdDistrict <- factor(dfCrime$PdDistrict)
dfCrime$Resolution <- factor(dfCrime$Resolution)

# Import federal holiday data --------------------------------------------------
dfHolidays <- read_csv("holidays.csv", col_types = "ccc")
dfHolidays$DTSTART <- ymd(dfHolidays$DTSTART)
dfHolidays$DTEND <- ymd(dfHolidays$DTEND)

# Convert column names to something more friendly
colnames(dfHolidays) <- c("HolName", "HolStart", "HolEnd")

# Create "Eve" variable by offsetting "HolStart" by 1.
dfHolidays$Eve <- dfHolidays$HolStart - 1


# Import lat/long for Police District HQ----------------------------------------
dfPolInfo <- read_csv("PoliceDept.csv", col_types = "cnn")
  
# Convert values for PdDistrict to AllCaps to match data from dfCrime
dfPolInfo$PdDistrict <- toupper(dfPolInfo$PdDistrict)
 
# Convert PdDistrict to factor
dfPolInfo$PdDistrict <- as.factor(dfPolInfo$PdDistrict)


# Import NOAA precipitation data -----------------------------------------------
# Initial read
dfWeather <- read_csv("SFWeather.csv", col_types = "__dn__")

# Rename variables to be more friendly
names(dfWeather) <- c("Date", "Precip")

# Convert "Date" to Date format for join with dfCrime
dfWeather$Date <- as.Date(as.character(dfWeather$Date), "%Y%m%d")
  
# Convert Precip to "Rain" (TRUE = Rain)
dfWeather$Rain <- ifelse(dfWeather$Precip > 0,TRUE,FALSE)
  


# Join datasets ----------------------------------------------------------------

# Create "Date" column in dfCrime for matching
dfCrime$Date <- date(dfCrime$DateTime)


# Join dfPolInfo to dfCrime ----------------------------------------------------
#  Purpose: to associate each PdDistrict with its geo coordinates (PdX and PdY)
dfCrime <- left_join(dfCrime, dfPolInfo, by = "PdDistrict")

# Join dfWeather to dfCrime ----------------------------------------------------
dfCrime <- left_join(dfCrime, dfWeather, by = "Date")
  
    
# Creating variables for analysis ----------------------------------------------

# Assorted time variables
dfCrime$Month <- month(dfCrime$DateTime)
dfCrime$Hour <- hour(dfCrime$DateTime)
dfCrime$Year <- year(dfCrime$DateTime)

# Reconfigure month variable into seasons
Spring <- dfCrime$Month %in% c(3:5)
Summer <- dfCrime$Month %in% c(6:8)
Autumn <- dfCrime$Month %in% c(9:11)
Winter <- dfCrime$Month %in% c(1, 2, 12)
dfCrime$Season <- c(1:nrow(dfCrime))
dfCrime$Season[Spring] <- "Spring"
dfCrime$Season[Summer] <- "Summer"
dfCrime$Season[Autumn] <- "Autumn"
dfCrime$Season[Winter] <- "Winter"

# Reconfigure Hour variable into TimeOfDay
night <- dfCrime$Hour %in% c(0, 1:5)
morning <- dfCrime$Hour %in% c(6:11)
afternoon <- dfCrime$Hour %in% c(12:17)
evening <- dfCrime$Hour %in% c(18:23)
dfCrime$TimeOfDay <- c(1:nrow(dfCrime))
dfCrime$TimeOfDay[morning] <- "morning"
dfCrime$TimeOfDay[afternoon] <- "afternoon"
dfCrime$TimeOfDay[evening] <- "evening"
dfCrime$TimeOfDay[night] <- "night"
  
# Create variable DayOffTomorrow 
#  This variable measures whether the incident occurred on a day before a 
#  non-workday. If the date appears in the list of holidays, set DayOffTomorrow 
#  to TRUE
dfCrime$DayOffTomorrow <- ifelse (date(dfCrime$DateTime) 
                                      %in% date(dfHolidays$Eve),
                                    "Weekend/holiday", "Workday")
  
# If the day is a Friday or Saturday, set DayOffTomorrow to Weekend/holiday
dfCrime$DayOffTomorrow <- ifelse(dfCrime$DayOfWeek
                                  %in% c("Saturday", "Friday"), 
                                 "Weekend/holiday", dfCrime$DayOffTomorrow)
# convert to factor
dfCrime$DayOffTomorrow <- as.factor(dfCrime$DayOffTomorrow)

remove(morning, afternoon, evening, night, 
       Autumn, Spring, Summer, Winter) # clean up global environment

# Create variable OtherInc
#  This variable measures whether the incident occurred alone or in conjunction 
#  with other incidents or charges. Groups incidents by incident number and gets
# frequency for each incident number
incCount <- dfCrime %>% group_by(IncNum) %>% summarize(n = n()) %>% arrange(desc(n))

# Subtract 1 from frequency so variable will not include its own incident  
incCount$OtherInc <- incCount$n - 1

# Remove the original frequency column so it won't appear in the join
incCount$n <- NULL

# Join to dfCrime
dfCrime <- left_join(dfCrime, incCount, by = "IncNum")



# Create variable PdDistance
#  This variable measures how far (in meters) the incident occurred from the 
#  headquarters of the Police District that handled it. 
#  NOTE: The distHaversine function is inefficient and so is commented out in 
#  favor of faster calculation that is sufficiently precise.

# Apply distHaversine function to each row to calculate distance in meters
  # distPd <- by(dfCrime, 1:nrow(dfCrime), function(row) distHaversine(c(row$X, row$Y), c(row$PdX, row$PdY) ))

# Import vector into dfCrime
  #dfCrime$PdDistance <- distPd 

#### Alternate version
dfCrime$PdDistance <- sqrt((dfCrime$PdX - dfCrime$X)^2 + (dfCrime$PdY - dfCrime$Y)^2)


# Create target variable ActionTaken
#  Whether action was taken in response to an incident
Actions <- c("ARREST, BOOKED",
             "ARREST, CITED", 
             "COMPLAINANT REFUSES TO PROSECUTE", 
             "DISTRICT ATTORNEY REFUSES TO PROSECUTE", 
             "JUVENILE BOOKED",
             "JUVENILE CITED",
             "JUVENILE DIVERTED",
             "LOCATED",
             "PROSECUTED BY OUTSIDE AGENCY",
             "PROSECUTED FOR LESSER OFFENSE",
             "PSYCHOPATHIC CASE")

dfCrime$ActionTaken <- as.factor(ifelse(!(dfCrime$Resolution %in% Actions), "No", "Yes"))


remove(Actions) # clean up global environment

# Create variable PYER (Prior Year Enforcment Rate)
# This variable measures at what rate crimes for each category were enforced 
#  (ARREST/IGNORE) in the prior year
# Create table that reports the response rate by Category, PdDistrict, and MatchYear
dfResponseRate <- dfCrime %>% 
        group_by(Category, ActionTaken, PdDistrict, Year) %>% 
        summarize(n = n()) %>% mutate(PYER = n / sum(n) * 100) %>% 
        filter(ActionTaken == TRUE)

# Offsetting the matchYear so that data will match to prior year. 
# IOW, Incidents in 2005 will be matched to rates that occurred in 2004
dfResponseRate$Year <- dfResponseRate$Year + 1
dfResponseRate <- dfResponseRate %>%
        subset(select = c("Year", "Category", "PdDistrict", "PYER"))

# Join dfResponseRate to dfCrime
dfCrime <- left_join(dfCrime, dfResponseRate, by = c("Category", "PdDistrict", "Year")) 

 

# Final changes before export for analysis in SAS EM ---------------------------

# Remove all data after Dec 31, 2016. This gives the dataset on even start and 
# end date and takes care of missing values for Precipitation
bef2017 <- year(dfCrime$DateTime) != 2017
dfCrime <- dfCrime[bef2017,]
remove(bef2017) # clean up global environment

# Remove single case with unexplained missing value on PdDistrict
dfCrime <- dfCrime %>% filter(!is.na(PdDistrict))

# For any observations after 2003, if PYER is NA, set it to 0. These are 
#  Crime Categories that had no precedent in that district in the prior year. 
#  Thus PYER should be 0
naPYERaft2003 <- is.na(dfCrime$PYER) & year(dfCrime$DateTime) > 2003
dfCrime$PYER[naPYERaft2003] <- 0
remove(naPYERaft2003) # clean up global environment

# Select and order final variables for analysis
dfCrime <- dfCrime %>% 
        subset(select = c("Category", 
                          "ActionTaken", "Month", "Season", "TimeOfDay", "Year", 
                          "DayOfWeek", "DayOffTomorrow", "PdDistrict", 
                          "PdDistance", "OtherInc", "PYER", "Precip"))

remove(dfHolidays, dfPolInfo, dfResponseRate, 
       dfWeather, incCount) # clean up global environment


# Exporting Crime data as r object ---------------------------------------------
saveRDS(dfCrime, "dfCrime.RDS")




# Set up citations list --------------------------------------------------------
citations <- list()  # Title = "", Author = "", AccessDate = "", URL = ""

citations <- list(
list(Name = "Crime", Title = "Police Department Incidents", 
     Author = "City and County of San Francisco", 
     AccessDate = date("2017-04-27"), 
     url = "http://www.datasf.org"), 
list(Name = "Holiday", Title = "Federal Holidays", 
     Author = "Office of Personnel Management, U.S. Office of Personnel", 
     AccessDate = date("2017-04-27"), 
     url = "https://catalog.data.gov/dataset/federal-holidays"), 
list(Name = "PdDistrict", 
     Title = "Redistricting of San Francisco Police Department Station Boundaries", 
     Author = "San Francisco Police Department", 
     AccessDate = date("2017-04-27"), PubDate = date("2017-07-17"), 
     url = "http://sanfranciscopolice.org/article/redistricting-san-francisco-police-department-station-boundaries"), 
list(Name = "Weather", 
     Title = "Climate Data Online", 
     Author = "National Centers for Environmental Information, National Oceanic and Atmospheric Administration", 
     AccessDate = date("2017-04-30"), url = "https://www.ncdc.noaa.gov/cdo-web/")
)

