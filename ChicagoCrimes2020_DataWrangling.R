library(utils)
library(mice)
library(tidyverse)
library(data.table)
library(DT)
library(lubridate)
library(plotly) 
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus) 
library(ggplot2)
library(leaflet)
library(htmltools)
library(scales)
library(priceR)
library(sf)
library(shinycssloaders)
library(leaflet)
library(lattice)
library(VIM)

set.seed(42)
options(scipen = 100, digits = 4)


####--------------- Loading the data -----------------------------------------------
crime <- data.table(fread("Crimes_2020.csv"))
area.shp <- sf::st_read("AreaTypeChicago.shp")



####--------------- Checking for missing data and initial Analysis -----------------
# Missing Data
sapply(crime, function(x) sum(is.na(x)))

# Distribution
summary(crime)

# sum of all NA values
missvaltotal <- data.frame(value = colSums(is.na(crime)))

#calculating the percentage of missing values
missvaltotal$Proportion_Missing <- (missvaltotal$value/nrow(crime)*100)

#missing value columns with proportion
missval.var <- missvaltotal[missvaltotal$value > 0, ]




####--------------- Data Manipulation ------------------------------------------
# Formatting Date column
crime$Date <- as.POSIXct(crime$Date, format = "%m/%d/%Y %H:%M")


# Dropping Missing  records of Date, Latitude and Longitude
crime <- crime[!is.na(crime$Date)]
crime <- crime[!is.na(crime$Latitude)]



# Dropping columns
#----  x coordinate and y coordinate (location of crime) is captured by lat and log variables
crime <- crime[,-c("X Coordinate", "Y Coordinate", "Location")]

#---- Since data is for 2020 only, dropping the year column
crime <- crime[, -c("Year")]

#---- Updated on
crime <- crime[, -c("Updated On")]



# Removing records outside Chicago
crime <- crime[crime$ID != 12091638,]
crime <- crime[crime$ID != 12108805,]




# Adding columns
#---- adding months
crime$Month <- month(crime$Date,label = TRUE, abbr = FALSE)

#---- adding day of the month
crime$Day <- as.numeric(format(crime$Date, "%d"))

#---- adding weekdays
crime$Weekday <- weekdays(crime$Date)

#---- adding hour of the day
crime$Hour <- as.numeric(format(crime$Date, "%H"))

#---- adding weekday/weekend
crime$Weekday.end <- ifelse(crime$Weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday")


#---- redefining crime type as there are 32 unique primary type
unique(sort(crime$`Primary Type`))

crime$`Crime Category` <- as.character(crime$`Primary Type`)
crime$`Crime Category` <- ifelse(crime$`Primary Type` %in% c("ARSON"),
                                 "Arson", crime$`Crime Category`)
crime$`Crime Category` <- ifelse(crime$`Primary Type` %in% c("ASSAULT"),
                                 "Assault", crime$`Crime Category`)
crime$`Crime Category` <- ifelse(crime$`Primary Type` %in% c("BATTERY"),
                                 "Battery", crime$`Crime Category`)
crime$`Crime Category` <- ifelse(crime$`Primary Type` %in% c("BURGLARY","MOTOR VEHICLE THEFT","ROBBERY","THEFT"),
                                 "Theft", crime$`Crime Category`)
crime$`Crime Category` <- ifelse(crime$`Primary Type` %in% c("CONCEALED CARRY LICENSE VIOLATION", 
                                                             "WEAPONS VIOLATION"),
                                 "Weapons", crime$`Crime Category`)
crime$`Crime Category` <- ifelse(crime$`Primary Type` %in% c("CRIM SEXUAL ASSAULT", "CRIMINAL SEXUAL ASSAULT", "SEX OFFENSE"),
                                 "Sexual", crime$`Crime Category`)
crime$`Crime Category` <- ifelse(crime$`Primary Type` %in% c("CRIMINAL DAMAGE"),
                                 "Damage", crime$`Crime Category`)
crime$`Crime Category` <- ifelse(crime$`Primary Type` %in% c("DECEPTIVE PRACTICE", "GAMBLING", "INTERFERENCE WITH PUBLIC OFFICER",
                                                             "INTIMIDATION","LIQUOR LAW VIOLATION", "NON-CRIMINAL","OBSCENITY",
                                                             "PUBLIC INDECENCY","PUBLIC PEACE VIOLATION", "STALKING",
                                                             "CRIMINAL TRESPASS"),
                                 "Non Violence", crime$`Crime Category`)
crime$`Crime Category` <- ifelse(crime$`Primary Type` %in% c("HOMICIDE"),
                                 "Homicide", crime$`Crime Category`)
crime$`Crime Category` <- ifelse(crime$`Primary Type` %in% c("HUMAN TRAFFICKING","KIDNAPPING","PROSTITUTION",
                                                             "OFFENSE INVOLVING CHILDREN"),
                                 "Exploitation", crime$`Crime Category`)
crime$`Crime Category` <- ifelse(crime$`Primary Type` %in% c("NARCOTICS","OTHER NARCOTIC VIOLATION"),
                                 "Narcotics", crime$`Crime Category`)
crime$`Crime Category` <- ifelse(crime$`Primary Type` %in% c("OTHER OFFENSE"),
                                 "Others", crime$`Crime Category`)



# Missing values on Location Description -- adding as other
crime$`Location Description` <- ifelse(crime$`Location Description` == "", "OTHER", crime$`Location Description`)


# Joining Shapefile with Dataframe
crime_sf = st_as_sf(crime, coords = c("Longitude", "Latitude"), crs = 4326) # 4326 is 
area_crime_join <- sf::st_join(area.shp , crime_sf, join = st_intersects)

# Dropping geometry
area_crime_join$geometry <- NULL


# Frequency Calculation
shp.freq.community <- data.table(area_crime_join %>% 
                                   filter(`Primary Type` %in% c("BATTERY","THEFT")) %>%
                                   group_by(AREA_NUM_1,COMMUNITY,Weekday.end,Hour,`Primary Type`) %>%
                                   summarise(freq = n()))







#### ---------------- Column Description for App --------------------------------------------
col.details <- data.table("Column Name" = c("ID","Case Number","Date", "Block", "IUCR", "Primary Type", "Description", 
                                            "Location Description","Arrest", "Domestic", "Beat", "District", "Ward", 
                                            "Community Area", "FBI Code", "Latitude","Longitude", "Month", "Day", 
                                            "Weekday", "Hour", "Crime Category", "Weekday.end"),
                          "Description" = c("Unique identifier for the record",
                                            "The Chicago Police Department RD Number (Records Division Number),which is unique to the incident",
                                            "Date and Time when the incident occurred",
                                            "The partially redacted address where the incident occurred",
                                            "The Illinois Unifrom Crime Reporting code",
                                            "The primary description of the IUCR code",
                                            "The secondary description of the IUCR code",
                                            "Description of the location where the incident occurred",
                                            "Indicates whether an arrest was made",
                                            "Indicates whether the incident was domestic-related as defined by the Illinois Domestic Violence Act",
                                            "Indicates the beat where the incident occurred",
                                            "Indicates the police district where the incident occurred",
                                            "The ward (City Council district) where the incident occurred",
                                            "Indicates the community area where the incident occurred",
                                            "Indicates the crime classification as outlined in the FBI's National Incident-Based Reporting System (NIBRS)",
                                            "The latitude of the location where the incident occurred.",
                                            "The longitude of the location where the incident occurred.",
                                            "Calculated field indicating the month when the incident occured",
                                            "Calculated field indicating the day of the month when the incident occured",
                                            "Calculated field indicating the day of the week when the incident occured",
                                            "Calculated field indicating the time of the day when the incident occured",
                                            "Redifined Crime categories for each primary crime type",
                                            "Calculated field indicating Weekday or Weekend"))


#### -------------------------------------------------------------------------
#### -------------------------- Shiny App ------------------------------------
#### -------------------------------------------------------------------------

#Running the App
shinyApp(ui, server)