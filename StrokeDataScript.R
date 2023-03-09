######################################################################################################
#Capstone Project
#Name: Alex McCorriston
#Date: 02/28/2023
######################################################################################################

library(corrplot)
library(ggmap)
library(ggrepel)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(readxl)
library(tidycensus)
library(tidyr)
library(tidyverse) 
library(writexl)

setwd("C:/Users/alexm/OneDrive/Desktop/UCLA Data Science Certification/Capstone Project")

#Import Ischemic Stroke data set. 
stroke = read.csv("IschemicStrokeData.csv")
summary(stroke)
str(stroke)

#Data management.

#Rename column variables
colnames(stroke)[c(6, 7, 8, 9, 10)] = c("Risk Adjusted Rate", "Number of 30 Day Readmissions/Deaths", "Number of Total Admissions", "Hospital Ratings", "Location")
                      
#Delete rows in data frame that contain statewide data rather than county data.
stroke = stroke[!grepl("statewide", stroke$Hospital, ignore.case = T), ]

#Change variables to appropriate types.
stroke$Year = factor(stroke$Year, levels = c('2011-2012', '2012-2013', '2013-2014', '2014-2015'))
stroke$OSHPDID = as.numeric(stroke$OSHPDID)
stroke$`Hospital Ratings` = factor(stroke$`Hospital Ratings`, levels = c('Worse', 'As Expected', 'Better'), ordered = T)

#Check to see whether number of OSPHDIDs and Hospitals match (they should).
unique_ID = unique(stroke$OSHPDID)
length(unique_ID) #There are 288 unique OSHPDIDs.

unique_hosp = unique(stroke$Hospital)
length(unique_hosp) #There are 395 unique Hospitals.

#Edit Hospital column of data frame so all OSHPDIDs and Hospitals match. 
stroke$Hospital = gsub("<96>", "", stroke$Hospital)
stroke$Hospital = gsub("<92>", "", stroke$Hospital)
stroke$Hospital = gsub("[-–—]", "-", stroke$Hospital)
stroke$Hospital = gsub("’", "'", stroke$Hospital)
stroke$Hospital = gsub(" Center  ", " Center - ", stroke$Hospital)
stroke$Hospital = gsub(" Hospital  ", " Hospital - ", stroke$Hospital)
stroke$Hospital = gsub("PeniAs Expectedula", "Peninsula", stroke$Hospital)
stroke$Hospital = gsub("Park  Slauson", "Park - Slauson", stroke$Hospital)
stroke$Hospital = gsub("JohAs Expectedton Memorial", "Johnston Memorial", stroke$Hospital)
stroke$Hospital = gsub("HawkiAs Expected", "Hawkins", stroke$Hospital)
stroke$Hospital = gsub("Newhall Hospital", "Newhall Memorial Hospital", stroke$Hospital)
stroke$Hospital = gsub("Hospital - SuAs Expectedet", "Hospital - Sunset", stroke$Hospital)
stroke$Hospital = gsub("Hospital  Orange County  Anaheim", "Hospital - Orange County - Anaheim", stroke$Hospital)
stroke$Hospital = gsub("County/Harbor  UCLA", "County/Harbor - UCLA", stroke$Hospital)
stroke$Hospital = gsub("County/Olive View  UCLA", "County/Olive View - UCLA", stroke$Hospital)
stroke$Hospital = gsub("Mercy Hospital  of Folsom", "Mercy Hospital - Folsom", stroke$Hospital)
stroke$Hospital = gsub("Mercy Medical Center  ", "Mercy Medical Center - ", stroke$Hospital)
stroke$Hospital = gsub("Mercy San Juan Hospital", "Mercy Hospital - San Juan", stroke$Hospital)
stroke$Hospital = gsub("VacaValley", "", stroke$Hospital)
stroke$Hospital = gsub("Bernadine", "Bernardine", stroke$Hospital)
stroke$Hospital = gsub("Saint Johns", "Saint John's", stroke$Hospital)
stroke$Hospital = gsub("Josephs", "Joseph's", stroke$Hospital)
stroke$Hospital = gsub("Marys", "Mary's", stroke$Hospital)
stroke$Hospital = gsub("Santa Monica  UCLA", "Santa Monica - UCLA", stroke$Hospital)
stroke$Hospital = gsub("Services  Sycamore", "Services - Sycamore", stroke$Hospital)
stroke$Hospital = gsub("System  Murrieta", "System - Murrieta", stroke$Hospital)
stroke$Hospital = gsub("OceaAs Expectedide", "Oceanside", stroke$Hospital)
stroke$Hospital = gsub("Victor Valley Community Hospital", "Victor Valley Global Medical Center", stroke$Hospital)
stroke$Hospital = gsub("Alhambra Hospital Medical Center", "Alhambra Hospital", stroke$Hospital)
stroke$Hospital = gsub("Sutter General Hospital", "Sutter Medical Center, Sacramento", stroke$Hospital)
stroke$Hospital = gsub("Lukes", "Luke's", stroke$Hospital)
stroke$Hospital = gsub("Community Hospital Long Beach", "Community Hospital of Long Beach", stroke$Hospital)
stroke$Hospital = gsub("Esplanade Campus", "Esplanade", stroke$Hospital)
stroke$Hospital = gsub("County  Anaheim", "County - Anaheim", stroke$Hospital)
stroke$Hospital = gsub("of Folsom", "- Folsom", stroke$Hospital)
stroke$Hospital = gsub("Center Mt. Shasta", "Center - Mt. Shasta", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106450940, "Shasta Regional Medical Center", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106190053, "Saint Mary Medical Center - Long Beach", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106281047, "Queen of the Valley Hospital – Napa", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106430837, "O'Connor Hospital - San Jose", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106440755, "Dominican Hospital - Santa Cruz/Soquel", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106410852, "Mills-Peninsula Medical Center", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106400466, "Marian Regional Medical Center - Arroyo Grande", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106301132, "Kaiser Foundation Hospital - Anaheim", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106190818, "University of Southern California Verdugo Hills Hospital", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106190756, "Providence Saint John's Health Center", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106190587, "Pacific Hospital of Long Beach", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106190243, "Downey Regional Medical Center", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106190110, "Southern California Hospital At Culver City", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106050932, "Mark Twain Medical Center", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106010937, "Alta Bates Summit Medical Center", stroke$Hospital)
stroke$Hospital = ifelse(stroke$OSHPDID == 106010846, "Highland Hospital", stroke$Hospital)

unique_hosp = unique(stroke$Hospital)
length(unique_hosp) #There are now 288 unique Hospitals.

#Split Location variable into Latitude and Longitude variables and change them to numeric data type.
stroke = separate(stroke, col = Location, into = c("Latitude", "Longitude"), sep = ",", remove = T)
stroke$Latitude = gsub("\\(", "", stroke$Latitude)
stroke$Longitude = gsub("\\)", "", stroke$Longitude)
stroke$Latitude = as.numeric(stroke$Latitude)
stroke$Longitude = as.numeric(stroke$Longitude)

#Create a new variable for Capacity, Zipcode, Business Name, and Entity Type data by merging data from health_facility_locations.xlsx. 
#URL to download health_facility_locations.xlsx: https://data.chhs.ca.gov/dataset/healthcare-facility-locations/resource/098bbc36-044d-441f-9442-1f4db4d8aaa0 
hospital_facilities_ca = read_excel('health_facility_locations.xlsx')
stroke = merge(stroke, hospital_facilities_ca, by.x = "OSHPDID", by.y = "OSHPD_ID", all.x = T)
stroke = stroke[, c(1:11, 34, 37, 52, 57)]
colnames(stroke)[12] = "Capacity (Number of Beds)"
colnames(stroke)[13] = "Zipcode"
colnames(stroke)[14] = "Business Name"
colnames(stroke)[15] = "Entity Type"

sum(is.na(stroke$`Capacity (Number of Beds)`)) #There are 140 missing Capacity (Number of Beds), Zipcodes, Business Names, and Entity Types. Find them manually. 
sum(is.na(stroke$Zipcode)) 
sum(is.na(stroke$`Business Name`))
sum(is.na(stroke$`Entity Type`))

missing_data = stroke[!complete.cases(stroke), ] #There are 150 rows of missing data. 10 of them have missing RARs but complete Capacity (Number of Beds), Zipcodes,
#Business Names, and Entity Types, so remove them from missing_data and stroke data frames.
missing_data = missing_data[complete.cases(missing_data$'Hospital Ratings'), ]
stroke = stroke[complete.cases(stroke$`Hospital Ratings`), ]
missing_data = missing_data[, c(1, 12:15)]

#Create data frame with missing data values (found from https://hcai.ca.gov/). 
missing_data_val = data.frame(OSHPDID = c(106380929, 106380964, 106190475, 106070904, 106010805, 106040875, 106230949,  106301132, 106010858, 106010856, 106410804,
106201281, 106190534, 106190307, 106370755, 106190762, 106490919, 106341052, 106190784), 
"Capacity (Number of Beds)" = c(501, 120, 158, 189, 130, 100, 25, 526, 100, 365, 153, 106, 204, 138, 286, 366, 84, 523, 170),
Zipcode = c("94117", "94110", "90804", "94806", "94546", "95969", "95490", "92618", "94538", "94801", "94063", "93637", "90036", "90012", "92025", "90057", "95404", "95819", "90004"),
"Business Name" = c("SUTTER HEALTH", "SUTTER HEALTH", "MEMORIAL HEALTH SERVICES", "WEST CONTRA COSTA HEALTHCARE DISTRICT", "SUTTER HEALTH", "FEATHER RIVER HOSPITAL",
"ADVENTIST HEALTH", "KAISER FOUNDATION HOSPITALS", "KAISER FOUNDATION HOSPITALS", "KAISER FOUNDATION HOSSPITALS", "KAISER FOUNDATION HOSPITALS", "MADERA COMMUNITY HOSPITAL",
"ALECTO HEALTHCARE SERVICES LLC", "PAMC, LTD", "PALOMAR HEALTH", "VERITY HEALTH SYSTEM", "SUTTER HEALTH", "SUTTER HEALTH", "TEMPLE HOSPITAL CORPORATION"),
"Entity Type" = c("NONPROFIT CORP", "NONPROFIT CORP", "LIMITED LIABILITY COMPANY", "LIMITED LIABILITY COMPANY", "NONPROFIT CORP", "NONPROFIT CORP", "NONPROFIT CORP", "NONPROFIT CORP",
"NONPROFIT CORP", "NONPROFIT CORP", "NONPROFIT CORP", "NONPROFIT CORP", "LIMITED LIABILITY COMPANY", "PROFIT CORP", "HEALTH CARE DISTRICT", "NONPROFIT CORP", "NONPROFIT CORP", "NONPROFIT CORP",
"PROFIT CORP"))

#Merge missing_data and missing_data_val by OSPHDID.
missing_data = merge(missing_data, missing_data_val, by = 'OSHPDID', all.x = T)
missing_data = missing_data[, c(1, 6:9)]
names(missing_data) = c("OSHPDID", "Capacity (Number of Beds)", "Zipcode", "Business Name", "Entity Type")

#Merge stroke with missing_data to fill in NAs with missing_data values.
stroke1 = stroke[, -c(1, 12:15)]
stroke2 = stroke[complete.cases(stroke), ] 
stroke2 = stroke2[, c(1, 12:15)]
stroke3 = rbind(stroke2, missing_data)
stroke4 = cbind(stroke1, stroke3)

#Change Business Names as needed.
stroke4$`Business Name` = ifelse(grepl("SUTTER", stroke4$`Business Name`), "SUTTER HEALTH", stroke4$`Business Name`)
stroke4$`Business Name` = ifelse(grepl("AHMC", stroke4$`Business Name`), "AHMC HEALTHCARE, INC.", stroke4$`Business Name`)
stroke4$`Business Name` = ifelse(grepl("ADVENTIST", stroke4$`Business Name`), "ADVENTIST HEALTH", stroke4$`Business Name`)
stroke4$`Business Name` = ifelse(grepl("DIGNITY", stroke4$`Business Name`), "DIGNITY COMMUNITY CARE", stroke4$`Business Name`)
stroke4$`Business Name` = ifelse(grepl("DOCTORS MEDICAL CENTER OF MODESTO, INC.", stroke4$`Business Name`), "DOCTOR'S MEDICAL CENTER OF MODESTO, INC.", stroke4$`Business Name`)
stroke4$`Business Name` = ifelse(grepl("PIH", stroke4$`Business Name`), "PIH HEALTH", stroke4$`Business Name`)
stroke4$`Business Name` = ifelse(grepl("PRIME", stroke4$`Business Name`), "PRIME HEALTHCARE SERVICES, INC.", stroke4$`Business Name`)
stroke4$`Entity Type` = as.factor(stroke4$`Entity Type`) #Change Entity Type to factor variable.

#Flip Number of 30 Day Readmissions/Death values and Number of Total Admissions values for the 2013-2014 year because they are switched in raw data.
stroke5 = stroke4
stroke5[stroke5$Year == '2013-2014', c(6, 7)] = stroke5[stroke5$Year == '2013-2014', c(7, 6)]
stroke4 = stroke5 #Update original data frame with flipped values.

#Split up stroke data frame by year in order to add variables from ACS data by year. 
stroke11 = subset(stroke4, Year == '2011-2012')
stroke12 = subset(stroke4, Year == '2012-2013')
stroke13 = subset(stroke4, Year == '2013-2014')
stroke14 = subset(stroke4, Year == '2014-2015')

#Import county health data by year (data from https://www.countyhealthrankings.org/explore-health-rankings/california/data-and-resources).
chealth11 = read_excel('CHealth11.xls', sheet = 'Outcomes & Factors Rankings')
chealth12 = read_excel('CHealth12.xls', sheet = 'Outcomes & Factors Rankings')
chealth13 = read_excel('CHealth13.xls', sheet = 'Outcomes & Factors Rankings')
chealth14 = read_excel('CHealth14.xls', sheet = 'Outcomes & Factors Rankings')

#Merge chealth data with stroke data by year.
stroke11 = merge(stroke11, chealth11, by.x = 'County', by.y = '...3', all.x = T)
stroke11 = stroke11[, -c(16, 17, 18, 20)]
colnames(stroke11)[16] = "County Rank Health Outcomes"
colnames(stroke11)[17] = "County Rank Health Factors"

stroke12 = merge(stroke12, chealth12, by.x = 'County', by.y = '...3', all.x = T)
stroke12 = stroke12[, -c(16, 17, 18, 20)]
colnames(stroke12)[16] = "County Rank Health Outcomes"
colnames(stroke12)[17] = "County Rank Health Factors"

stroke13 = merge(stroke13, chealth13, by.x = 'County', by.y = '...3', all.x = T)
stroke13 = stroke13[, -c(16, 17, 18, 20)]
colnames(stroke13)[16] = "County Rank Health Outcomes"
colnames(stroke13)[17] = "County Rank Health Factors"

stroke14 = merge(stroke14, chealth14, by.x = 'County', by.y = '...3', all.x = T)
stroke14 = stroke14[, -c(16, 17, 18, 20)]
colnames(stroke14)[16] = "County Rank Health Outcomes"
colnames(stroke14)[17] = "County Rank Health Factors"

#Import median household income, education, population, poverty rate, and age 65-84 data from census and add it as a variable to the data frame by year.
#Note: ACS variable "DP05_0017PE" for percent of total population ages 85 and up is not included because it had NAs for all relevant years. 

#Only need to run the following three lines one time (each person will have their own key). 
#census_api_key("bba651673f601ccdf83ed14027327f1c4404d6f8", install = TRUE) # overwrite = TRUE.
#readRenviron("~/.Renviron")
#Sys.getenv("CENSUS_API_KEY")

#Get ACS variable key for each year in stroke data set to ensure variables remain consistent over time.
acs_var11 = load_variables(2012, "acs5", cache = TRUE)
acs_var12 = load_variables(2013, "acs5", cache = TRUE)
acs_var13 = load_variables(2014, "acs5", cache = TRUE)
acs_var14 = load_variables(2015, "acs5", cache = TRUE)

#2011:
mincome11 = get_acs(geography = "zcta", variables = "B19013_001", state = "CA", year= 2011)
mincome11 = mincome11[, c(2, 4)]
mincome11$NAME = gsub("ZCTA5 ", "", mincome11$NAME)
colnames(mincome11)[2] = "Median Household Income Estimate"

ed11 = get_acs(geography = "zcta", variables = c(ed_below9 = "DP02_0060PE", ed_g912 = "DP02_0061PE",ed_hs = "DP02_0062PE",
                                                 ed_scollege = "DP02_0063PE",ed_associate = "DP02_0064PE",ed_bachelor = "DP02_0065PE",ed_higher = "DP02_0066PE"),                                        
               state = "CA", survey = "acs5", output = "wide", year = 2011)
chci11 = ed11 %>% dplyr::select(GEOID, NAME, ed_below9, ed_g912, ed_hs, ed_scollege, ed_associate, ed_bachelor, ed_higher)
chci11 = chci11 %>% mutate(chci12 = (1 / 100) * (50 * ed_below9 + 100 * ed_g912 + 120 * ed_hs + 130 * ed_scollege + 140 * ed_associate + 190 * ed_bachelor + 230 * ed_higher))
chci11 = chci11[, c(2, 10)]
chci11$NAME = gsub("ZCTA5 ", "", chci11$NAME)
colnames(chci11)[2] = "CHCI"

pop11 = get_acs(geography = "zcta", variables = "B01003_001", state = "CA", year= 2011)
pop11 = pop11[, c(2, 4)]
pop11$NAME = gsub("ZCTA5 ", "", pop11$NAME)
colnames(pop11)[2] = "Population Estimate"

povr11 = get_acs(geography = "zcta", variables = c(poverty ="DP03_0119PE"), state = "CA", year= 2011)
povr11 = povr11[, c(2, 4)]
povr11$NAME = gsub("ZCTA5 ", "", povr11$NAME)
colnames(povr11)[2] = "Poverty Rate"

age45a11 = get_acs(geography = "zcta", variables = c(a7584="DP05_0016PE", a6574 = "DP05_0015PE", a6064 ="DP05_0014PE", a5559 ="DP05_0013PE", a4554 ="DP05_0012PE"), 
state = "CA", year = 2011, output = "wide")
age45a11 = age45a11[, c(2, 3, 5, 7, 9, 11)]
age45a11 = age45a11 %>% mutate('Percent Age 65-84' = a6574 + a7584, 'Percent Age 60 Up' = a6064 + `Percent Age 65-84`, 'Percent Age 55 Up' = a5559 + `Percent Age 60 Up`,
'Percent Age 45 Up' = a4554 + `Percent Age 55 Up`)
age45a11 = age45a11[, c(1, 7:10)]
age45a11$NAME = gsub("ZCTA5 ", "", age45a11$NAME)

acs11var = merge(mincome11, chci11, by = "NAME")
acs11var = merge(acs11var, pop11, by = "NAME")
acs11var = merge(acs11var, povr11, by = "NAME")
acs11var = merge(acs11var, age45a11, by = "NAME")

stroke11 = merge(stroke11, acs11var, by.x = "Zipcode", by.y = "NAME", all.x = T)

#2012:
mincome12 = get_acs(geography = "zcta", variables = "B19013_001", state = "CA", year= 2012)
mincome12 = mincome12[, c(2, 4)]
mincome12$NAME = gsub("ZCTA5 ", "", mincome12$NAME)
colnames(mincome12)[2] = "Median Household Income Estimate"

ed12 = get_acs(geography = "zcta", variables = c(ed_below9 = "DP02_0060PE", ed_g912 = "DP02_0061PE",ed_hs = "DP02_0062PE",
              ed_scollege = "DP02_0063PE",ed_associate = "DP02_0064PE",ed_bachelor = "DP02_0065PE",ed_higher = "DP02_0066PE"),                                        
              state = "CA", survey = "acs5", output = "wide", year = 2012)
chci12 = ed12 %>% dplyr::select(GEOID, NAME, ed_below9, ed_g912, ed_hs, ed_scollege, ed_associate, ed_bachelor, ed_higher)
chci12 = chci12 %>% mutate(chci12 = (1 / 100) * (50 * ed_below9 + 100 * ed_g912 + 120 * ed_hs + 130 * ed_scollege + 140 * ed_associate + 190 * ed_bachelor + 230 * ed_higher))
chci12 = chci12[, c(2, 10)]
chci12$NAME = gsub("ZCTA5 ", "", chci12$NAME)
colnames(chci12)[2] = "CHCI"

pop12 = get_acs(geography = "zcta", variables = "B01003_001", state = "CA", year= 2012)
pop12 = pop12[, c(2, 4)]
pop12$NAME = gsub("ZCTA5 ", "", pop12$NAME)
colnames(pop12)[2] = "Population Estimate"

povr12 = get_acs(geography = "zcta", variables = c(poverty ="DP03_0119PE"), state = "CA", year= 2012)
povr12 = povr12[, c(2, 4)]
povr12$NAME = gsub("ZCTA5 ", "", povr12$NAME)
colnames(povr12)[2] = "Poverty Rate"

age45a12 = get_acs(geography = "zcta", variables = c(a7584="DP05_0016PE", a6574 = "DP05_0015PE", a6064 ="DP05_0014PE", a5559 ="DP05_0013PE", a4554 ="DP05_0012PE"), 
state = "CA", year = 2012, output = "wide")
age45a12 = age45a12[, c(2, 3, 5, 7, 9, 11)]
age45a12 = age45a12 %>% mutate('Percent Age 65-84' = a6574 + a7584, 'Percent Age 60 Up' = a6064 + `Percent Age 65-84`, 'Percent Age 55 Up' = a5559 + `Percent Age 60 Up`,
'Percent Age 45 Up' = a4554 + `Percent Age 55 Up`)
age45a12 = age45a12[, c(1, 7:10)]
age45a12$NAME = gsub("ZCTA5 ", "", age45a12$NAME)

acs12var = merge(mincome12, chci12, by = "NAME")
acs12var = merge(acs12var, pop12, by = "NAME")
acs12var = merge(acs12var, povr12, by = "NAME")
acs12var = merge(acs12var, age45a12, by = "NAME")

stroke12 = merge(stroke12, acs12var, by.x = "Zipcode", by.y = "NAME", all.x = T)

#2013:
mincome13 = get_acs(geography = "zcta", variables = "B19013_001", state = "CA", year= 2013)
mincome13 = mincome13[, c(2, 4)]
mincome13$NAME = gsub("ZCTA5 ", "", mincome13$NAME)
colnames(mincome13)[2] = "Median Household Income Estimate"

ed13 = get_acs(geography = "zcta", variables = c(ed_below9 = "DP02_0060PE", ed_g912 = "DP02_0061PE",ed_hs = "DP02_0062PE",
                                                 ed_scollege = "DP02_0063PE",ed_associate = "DP02_0064PE",ed_bachelor = "DP02_0065PE",ed_higher = "DP02_0066PE"),                                        
               state = "CA", survey = "acs5", output = "wide", year = 2013)
chci13 = ed13 %>% dplyr::select(GEOID, NAME, ed_below9, ed_g912, ed_hs, ed_scollege, ed_associate, ed_bachelor, ed_higher)
chci13 = chci13 %>% mutate(chci13 = (1 / 100) * (50 * ed_below9 + 100 * ed_g912 + 120 * ed_hs + 130 * ed_scollege + 140 * ed_associate + 190 * ed_bachelor + 230 * ed_higher))
chci13 = chci13[, c(2, 10)]
chci13$NAME = gsub("ZCTA5 ", "", chci13$NAME)
colnames(chci13)[2] = "CHCI"

pop13 = get_acs(geography = "zcta", variables = "B01003_001", state = "CA", year= 2013)
pop13 = pop13[, c(2, 4)]
pop13$NAME = gsub("ZCTA5 ", "", pop13$NAME)
colnames(pop13)[2] = "Population Estimate"

povr13 = get_acs(geography = "zcta", variables = c(poverty ="DP03_0119PE"), state = "CA", year= 2013)
povr13 = povr13[, c(2, 4)]
povr13$NAME = gsub("ZCTA5 ", "", povr13$NAME)
colnames(povr13)[2] = "Poverty Rate"

age45a13 = get_acs(geography = "zcta", variables = c(a7584="DP05_0016PE", a6574 = "DP05_0015PE", a6064 ="DP05_0014PE", a5559 ="DP05_0013PE", a4554 ="DP05_0012PE"), 
state = "CA", year = 2013, output = "wide")
age45a13 = age45a13[, c(2, 3, 5, 7, 9, 11)]
age45a13 = age45a13 %>% mutate('Percent Age 65-84' = a6574 + a7584, 'Percent Age 60 Up' = a6064 + `Percent Age 65-84`, 'Percent Age 55 Up' = a5559 + `Percent Age 60 Up`,
'Percent Age 45 Up' = a4554 + `Percent Age 55 Up`)
age45a13 = age45a13[, c(1, 7:10)]
age45a13$NAME = gsub("ZCTA5 ", "", age45a13$NAME)

acs13var = merge(mincome13, chci13, by = "NAME")
acs13var = merge(acs13var, pop13, by = "NAME")
acs13var = merge(acs13var, povr13, by = "NAME")
acs13var = merge(acs13var, age45a13, by = "NAME")

stroke13 = merge(stroke13, acs13var, by.x = "Zipcode", by.y = "NAME", all.x = T)

#2014:
mincome14 = get_acs(geography = "zcta", variables = "B19013_001", state = "CA", year= 2014)
mincome14 = mincome14[, c(2, 4)]
mincome14$NAME = gsub("ZCTA5 ", "", mincome14$NAME)
colnames(mincome14)[2] = "Median Household Income Estimate"

ed14 = get_acs(geography = "zcta", variables = c(ed_below9 = "DP02_0060PE", ed_g912 = "DP02_0061PE",ed_hs = "DP02_0062PE",
                                                 ed_scollege = "DP02_0063PE",ed_associate = "DP02_0064PE",ed_bachelor = "DP02_0065PE",ed_higher = "DP02_0066PE"),                                        
               state = "CA", survey = "acs5", output = "wide", year = 2014)
chci14 = ed14 %>% dplyr::select(GEOID, NAME, ed_below9, ed_g912, ed_hs, ed_scollege, ed_associate, ed_bachelor, ed_higher)
chci14 = chci14 %>% mutate(chci14 = (1 / 100) * (50 * ed_below9 + 100 * ed_g912 + 120 * ed_hs + 130 * ed_scollege + 140 * ed_associate + 190 * ed_bachelor + 230 * ed_higher))
chci14 = chci14[, c(2, 10)]
chci14$NAME = gsub("ZCTA5 ", "", chci14$NAME)
colnames(chci14)[2] = "CHCI"

pop14 = get_acs(geography = "zcta", variables = "B01003_001", state = "CA", year= 2014)
pop14 = pop14[, c(2, 4)]
pop14$NAME = gsub("ZCTA5 ", "", pop14$NAME)
colnames(pop14)[2] = "Population Estimate"

povr14 = get_acs(geography = "zcta", variables = c(poverty ="DP03_0119PE"), state = "CA", year= 2014)
povr14 = povr14[, c(2, 4)]
povr14$NAME = gsub("ZCTA5 ", "", povr14$NAME)
colnames(povr14)[2] = "Poverty Rate"

age45a14 = get_acs(geography = "zcta", variables = c(a7584="DP05_0016PE", a6574 = "DP05_0015PE", a6064 ="DP05_0014PE", a5559 ="DP05_0013PE", a4554 ="DP05_0012PE"), 
state = "CA", year = 2014, output = "wide")
age45a14 = age45a14[, c(2, 3, 5, 7, 9, 11)]
age45a14 = age45a14 %>% mutate('Percent Age 65-84' = a6574 + a7584, 'Percent Age 60 Up' = a6064 + `Percent Age 65-84`, 'Percent Age 55 Up' = a5559 + `Percent Age 60 Up`,
'Percent Age 45 Up' = a4554 + `Percent Age 55 Up`)
age45a14 = age45a14[, c(1, 7:10)]
age45a14$NAME = gsub("ZCTA5 ", "", age45a14$NAME)

acs14var = merge(mincome14, chci14, by = "NAME")
acs14var = merge(acs14var, pop14, by = "NAME")
acs14var = merge(acs14var, povr14, by = "NAME")
acs14var = merge(acs14var, age45a14, by = "NAME")

stroke14 = merge(stroke14, acs14var, by.x = "Zipcode", by.y = "NAME", all.x = T)

#Row bind stroke12, stroke13, stroke14, and stroke15 together. 
strokef = rbind(stroke11, stroke12, stroke13, stroke14)
strokef = strokef[, c(3, 2, 4, 11, 12, 1, 5, 14, 15, 6, 8, 9, 7, 13, 10, 16, 17, 19, 18, 23, 22, 21, 20)]

#Separate strokef data into readmissions and mortality data frames. Separate models will be run and analyzed for each data frame.
readmissions = strokef[grepl("Readmission", strokef$Measure), ]
mortality = strokef[grepl("Mortality", strokef$Measure), ]

#Export strokef, readmissions, and mortality to excel.
write_xlsx(strokef, "C:/Users/alexm/OneDrive/Desktop/UCLA Data Science Certification/Capstone Project/strokef.xlsx")
write_xlsx(readmissions, "C:/Users/alexm/OneDrive/Desktop/UCLA Data Science Certification/Capstone Project/readmissions.xlsx")
write_xlsx(mortality, "C:/Users/alexm/OneDrive/Desktop/UCLA Data Science Certification/Capstone Project/mortality.xlsx")

#Exploratory Data Analysis.

#Table counts.
table(strokef$Year)
table(strokef$'Hospital Ratings')

#Make three new data frames subset according to Hospital Rating.
rating_exp = strokef[strokef$`Hospital Ratings` == "As Expected", ]
rating_worse = strokef[strokef$`Hospital Ratings` == "Worse", ]
rating_better = strokef[strokef$`Hospital Ratings` == "Better", ]

#Visualize Hospital Ratings. 
p1 = strokef %>% group_by(`Hospital Ratings`) %>% summarise(count = n()) %>% 
mutate(percent = prop.table(count) * 100) %>% ggplot(aes(x = factor(`Hospital Ratings`, levels = rev(factor(`Hospital Ratings`))), percent), fill = `Hospital Ratings`) +
geom_col(fill = c('purple', 'pink', 'light blue')) + geom_text(aes(label = sprintf("%.1f%%", percent)), hjust = 0.5, vjust = 1, size = 5) + theme_bw() +  
xlab("Hospital Ratings") + ylab("Percent") + ggtitle("Hospital Ratings")

p2 = readmissions %>% group_by(`Hospital Ratings`) %>% summarise(count = n()) %>% 
  mutate(percent = prop.table(count) * 100) %>% ggplot(aes(reorder(`Hospital Ratings`, -percent), percent), fill = `Hospital Ratings`) +
  geom_col(fill = c('pink', 'light blue', 'purple')) + geom_text(aes(label = sprintf("%.1f%%", percent)), hjust = 0.2, vjust = 1, size = 5) + theme_bw() +  
  xlab("Hospital Ratings") + ylab("Percent") + ggtitle("Hospital Ratings (Readmissions)")

p3 = mortality %>% group_by(`Hospital Ratings`) %>% summarise(count = n()) %>% 
  mutate(percent = prop.table(count) * 100) %>% ggplot(aes(reorder(`Hospital Ratings`, -percent), percent), fill = `Hospital Ratings`) +
  geom_col(fill = c('pink', 'light blue', 'purple')) + geom_text(aes(label = sprintf("%.1f%%", percent)), hjust = 0.2, vjust = 1, size = 5) + theme_bw() +  
  xlab("Hospital Ratings") + ylab("Percent") + ggtitle("Hospital Ratings (Mortality)")

#Mean Risk Adjusted Rate: Note - risk adjusted rate adjusts the Percentage of 30 Day Readmissions/Deaths statistics.
#This statistical methodology takes into account pre-existing health problems that put some patients at greater risk 
#of death/readmission to “level the playing field” and allow fair comparisons across hospitals. 
med_RARe = median(rating_exp$`Risk Adjusted Rate`) #Mean risk adjusted rate is 11.07.
med_RARw = median(rating_worse$`Risk Adjusted Rate`) #Mean risk adjusted rate is 18.24.
med_RARb = median(rating_better$`Risk Adjusted Rate`) #Mean risk adjusted rate is 5.75.
med_RAR_lab = c("med_RARe", "med_RARw","med_RARb")
med_RAR_val = c(med_RARe, med_RARw, med_RARb)
med_RAR = data.frame(med_RAR_lab, med_RAR_val)
p4 = ggplot(med_RAR, aes(med_RAR_lab, med_RAR_val)) + geom_bar(stat = "identity", fill = c("pink", "purple", "light blue")) +
geom_text(aes(label = round(med_RAR_val, 2)), hjust = 0.5, vjust = 1, size = 5) + theme_bw() +  
scale_x_discrete(labels = c("Better", "As Expected", "Worse")) + xlab("Hospital Rating") + 
ylab("Median Risk Adjusted Rate") + ggtitle("Median Risk Adjusted Rates by Hospital Rating")

#Plot p1-p4 side by side.
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

#Visualize change in Risk Adjusted Rate for each hospital over 2011-2015 period for both readmissions and mortality data frames.
p5 = boxplot(readmissions$`Risk Adjusted Rate` ~ readmissions$Year)
p6 = boxplot(mortality$`Risk Adjusted Rate` ~ mortality$Year)
#For both data frames, median RAR decreases for 2011-2012, 2012-2013, and 2014-14, and then they slightly go up from 2014-2015.

#Find outliers in Risk Adjusted Rates by Year visualization and subset them from readmissions data frame in order to label them on visualization.
p5$out
p6$out

readmissions_out = readmissions[readmissions$`Risk Adjusted Rate` %in% p5$out, ]
mortality_out = mortality[mortality$`Risk Adjusted Rate` %in% p6$out, ]
nrow(readmissions_out) #There are 36 outliers in the readmissions data frame.
nrow(mortality_out) #There are 46 outliers in the mortality data frame.

readmissions_out_a = readmissions_out[readmissions_out$`Risk Adjusted Rate` > 11.5,]
readmissions_out_b = readmissions_out[readmissions_out$`Risk Adjusted Rate` < 11.5,]
mortality_out_a = mortality_out[mortality_out$`Risk Adjusted Rate` > 9.5,]
mortality_out_b = mortality_out[mortality_out$`Risk Adjusted Rate` < 9.5,]

table(readmissions_out_a$Hospital) #Greater El Monte Community, Loma Linda Murrieta, Mad River Community, Pacifica Hospital of the Valley,
#and Saint Rose Hospital are in high RAR outliers twice.
table(readmissions_out_b$Hospital) #Fairchild Medical Center, Frank R. Howard Memorial, and Marian Regional Medical Center - Arroyo Grande are
#in low RAR outliers twice. Sonoma Valley is in low outliers three times. 
table(mortality_out_a$Hospital) #Kaiser Foundation Hospital - Panorama City and San Francisco General Hospital are in high RAR outliers twice. LA County/USC
#Medical Center, Madera Community, and Ridgecrest Regional are in high RAR outliers three times. San Joaquin General is in high RAR outliers four times.
table(mortality_out_b$Hospital) #Kern Medical, Natividad Medical, and Sherman Oaks are in low RAR outliers twice.

nrow(readmissions_out_a) #There are 18 outliers above the median in the readmissions data frame (high RAR).
nrow(readmissions_out_b) #There are 18 outliers below the median in the readmissions data frame (low RAR).
nrow(mortality_out_a) #There are 32 outliers above the median in the mortality data frame (high RAR).
nrow(mortality_out_b) #There are 14 outliers below the median in the mortality data frame (low RAR).

p7 = ggplot(readmissions, aes(x = Year, y = `Risk Adjusted Rate`)) + geom_boxplot(outlier.color = "red") + stat_summary(fun = "median", geom = "point", shape = 20, size = 4, color = "blue") + 
stat_summary(fun = "median", geom = "text", aes(label =   sprintf("Median: %.2f", ..y..)), vjust = -0.9, size = 3) + ggtitle("Risk Adjusted Rates by Year (Readmissions)") + theme_bw() +  
geom_text_repel(data = readmissions_out, label = readmissions_out$Hospital, size = 2.5, max.overlaps = 50)
p7
p8 = ggplot(mortality, aes(x = Year, y = `Risk Adjusted Rate`)) + geom_boxplot(outlier.color = "red") + stat_summary(fun = "median", geom = "point", shape = 20, size = 4, color = "blue") + 
stat_summary(fun = "median", geom = "text", aes(label =   sprintf("Median: %.2f", ..y..)), vjust = -0.9, size = 3) + ggtitle("Risk Adjusted Rates by Year (Mortality)") + theme_bw() +  
geom_text_repel(data = mortality_out, label = mortality_out$Hospital, size = 2.5, max.overlaps = 50)
p8

#Modeling: Panel Data Regression Analysis for readmissions and mortality data frames.
eq01 = lm(`Risk Adjusted Rate` ~ `Median Household Income Estimate` + CHCI + `Population Estimate` + `Poverty Rate` + `Percent Age 65-84` + `Percent Age 60 Up` + `Percent Age 55 Up` +
`Percent Age 45 Up` + `Capacity (Number of Beds)` + `Entity Type`, data = readmissions)
summary1 = summary(eq01)
summary1

eq02 = lm(`Risk Adjusted Rate` ~ `Median Household Income Estimate` + CHCI + `Population Estimate` + `Poverty Rate` + `Percent Age 65-84` + `Percent Age 60 Up` + `Percent Age 55 Up` +
            `Percent Age 45 Up` + `Capacity (Number of Beds)` + `Entity Type`, data = mortality)
summary2 = summary(eq02)
summary2

eq01 = lm(`Risk Adjusted Rate` ~ `Median Household Income Estimate` + CHCI + `Population Estimate` + `Poverty Rate` + `Percent Age 65-84` + `Percent Age 60 Up` + `Percent Age 55 Up` +
            `Percent Age 45 Up` + `Capacity (Number of Beds)` + `Entity Type`, data = readmissions)
summary1 = summary(eq01)
summary1

eq03 = lm(`Risk Adjusted Rate` ~ `Median Household Income Estimate` + CHCI + `Population Estimate` + `Poverty Rate` + `Percent Age 65-84` + `Percent Age 60 Up` + `Percent Age 55 Up` +
            `Percent Age 45 Up` + `Capacity (Number of Beds)` + `Entity Type` + factor(County), data = readmissions)
summary3 = summary(eq03)
summary3

eq04 = lm(`Risk Adjusted Rate` ~ `Median Household Income Estimate` + CHCI + `Population Estimate` + `Poverty Rate` + `Percent Age 65-84` + `Percent Age 60 Up` + `Percent Age 55 Up` +
            `Percent Age 45 Up` + `Capacity (Number of Beds)` + `Entity Type` + factor(County), data = mortality)
summary4 = summary(eq04)
summary4

eq05 = lm(`Risk Adjusted Rate` ~ `Median Household Income Estimate` + CHCI + `Population Estimate` + `Poverty Rate` + `Percent Age 65-84` + `Percent Age 60 Up` + `Percent Age 55 Up` +
            `Percent Age 45 Up` + `Capacity (Number of Beds)` + `Entity Type` + factor(Hospital), data = readmissions)
summary5 = summary(eq05)
summary5

eq06 = lm(`Risk Adjusted Rate` ~ `Median Household Income Estimate` + CHCI + `Population Estimate` + `Poverty Rate` + `Percent Age 65-84` + `Percent Age 60 Up` + `Percent Age 55 Up` +
            `Percent Age 45 Up` + `Capacity (Number of Beds)` + `Entity Type` + factor(Hospital), data = mortality)
summary6 = summary(eq06)
summary6


eq07 = lm(`Risk Adjusted Rate` ~ `Median Household Income Estimate` + CHCI + `Population Estimate` + `Poverty Rate` + `Percent Age 65-84` + `Percent Age 60 Up` + `Percent Age 55 Up` +
            `Percent Age 45 Up` + `Capacity (Number of Beds)` + `Entity Type` + factor(`Business Name`), data = readmissions)
summary7 = summary(eq07)
summary7

eq08 = lm(`Risk Adjusted Rate` ~ `Median Household Income Estimate` + CHCI + `Population Estimate` + `Poverty Rate` + `Percent Age 65-84` + `Percent Age 60 Up` + `Percent Age 55 Up` +
            `Percent Age 45 Up` + `Capacity (Number of Beds)` + `Entity Type` + factor(`Business Name`), data = mortality)
summary8 = summary(eq08)
summary8

eq09 = lm(`Risk Adjusted Rate` ~ `Median Household Income Estimate` + CHCI + `Population Estimate` + `Poverty Rate` + `Percent Age 65-84` + `Percent Age 60 Up` + `Percent Age 55 Up` +
            `Percent Age 45 Up` + `Capacity (Number of Beds)` + `Entity Type` + factor(Hospital) + factor(County), data = readmissions)
summary9 = summary(eq09)
summary9

eq10 = lm(`Risk Adjusted Rate` ~ `Median Household Income Estimate` + CHCI + `Population Estimate` + `Poverty Rate` + `Percent Age 65-84` + `Percent Age 60 Up` + `Percent Age 55 Up` +
            `Percent Age 45 Up` + `Capacity (Number of Beds)` + `Entity Type` + factor(Hospital) + factor(County), data = mortality)
summary10 = summary(eq10)
summary10

eq11 = lm(`Risk Adjusted Rate` ~ `Median Household Income Estimate` + CHCI + `Population Estimate` + `Poverty Rate` + `Percent Age 65-84` + `Percent Age 60 Up` + `Percent Age 55 Up` +
            `Percent Age 45 Up` + `Capacity (Number of Beds)` + `Entity Type` + factor(Hospital) + factor(County) + factor(Year), data = readmissions)
summary11 = summary(eq11)
summary11

eq12 = lm(`Risk Adjusted Rate` ~ `Median Household Income Estimate` + CHCI + `Population Estimate` + `Poverty Rate` + `Percent Age 65-84` + `Percent Age 60 Up` + `Percent Age 55 Up` +
            `Percent Age 45 Up` + `Capacity (Number of Beds)` + `Entity Type` + factor(Hospital) + factor(County) + factor(Year), data = mortality)
summary12 = summary(eq12)
summary12