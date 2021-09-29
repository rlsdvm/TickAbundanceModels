###adding in humidity data later on and dew point data and vapor pressure

library(readxl)
library(dplyr)
WHumid1 = read_xlsx("/Users/rlsdvm/Box/R projects/VBD_consulting/Ellie/newdata/HUMIDITY_Data.xlsx", sheet = 7)
WHumid2 = read_xlsx("/Users/rlsdvm/Box/R projects/VBD_consulting/Ellie/newdata/HUMIDITY_Data.xlsx", sheet = 8)
WDewPt = read_xlsx("/Users/rlsdvm/Box/R projects/VBD_consulting/Ellie/newdata/HUMIDITY_Data.xlsx", sheet = 6)
WVPavgmax = read_xlsx("/Users/rlsdvm/Box/R projects/VBD_consulting/Ellie/newdata/VaporPressure.xlsx", sheet = 1)
WVPavgmin = read_xlsx("/Users/rlsdvm/Box/R projects/VBD_consulting/Ellie/newdata/VaporPressure.xlsx", sheet = 2)

library(reshape2)
TWHumid = melt(WHumid1, id.vars = "County")
colnames(TWHumid) = c("County", "WeatherData", "Value")

TWHumid2 = melt(WHumid2, id.vars = "County")
colnames(TWHumid2) = c("County", "WeatherData", "Value")

TWDewpt = melt(WDewPt, id.vars = "County")
colnames(TWDewpt) = c("County", "WeatherData", "Value")

TWVPavgmax = melt(WVPavgmax, id.vars = "County")
colnames(TWVPavgmax) = c("County", "WeatherData", "Value")

TWVPavgmin = melt(WVPavgmin, id.vars = "County")
colnames(TWVPavgmin) = c("County", "WeatherData", "Value")

library(stringr)
PerHum = TWHumid %>% filter(str_detect(WeatherData, "^RH"))
colnames(PerHum)[3] = "PercentRelativeHumidity"
PerHum$Year = substr(PerHum$WeatherData, start = 3, stop = 6)
PerHum$Month = substr(PerHum$WeatherData, start = 7, stop = 8)
PerHum = PerHum[,-2]

PerHum2 = TWHumid2 %>% filter(str_detect(WeatherData, "^RH"))
colnames(PerHum2)[3] = "PercentRelativeHumidity2"
PerHum2$Year = substr(PerHum2$WeatherData, start = 3, stop = 6)
PerHum2$Month = substr(PerHum2$WeatherData, start = 7, stop = 8)
PerHum2 = PerHum2[,-2]

Dewpoint = TWDewpt %>% filter(str_detect(WeatherData, "^DP"))
colnames(Dewpoint)[3] = "DewPoint"
Dewpoint$Year = substr(Dewpoint$WeatherData, start = 3, stop = 6)
Dewpoint$Month = substr(Dewpoint$WeatherData, start = 7, stop = 8)
Dewpoint = Dewpoint[,-2]

VPavgmax = TWVPavgmax %>% filter(str_detect(WeatherData, "^VPmax"))
colnames(VPavgmax)[3] = "AverageMaxVaporPressure"
VPavgmax$Year = substr(VPavgmax$WeatherData, start = 6 , stop = 9)
VPavgmax$Month = substr(VPavgmax$WeatherData, start = 10, stop = 11)
VPavgmax = VPavgmax[,-2]

VPavgmin = TWVPavgmin %>% filter(str_detect(WeatherData, "^VPmin"))
colnames(VPavgmin)[3] = "AverageMinVaporPressure"
VPavgmin$Year = substr(VPavgmin$WeatherData, start = 6 , stop = 9)
VPavgmin$Month = substr(VPavgmin$WeatherData, start = 10, stop = 11)
VPavgmin = VPavgmin[,-2]

#merging humidity data
HumAll = merge(PerHum, PerHum2, by = c("County", "Month", "Year"))
HumAll2 = merge(HumAll, Dewpoint, by = c("County", "Month", "Year"))

#merging vapor pressure data
VPall = merge(VPavgmax, VPavgmin, by = c("County", "Month", "Year"))

# 2. convert lat and long into county for site dates file

library(openxlsx)
Dates20182019 = read_excel("/Users/rlsdvm/Box/R projects/VBD_consulting/Ellie/newdata/CollectSiteDates20182019.xlsx", sheet = 1)
as.numeric(Dates20182019$Date_Found)
Dates20182019$Date_Found <- convertToDate(Dates20182019$Date_Found)



#7. adding in ISCAP tick data
library(readxl)
ISCAP2018 = read_xlsx("/Users/rlsdvm/Box/R projects/VBD_consulting/Ellie/newdata/ISCAP2018.xlsx", sheet = 1)
ISCAP2019 = read_xlsx("/Users/rlsdvm/Box/R projects/VBD_consulting/Ellie/newdata/ISCAP2019.xlsx", sheet = 1)

ISCAPall = rbind(ISCAP2018, ISCAP2019)

library(dplyr)

TotalDates = Dates20182019 [!duplicated(Dates20182019[c(1,2)]),]


# 8. adding year and month columns to tick data
ISCAPall$Month = substr(ISCAPall$Date_Found, start = 6, stop = 7)
ISCAPall$Year = substr(ISCAPall$Date_Found, start = 1, stop = 4)



#9 adding region and time frame columns to tick data
ISCAPall$Region = ifelse(ISCAPall$County %in% c("McHenry", "Lake", "Cook", "DuPage"), "North",
                         ifelse(ISCAPall$County %in% c("Edwards", "Hamilton", "Perry", "Jefferson", "Jackson", "Alexander", "Clinton", "Franklin", "Gallatin", "Hardin", "Johnson", "Massac", "Monroe", "Pope", "Pulaski", "Randolph", "Saline", "St Clair", "Union", "Wabash", "Washington", "Wayne", "White", "Williamson"), "South", "Central"))      


#data collected between April and May being grouped as late spring, data collected between June and August grouped as summer, and data collected between September and November grouped as fall. 
library(lubridate)
ISCAPall$TimeFrame = ifelse(ISCAPall$Month %in% c("04", "05"), "Late Spring",
                            ifelse (ISCAPall$Month %in% c("06", "07", "08"), "Summer", "Fall"))



#10. making seperate adults and nymphs sheets
ISCAPadult = ISCAPall %>% filter(Life_Stage == "Adult")
ISCAPnymph = ISCAPall %>% filter(Life_Stage == "Nymph")




# 11. added in weather data
W2Precip = read_xlsx("/Users/rlsdvm/Box/R projects/VBD_consulting/Ellie/newdata/WeatherData.xlsx", sheet = 1)
W2Tmax = read_xlsx("/Users/rlsdvm/Box/R projects/VBD_consulting/Ellie/newdata/WeatherData.xlsx", sheet = 2)
W2Tmin = read_xlsx("/Users/rlsdvm/Box/R projects/VBD_consulting/Ellie/newdata/WeatherData.xlsx", sheet = 3)
W2Tmean = read_xlsx("/Users/rlsdvm/Box/R projects/VBD_consulting/Ellie/newdata/WeatherData.xlsx", sheet = 4)

##combiningweathersheets!!!!!
W2combo = merge(W2Precip, W2Tmax, by = c("County"))
W2combo2 = merge(W2combo, W2Tmin, by = c("County"))
W2combo3 = merge(W2combo2, W2Tmean, by = c("County"))

library(reshape2)
W2combo4 = melt(W2combo3, id.vars = "County")
colnames(W2combo4) = c("County", "WeatherData", "Value")

#add year and month columns to weather data
library(stringr)
W2combo4$Year = str_sub(W2combo4$WeatherData,-6,-3)
W2combo4$Month = str_sub(W2combo4$WeatherData, -2, -1)

# make weather columns seperate
W2Precip = W2combo4 %>% filter(str_detect(WeatherData, "^P"))
W2Tavg = W2combo4 %>% filter(str_detect(WeatherData, "Tavg"))
W2Tmin= W2combo4 %>% filter(str_detect(WeatherData, "^Tmin"))
W2Tmax = W2combo4 %>% filter(str_detect(WeatherData, "^Tmax"))

#combining filtered sheets
W2combo5 = merge(W2Precip, W2Tavg, by = c("County", "Month", "Year"))
W2combo6 = merge(W2combo5, W2Tmin, by = c("County", "Month", "Year"))
W2combo7 = merge(W2combo6, W2Tmax, by = c("County", "Month", "Year"))

#renaming columns and getting rid of baggage columns
colnames(W2combo7)[5] = "Monthly_Total_Precipitation"
colnames(W2combo7)[7] = "Average_Daily_Mean_Temperature_per_Month"
colnames(W2combo7)[9] = "Average_Daily_Min_Temperature_per_Month"
colnames(W2combo7)[11] = "Average_Daily_Max_Temperature_per_Month"

W2combo8 = W2combo7[,-4]
W2combo9 = W2combo8[,-5]
W2combo10 = W2combo9[, -6]
W2combo10a = merge(W2combo10, HumAll2, by = c("County", "Month", "Year"))
W2combo10b = merge(W2combo10a, VPall, by = c("County", "Month", "Year"))
W2combo11 = W2combo10b[-7]


#12. merge weather data with ISCAP tick data
TWISCAPadult = merge(ISCAPadult, W2combo11, by = c("County", "Year", "Month"))
TWISCAPnymph = merge(ISCAPnymph, W2combo11, by = c("County", "Year", "Month"))




#13. adding year and month to collecting site dates data

TotalDates$Month = substr(TotalDates$Date_Found, start = 6, stop = 7)
TotalDates$Year = substr(TotalDates$Date_Found, start = 1, stop = 4)



#14 adding region and time frame columns to site dates data
TotalDates$Region = ifelse(TotalDates$County %in% c("McHenry", "Lake", "Cook", "DuPage"), "North",
                           ifelse(TotalDates$County %in% c("Edwards", "Hamilton", "Perry", "Jefferson", "Jackson", "Alexander", "Clinton", "Franklin", "Gallatin", "Hardin", "Johnson", "Massac", "Monroe", "Pope", "Pulaski", "Randolph", "Saline", "St Clair", "Union", "Wabash", "Washington", "Wayne", "White", "Williamson"), "South", "Central"))      


#data collected between April and May being grouped as late spring, data collected between June and August grouped as summer, and data collected between September and November grouped as fall. 
library(lubridate)
TotalDates$TimeFrame = ifelse(TotalDates$Month %in% c("04", "05"), "Late Spring",
                              ifelse (TotalDates$Month %in% c("06", "07", "08"), "Summer", "Fall"))

#15 adding weather data to totals site dates data
WTotalDates = merge(TotalDates, W2combo11, by = c("County", "Year", "Month"))


#16. combining tick data with collecting sites dates list 
library(dplyr)
COISCAPadult = dplyr:: bind_rows(TWISCAPadult, WTotalDates)
COISCAPnymph = dplyr:: bind_rows(TWISCAPnymph, WTotalDates)

#getting rid of duplicates
COISCAPadult = COISCAPadult [!duplicated(COISCAPadult[c(1,4)]),]
COISCAPnymph = COISCAPnymph [!duplicated(COISCAPnymph[c(1,4)]),]


#17. changing NAs to 0s
COISCAPadult$Quantity_inVial[is.na(COISCAPadult$Quantity_inVial)] = 0 
COISCAPnymph$Quantity_inVial[is.na(COISCAPnymph$Quantity_inVial)] = 0 



#18. changing column names and getting rid of Life-Stage column
colnames(COISCAPadult)[7] = "Daily_Total_Number_of_Adults"
COISCAPadult = COISCAPadult[-6]

colnames(COISCAPnymph)[7] = "Daily_Total_Number_of_Nymphs"
COISCAPnymph = COISCAPnymph[-6]



#19. getting adults and nymphs back into combined data sheets

COISCAPall = merge(COISCAPadult, COISCAPnymph, 
                   by = c("County", "Year", "Month", "Date_Found", "Region", "TimeFrame", "Monthly_Total_Precipitation", "Average_Daily_Mean_Temperature_per_Month", "Average_Daily_Min_Temperature_per_Month", "Average_Daily_Max_Temperature_per_Month"),
                   all = T)
COISCAPall$Daily_Total_Number_of_Adults[is.na(COISCAPall$Daily_Total_Number_of_Adults)]=0
COISCAPall$Daily_Total_Number_of_Nymphs[is.na(COISCAPall$Daily_Total_Number_of_Nymphs)]=0
COISCAPall$PercentRelativeHumidity.x[is.na(COISCAPall$PercentRelativeHumidity.x)]=COISCAPall$PercentRelativeHumidity.y[is.na(COISCAPall$PercentRelativeHumidity.x)]
COISCAPall$PercentRelativeHumidity2.x[is.na(COISCAPall$PercentRelativeHumidity2.x)]=COISCAPall$PercentRelativeHumidity2.y[is.na(COISCAPall$PercentRelativeHumidity2.x)]
COISCAPall$DewPoint.x[is.na(COISCAPall$DewPoint.x)]=COISCAPall$DewPoint.y[is.na(COISCAPall$DewPoint.x)]
COISCAPall$AverageMaxVaporPressure.x[is.na(COISCAPall$AverageMaxVaporPressure.x)]=COISCAPall$AverageMaxVaporPressure.y[is.na(COISCAPall$AverageMaxVaporPressure.x)]
COISCAPall$AverageMinVaporPressure.x[is.na(COISCAPall$AverageMinVaporPressure.x)]=COISCAPall$AverageMinVaporPressure.y[is.na(COISCAPall$AverageMinVaporPressure.x)]

#20. finding median # of ticks found in each month per county; Avg daily ____ per month really means avg ___ collected per sampling per month for each county
for(county in COISCAPall$County) 
  for(year in COISCAPall$Year) {
    for(month in COISCAPall$Month) 
      
    {  COISCAPalla = subset(COISCAPall, subset=(Month == month & Year == year & County == county),)
    COISCAPall[which(COISCAPall$Month == month & COISCAPall$Year == year & COISCAPall$County == county), "Median_Daily_Nymphs_per_Month"] = median(as.numeric(COISCAPalla$Daily_Total_Number_of_Nymphs))
    
    }
  }
for(county in COISCAPall$County) 
  for(year in COISCAPall$Year) {
    for(month in COISCAPall$Month) 
      
    {  COISCAPallb = subset(COISCAPall, subset=(Month == month & Year == year & County == county),)
    COISCAPall[which(COISCAPall$Month == month & COISCAPall$Year == year & COISCAPall$County == county), "Median_Daily_Adults_per_Month"] = median(as.numeric(COISCAPallb$Daily_Total_Number_of_Adults))
    
    }
  }





#21. cleaning up data
COISCAPall = COISCAPall [!duplicated(COISCAPall[c(1,2,3)]),]
COISCAPall = COISCAPall[-4]
COISCAPall = COISCAPall[-10]
COISCAPall = COISCAPall[-16]
COISCAPall = COISCAPall[-17]
COISCAPall = COISCAPall[-17]
COISCAPall = COISCAPall[-17]
COISCAPall = COISCAPall[-17]
COISCAPall = COISCAPall[-17]
COISCAPall$Rounded_Median_Daily_Nymphs_per_Month = ceiling(COISCAPall$Median_Daily_Nymphs_per_Month)
COISCAPall$Rounded_Median_Daily_Adults_per_Month = ceiling(COISCAPall$Median_Daily_Adults_per_Month)

# south was not sampled in late spring so had to filter it out
COISCAPall = COISCAPall %>% filter(TimeFrame != "Late Spring")


#made lifecyle timing column
COISCAPall$LifeCycle = COISCAPall$Rounded_Median_Daily_Nymphs_per_Month - COISCAPall$Rounded_Median_Daily_Adults_per_Month
#21. subsetting data into central and south (no north was sampled)

CentralISCAP = COISCAPall %>% filter(Region == "Central")
SouthISCAP = COISCAPall %>% filter(Region == "South")


save(COISCAPall,file="ISCAP_data.Rdata")
save(CentralISCAP,file="ISCAP_data_Central.Rdata")
save(SouthISCAP,file="ISCAP_data_South.Rdata")

