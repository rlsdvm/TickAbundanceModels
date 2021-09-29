#1. DEVA and AMAM ticks analysis
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
library(readxl)
Dates20182019 = read_excel("/Users/rlsdvm/Box/R projects/VBD_consulting/Ellie/newdata/CollectSiteDates20182019.xlsx", sheet = 1)






#7.adding in tick data
library(readxl)
TickCO2018 = read_xlsx("/Users/rlsdvm/Box/R projects/VBD_consulting/Ellie/newdata/DermAm_2018.xlsx", sheet = 1)
TickCO2019 = read_xlsx("/Users/rlsdvm/Box/R projects/VBD_consulting/Ellie/newdata/DermAm_2019.xlsx", sheet = 1)
names(TickCO2018)=names(TickCO2019)

TickCO2 = rbind(TickCO2018, TickCO2019)


library(dplyr)
TickDEVA = TickCO2 %>% filter(Species == "DEVA")
TickAMAM = TickCO2 %>% filter(Species == "AMAM")

TotalDates = Dates20182019 [!duplicated(Dates20182019[c(1,2)]),]
TickDEVA = TickDEVA[-2]


TickAMAM = TickAMAM[-2]

# 8. adding year and month columns to tick data
TickDEVA$Month = substr(TickDEVA$Date_Found, start = 6, stop = 7)
TickDEVA$Year = substr(TickDEVA$Date_Found, start = 1, stop = 4)

TickAMAM$Month = substr(TickAMAM$Date_Found, start = 6, stop = 7)
TickAMAM$Year = substr(TickAMAM$Date_Found, start = 1, stop = 4)

#9 adding region and time frame columns to tick data
TickDEVA$Region = ifelse(TickDEVA$County %in% c("McHenry", "Lake", "Cook", "DuPage"), "North",
                         ifelse(TickDEVA$County %in% c("Edwards", "Hamilton", "Perry", "Jefferson", "Jackson", "Alexander", "Clinton", "Franklin", "Gallatin", "Hardin", "Johnson", "Massac", "Monroe", "Pope", "Pulaski", "Randolph", "Saline", "St Clair", "Union", "Wabash", "Washington", "Wayne", "White", "Williamson"), "South", "Central"))      

TickAMAM$Region = ifelse(TickAMAM$County %in% c("McHenry", "Lake", "Cook", "DuPage"), "North",
                         ifelse(TickAMAM$County %in% c("Edwards", "Hamilton", "Perry", "Jefferson", "Jackson", "Alexander", "Clinton", "Franklin", "Gallatin", "Hardin", "Johnson", "Massac", "Monroe", "Pope", "Pulaski", "Randolph", "Saline", "St Clair", "Union", "Wabash", "Washington", "Wayne", "White", "Williamson"), "South", "Central"))      

#data collected between April and May being grouped as late spring, data collected between June and August grouped as summer, and data collected between September and November grouped as fall. 
library(lubridate)
TickDEVA$TimeFrame = ifelse(TickDEVA$Month %in% c("04", "05"), "Late Spring",
                            ifelse (TickDEVA$Month %in% c("06", "07", "08"), "Summer", "Fall"))
TickAMAM$TimeFrame = ifelse(TickAMAM$Month %in% c("04", "05"), "Late Spring",
                            ifelse (TickAMAM$Month %in% c("06", "07", "08"), "Summer", "Fall"))


#10. making seperate adults and nymphs sheets
TickDEVAadult = TickDEVA %>% filter(Life_Stage == "Adult")
TickDEVAnymph = TickDEVA %>% filter(Life_Stage == "Nymph")

TickAMAMadult = TickAMAM %>% filter(Life_Stage == "Adult")
TickAMAMnymph = TickAMAM %>% filter(Life_Stage == "Nymph")


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



#12. merge weather data with DEVA and AMAM tick data
TWDEVAadult = merge(TickDEVAadult, W2combo11, by = c("County", "Year", "Month"))
TWDEVAnymph = merge(TickDEVAnymph, W2combo11, by = c("County", "Year", "Month"))
TWAMAMadult = merge(TickAMAMadult, W2combo11, by = c("County", "Year", "Month"))
TWAMAMnymph = merge(TickAMAMnymph, W2combo11, by = c("County", "Year", "Month"))



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
TickCODEVAadult = dplyr:: bind_rows(TWDEVAadult, WTotalDates)
TickCODEVAnymph = dplyr:: bind_rows(TWDEVAnymph, WTotalDates)
TickCOAMAMadult = dplyr:: bind_rows(TWAMAMadult, WTotalDates)
TickCOAMAMnymph = dplyr:: bind_rows(TWAMAMnymph, WTotalDates)
#getting rid of duplicates
TickCODEVAadult = TickCODEVAadult [!duplicated(TickCODEVAadult[c(1,6)]),]
TickCODEVAnymph = TickCODEVAnymph [!duplicated(TickCODEVAnymph[c(1,6)]),]
TickCOAMAMadult = TickCOAMAMadult [!duplicated(TickCOAMAMadult[c(1,6)]),]
TickCOAMAMnymph = TickCOAMAMnymph [!duplicated(TickCOAMAMnymph[c(1,6)]),]

#17. changing NAs to 0s
TickCODEVAadult$Quantity_inVial[is.na(TickCODEVAadult$Quantity_inVial)] = 0 
TickCODEVAnymph$Quantity_inVial[is.na(TickCODEVAnymph$Quantity_inVial)] = 0 
TickCOAMAMadult$Quantity_inVial[is.na(TickCOAMAMadult$Quantity_inVial)] = 0 
TickCOAMAMnymph$Quantity_inVial[is.na(TickCOAMAMnymph$Quantity_inVial)] = 0 


#18. changing column names and getting rid of Life-Stage column
colnames(TickCODEVAadult)[5] = "Daily_Total_Number_of_Adults"
TickCODEVAadult = TickCODEVAadult[-4]

colnames(TickCODEVAnymph)[5] = "Daily_Total_Number_of_Nymphs"
TickCODEVAnymph = TickCODEVAnymph[-4]

colnames(TickCOAMAMadult)[5] = "Daily_Total_Number_of_Adults"
TickCOAMAMadult = TickCOAMAMadult[-4]

colnames(TickCOAMAMnymph)[5] = "Daily_Total_Number_of_Nymphs"
TickCOAMAMnymph = TickCOAMAMnymph[-4]

#19. getting adults and nymphs back into combined data sheets

TickCODEVAall = merge(TickCODEVAadult, TickCODEVAnymph, by = c("County", "Year", "Month", "Date_Found", "Region", "TimeFrame", "Monthly_Total_Precipitation", "Average_Daily_Mean_Temperature_per_Month", "Average_Daily_Min_Temperature_per_Month", "Average_Daily_Max_Temperature_per_Month"),all = T)
TickCODEVAall$Daily_Total_Number_of_Adults[is.na(TickCODEVAall$Daily_Total_Number_of_Adults)]=0
TickCODEVAall$Daily_Total_Number_of_Nymphs[is.na(TickCODEVAall$Daily_Total_Number_of_Nymphs)]=0
TickCODEVAall$PercentRelativeHumidity.x[is.na(TickCODEVAall$PercentRelativeHumidity.x)]=TickCODEVAall$PercentRelativeHumidity.y[is.na(TickCODEVAall$PercentRelativeHumidity.x)]
TickCODEVAall$PercentRelativeHumidity2.x[is.na(TickCODEVAall$PercentRelativeHumidity2.x)]=TickCODEVAall$PercentRelativeHumidity2.y[is.na(TickCODEVAall$PercentRelativeHumidity2.x)]
TickCODEVAall$DewPoint.x[is.na(TickCODEVAall$DewPoint.x)]=TickCODEVAall$DewPoint.y[is.na(TickCODEVAall$DewPoint.x)]
TickCODEVAall$AverageMaxVaporPressure.x[is.na(TickCODEVAall$AverageMaxVaporPressure.x)]=TickCODEVAall$AverageMaxVaporPressure.y[is.na(TickCODEVAall$AverageMaxVaporPressure.x)]
TickCODEVAall$AverageMinVaporPressure.x[is.na(TickCODEVAall$AverageMinVaporPressure.x)]=TickCODEVAall$AverageMinVaporPressure.y[is.na(TickCODEVAall$AverageMinVaporPressure.x)]


TickCOAMAMall = merge(TickCOAMAMadult, TickCOAMAMnymph, by = c("County", "Year", "Month", "Date_Found", "Region", "TimeFrame", "Monthly_Total_Precipitation", "Average_Daily_Mean_Temperature_per_Month", "Average_Daily_Min_Temperature_per_Month", "Average_Daily_Max_Temperature_per_Month"),all = T)
TickCOAMAMall$Daily_Total_Number_of_Adults[is.na(TickCOAMAMall$Daily_Total_Number_of_Adults)]=0
TickCOAMAMall$Daily_Total_Number_of_Nymphs[is.na(TickCOAMAMall$Daily_Total_Number_of_Nymphs)]=0
TickCOAMAMall$PercentRelativeHumidity.x[is.na(TickCOAMAMall$PercentRelativeHumidity.x)]=TickCOAMAMall$PercentRelativeHumidity.y[is.na(TickCOAMAMall$PercentRelativeHumidity.x)]
TickCOAMAMall$PercentRelativeHumidity2.x[is.na(TickCOAMAMall$PercentRelativeHumidity2.x)]=TickCOAMAMall$PercentRelativeHumidity2.y[is.na(TickCOAMAMall$PercentRelativeHumidity2.x)]
TickCOAMAMall$DewPoint.x[is.na(TickCOAMAMall$DewPoint.x)]=TickCOAMAMall$DewPoint.y[is.na(TickCOAMAMall$DewPoint.x)]
TickCOAMAMall$AverageMaxVaporPressure.x[is.na(TickCOAMAMall$AverageMaxVaporPressure.x)]=TickCOAMAMall$AverageMaxVaporPressure.y[is.na(TickCOAMAMall$AverageMaxVaporPressure.x)]
TickCOAMAMall$AverageMinVaporPressure.x[is.na(TickCOAMAMall$AverageMinVaporPressure.x)]=TickCOAMAMall$AverageMinVaporPressure.y[is.na(TickCOAMAMall$AverageMinVaporPressure.x)]


#20. finding median # of ticks found in each month per county; median daily ____ per month really means avg ___ collected per sampling per month for each county
for(county in TickCODEVAall$County) 
  for(year in TickCODEVAall$Year) {
    for(month in TickCODEVAall$Month) 
      
    {  TickCODEVAalla = subset(TickCODEVAall, subset=(Month == month & Year == year & County == county),)
    TickCODEVAall[which(TickCODEVAall$Month == month & TickCODEVAall$Year == year & TickCODEVAall$County == county), "Median_Daily_Nymphs_per_Month"] = median(as.numeric(TickCODEVAalla$Daily_Total_Number_of_Nymphs))
    
    }
  }
for(county in TickCODEVAall$County) 
  for(year in TickCODEVAall$Year) {
    for(month in TickCODEVAall$Month) 
      
    {  TickCODEVAallb = subset(TickCODEVAall, subset=(Month == month & Year == year & County == county),)
    TickCODEVAall[which(TickCODEVAall$Month == month & TickCODEVAall$Year == year & TickCODEVAall$County == county), "Median_Daily_Adults_per_Month"] = median(as.numeric(TickCODEVAallb$Daily_Total_Number_of_Adults))
    
    }
  }
for(county in TickCOAMAMall$County) 
  for(year in TickCOAMAMall$Year) {
    for(month in TickCOAMAMall$Month) 
      
    {  TickCOAMAMalla = subset(TickCOAMAMall, subset=(Month == month & Year == year & County == county),)
    TickCOAMAMall[which(TickCOAMAMall$Month == month & TickCOAMAMall$Year == year & TickCOAMAMall$County == county), "Median_Daily_Nymphs_per_Month"] = median(as.numeric(TickCOAMAMalla$Daily_Total_Number_of_Nymphs))
    
    }
  }
for(county in TickCOAMAMall$County) 
  for(year in TickCOAMAMall$Year) {
    for(month in TickCOAMAMall$Month) 
      
    {  TickCOAMAMallb = subset(TickCOAMAMall, subset=(Month == month & Year == year & County == county),)
    TickCOAMAMall[which(TickCOAMAMall$Month == month & TickCOAMAMall$Year == year & TickCOAMAMall$County == county), "Median_Daily_Adults_per_Month"] = median(as.numeric(TickCOAMAMallb$Daily_Total_Number_of_Adults))
    
    }
  }


#21. cleaning up data
TickCODEVAall = TickCODEVAall [!duplicated(TickCODEVAall[c(1,2,3)]),]
TickCODEVAall = TickCODEVAall[-4]
TickCODEVAall = TickCODEVAall[-17]
TickCODEVAall = TickCODEVAall[-17]
TickCODEVAall = TickCODEVAall[-17]
TickCODEVAall = TickCODEVAall[-17]
TickCODEVAall = TickCODEVAall[-17]
as.numeric(TickCODEVAall$Median_Daily_Nymphs_per_Month)
TickCODEVAall$Rounded_Median_Daily_Nymphs_per_Month = ceiling(TickCODEVAall$Median_Daily_Nymphs_per_Month)
TickCODEVAall$Rounded_Median_Daily_Adults_per_Month = ceiling(TickCODEVAall$Median_Daily_Adults_per_Month)

TickCOAMAMall = TickCOAMAMall [!duplicated(TickCOAMAMall[c(1,2,3)]),]
TickCOAMAMall = TickCOAMAMall[-4]
TickCOAMAMall = TickCOAMAMall[-17]
TickCOAMAMall = TickCOAMAMall[-17]
TickCOAMAMall = TickCOAMAMall[-17]
TickCOAMAMall = TickCOAMAMall[-17]
TickCOAMAMall = TickCOAMAMall[-17]
TickCOAMAMall$Rounded_Median_Daily_Nymphs_per_Month = ceiling(TickCOAMAMall$Median_Daily_Nymphs_per_Month)
TickCOAMAMall$Rounded_Median_Daily_Adults_per_Month = ceiling(TickCOAMAMall$Median_Daily_Adults_per_Month)

# south was not sampled in late spring so had to filter it out
TickCODEVAall = TickCODEVAall %>% filter(TimeFrame != "Late Spring")
TickCOAMAMall = TickCOAMAMall %>% filter(TimeFrame != "Late Spring")

#made lifecyle timing column
TickCODEVAall$LifeCycle = TickCODEVAall$Rounded_Median_Daily_Nymphs_per_Month - TickCODEVAall$Rounded_Median_Daily_Adults_per_Month
TickCOAMAMall$LifeCycle = TickCOAMAMall$Rounded_Median_Daily_Nymphs_per_Month - TickCOAMAMall$Rounded_Median_Daily_Adults_per_Month


#21. subsetting data into central and south (no north was sampled)

CentralDEVA = TickCODEVAall %>% filter(Region == "Central")
SouthDEVA = TickCODEVAall %>% filter(Region == "South")


CentralAMAM = TickCOAMAMall %>% filter(Region == "Central")
SouthAMAM = TickCOAMAMall %>% filter(Region == "South")

save(CentralDEVA,file="DEVA_data_Central.Rdata")
save(SouthDEVA,file="DEVA_data_South.Rdata")
save(CentralAMAM,file="AMAM_data_Central.Rdata")
save(SouthAMAM,file="AMAM_data_South.Rdata")
save(TickCOAMAMall,file="AMAM_data.Rdata")
save(TickCODEVAall,file="DEVA_data.Rdata")
