library(MuMIn)
library(pscl)
library(MASS)
load("ISCAP_data_Central.Rdata")
load("ISCAP_data_South.Rdata")
load("DEVA_data_Central.Rdata")
load("DEVA_data_South.Rdata")
load("AMAM_data_Central.Rdata")
load("AMAM_data_South.Rdata")


###ISCAP
#zero inflated poisson for central and nymphs
ISCAPCenNym1.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month, data = CentralISCAP, dist = "poisson")
ISCAPCenNym2.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x, data = CentralISCAP, dist = "poisson")
ISCAPCenNym3.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenNym4.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenNym5.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x, data = CentralISCAP, dist = "poisson")
ISCAPCenNym6.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenNym7.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenNym8.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenNym9.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = CentralISCAP, dist = "poisson")
ISCAPCenNym10.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month + AverageMinVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenNym11.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenNym12.zeropoisson<- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x +AverageMaxVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenNym13.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = CentralISCAP, dist = "poisson")
ISCAPCenNym14.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ Monthly_Total_Precipitation, data = CentralISCAP, dist = "poisson")
ISCAPCenNym15.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ Monthly_Total_Precipitation + Average_Daily_Max_Temperature_per_Month, data = CentralISCAP, dist = "poisson")

ISCAPCenNymTab=model.sel(ISCAPCenNym11.zeropoisson,
                        ISCAPCenNym14.zeropoisson)
write.csv(ISCAPCenNymTab,file="ISCAPCenNym.csv")

#zero inflated neg bin for south and nymphs
ISCAPSouNym1.zeropoisson = zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month, data = SouthISCAP, dist = "poisson")
ISCAPSouNym2.zeropoisson = zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x, data = SouthISCAP, dist = "poisson")
ISCAPSouNym3.zeropoisson = zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouNym4.zeropoisson = zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouNym5.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x, data = SouthISCAP, dist = "poisson")
ISCAPSouNym6.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouNym7.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouNym8.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouNym9.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = SouthISCAP, dist = "poisson")
ISCAPSouNym10.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month + AverageMinVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouNym11.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouNym12.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x + AverageMaxVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouNym13.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = SouthISCAP, dist = "poisson")
ISCAPSouNym14.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ Monthly_Total_Precipitation, data = SouthISCAP, dist = "poisson")
ISCAPSouNym15.zeropoisson <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ Monthly_Total_Precipitation + Average_Daily_Max_Temperature_per_Month, data = SouthISCAP, dist = "poisson")

ISCAPSouNymTab=model.sel(#ISCAPSouNym1.zeropoisson,ISCAPSouNym2.zeropoisson,ISCAPSouNym3.zeropoisson,ISCAPSouNym4.zeropoisson,ISCAPSouNym5.zeropoisson,ISCAPSouNym6.zeropoisson,ISCAPSouNym7.zeropoisson,
                         #ISCAPSouNym8.zeropoisson,ISCAPSouNym9.zeropoisson,ISCAPSouNym10.zeropoisson,
                         ISCAPSouNym11.zeropoisson,#ISCAPSouNym12.zeropoisson,ISCAPSouNym13.zeropoisson,
                         ISCAPSouNym14.zeropoisson#,ISCAPSouNym15.zeropoisson
                         )
write.csv(ISCAPSouNymTab,file="ISCAPSouNym.csv")


# zero infl poission models for central and adults
ISCAPCenAdu1.zeropoisson = hurdle(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month, data = CentralISCAP, dist = "poisson")
ISCAPCenAdu2.zeropoisson = hurdle(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x, data = CentralISCAP, dist = "poisson")
ISCAPCenAdu3.zeropoisson = hurdle(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenAdu4.zeropoisson = hurdle(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenAdu5.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x, data = CentralISCAP, dist = "poisson")
ISCAPCenAdu6.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenAdu7.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenAdu8.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenAdu9.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = CentralISCAP, dist = "poisson")
ISCAPCenAdu10.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month + AverageMinVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenAdu11.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenAdu12.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x +AverageMaxVaporPressure.x, data = CentralISCAP, dist = "poisson")
ISCAPCenAdu13.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = CentralISCAP, dist = "poisson")
ISCAPCenAdu14.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ Monthly_Total_Precipitation, data = CentralISCAP, dist = "poisson")
ISCAPCenAdu15.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ Monthly_Total_Precipitation + Average_Daily_Max_Temperature_per_Month, data = CentralISCAP, dist = "poisson")

ISCAPCenAduTab=model.sel(ISCAPCenAdu1.zeropoisson,ISCAPCenAdu2.zeropoisson,ISCAPCenAdu3.zeropoisson,ISCAPCenAdu4.zeropoisson,ISCAPCenAdu5.zeropoisson,ISCAPCenAdu6.zeropoisson,ISCAPCenAdu7.zeropoisson,
                         ISCAPCenAdu8.zeropoisson,ISCAPCenAdu9.zeropoisson,ISCAPCenAdu10.zeropoisson,ISCAPCenAdu11.zeropoisson,ISCAPCenAdu12.zeropoisson,ISCAPCenAdu13.zeropoisson,
                         ISCAPCenAdu14.zeropoisson,ISCAPCenAdu15.zeropoisson)
write.csv(ISCAPCenAduTab,file="ISCAPCenAdu.csv")

#5 zero inflated poisson models for South and adult
ISCAPSouAdu1.zeropoisson = hurdle(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month, data = SouthISCAP, dist = "poisson")
ISCAPSouAdu2.zeropoisson = hurdle(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x, data = SouthISCAP, dist = "poisson")
ISCAPSouAdu3.zeropoisson = hurdle(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouAdu4.zeropoisson = hurdle(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouAdu5.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x, data = SouthISCAP, dist = "poisson")
ISCAPSouAdu6.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouAdu7.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouAdu8.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouAdu9.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = SouthISCAP, dist = "poisson")
ISCAPSouAdu10.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month + AverageMinVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouAdu11.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouAdu12.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x + AverageMaxVaporPressure.x, data = SouthISCAP, dist = "poisson")
ISCAPSouAdu13.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = SouthISCAP, dist = "poisson")
ISCAPSouAdu14.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ Monthly_Total_Precipitation, data = SouthISCAP, dist = "poisson")
ISCAPSouAdu15.zeropoisson <- hurdle(Rounded_Median_Daily_Adults_per_Month ~ Monthly_Total_Precipitation + Average_Daily_Max_Temperature_per_Month, data = SouthISCAP, dist = "poisson")

ISCAPSouAduTab=model.sel(ISCAPSouAdu1.zeropoisson,ISCAPSouAdu2.zeropoisson,ISCAPSouAdu3.zeropoisson,ISCAPSouAdu5.zeropoisson,ISCAPSouAdu6.zeropoisson,
                         ISCAPSouAdu7.zeropoisson,ISCAPSouAdu8.zeropoisson,ISCAPSouAdu9.zeropoisson,ISCAPSouAdu11.zeropoisson,ISCAPSouAdu12.zeropoisson,ISCAPSouAdu13.zeropoisson,
                         ISCAPSouAdu14.zeropoisson,ISCAPSouAdu15.zeropoisson)
write.csv(ISCAPSouAduTab,file="ISCAPSouAdu.csv")

###DEVA
#2 zero-inflated neg bin models with central and nymphs (DEVA)
DEVACenNym1.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month, data = CentralDEVA, family = "binomial")
DEVACenNym2.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x, data = CentralDEVA, family = "binomial")
DEVACenNym3.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x, data = CentralDEVA, family = "binomial")
DEVACenNym4.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = CentralDEVA, family = "binomial")
DEVACenNym5.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x, data = CentralDEVA, family = "binomial")
DEVACenNym6.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x, data = CentralDEVA, family = "binomial")
DEVACenNym7.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = CentralDEVA, family = "binomial")
DEVACenNym8.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x, data = CentralDEVA, family = "binomial")
DEVACenNym9.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = CentralDEVA, family = "binomial")
DEVACenNym10.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month + AverageMinVaporPressure.x, data = CentralDEVA, family = "binomial")
DEVACenNym11.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x, data = CentralDEVA, family = "binomial")
DEVACenNym12.logistic<- glm(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x +AverageMaxVaporPressure.x, data = CentralDEVA, family = "binomial")
DEVACenNym13.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = CentralDEVA, family = "binomial")
DEVACenNym14.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ Monthly_Total_Precipitation, data = CentralDEVA, family = "binomial")
DEVACenNym15.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ Monthly_Total_Precipitation + Average_Daily_Max_Temperature_per_Month, data = CentralDEVA, family = "binomial")

DEVACenNymTab=model.sel(DEVACenNym1.logistic,DEVACenNym2.logistic,DEVACenNym5.logistic,DEVACenNym6.logistic,DEVACenNym7.logistic,
                        DEVACenNym8.logistic,DEVACenNym11.logistic,DEVACenNym12.logistic,DEVACenNym13.logistic,
                        DEVACenNym14.logistic,DEVACenNym15.logistic)
write.csv(DEVACenNymTab,file="DEVACenNym.csv")

#zero inflated neg bin for south and nymphs (DEVA)
DEVASouNym1.logistic = glm(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month, data = SouthDEVA, family = "binomial")
DEVASouNym2.logistic = glm(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x, data = SouthDEVA, family = "binomial")
DEVASouNym3.logistic = glm(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x, data = SouthDEVA, family = "binomial")
DEVASouNym4.logistic = glm(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = SouthDEVA, family = "binomial")
DEVASouNym5.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x, data = SouthDEVA, family = "binomial")
DEVASouNym6.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x, data = SouthDEVA, family = "binomial")
DEVASouNym7.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = SouthDEVA, family = "binomial")
DEVASouNym8.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x, data = SouthDEVA, family = "binomial")
DEVASouNym9.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = SouthDEVA, family = "binomial")
DEVASouNym10.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month + AverageMinVaporPressure.x, data = SouthDEVA, family = "binomial")
DEVASouNym11.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x, data = SouthDEVA, family = "binomial")
DEVASouNym12.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x + AverageMaxVaporPressure.x, data = SouthDEVA, family = "binomial")
DEVASouNym13.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = SouthDEVA, family = "binomial")
DEVASouNym14.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ Monthly_Total_Precipitation, data = SouthDEVA, family = "binomial")
DEVASouNym15.logistic <- glm(Rounded_Median_Daily_Nymphs_per_Month ~ Monthly_Total_Precipitation + Average_Daily_Max_Temperature_per_Month, data = SouthDEVA, family = "binomial")

DEVASouNymTab=model.sel(DEVASouNym1.logistic,DEVASouNym2.logistic,DEVASouNym5.logistic,DEVASouNym6.logistic,DEVASouNym7.logistic,
                        DEVASouNym8.logistic,DEVASouNym9.logistic,DEVASouNym10.logistic,DEVASouNym11.logistic,DEVASouNym12.logistic,DEVASouNym13.logistic,
                        DEVASouNym14.logistic,DEVASouNym15.logistic)
write.csv(DEVASouNymTab,file="DEVASouNym.csv")

#4. zeroinfl models for central and adults for DEVA
DEVACenAdu1.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month, data = CentralDEVA, dist = "negbin")
DEVACenAdu2.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x, data = CentralDEVA, dist = "negbin")
DEVACenAdu3.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x, data = CentralDEVA, dist = "negbin")
DEVACenAdu4.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = CentralDEVA, dist = "negbin")
DEVACenAdu5.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x, data = CentralDEVA, dist = "negbin")
DEVACenAdu6.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x, data = CentralDEVA, dist = "negbin")
DEVACenAdu7.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = CentralDEVA, dist = "negbin")
DEVACenAdu8.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x, data = CentralDEVA, dist = "negbin")
DEVACenAdu9.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = CentralDEVA, dist = "negbin")
DEVACenAdu10.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month + AverageMinVaporPressure.x, data = CentralDEVA, dist = "negbin")
DEVACenAdu11.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x, data = CentralDEVA, dist = "negbin")
DEVACenAdu12.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x +AverageMaxVaporPressure.x, data = CentralDEVA, dist = "negbin")
DEVACenAdu13.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = CentralDEVA, dist = "negbin")
DEVACenAdu14.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ Monthly_Total_Precipitation, data = CentralDEVA, dist = "negbin")
DEVACenAdu15.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ Monthly_Total_Precipitation + Average_Daily_Max_Temperature_per_Month, data = CentralDEVA, dist = "negbin")

DEVACenAduTab=model.sel(DEVACenAdu1.zeroinfl,DEVACenAdu2.zeroinfl,DEVACenAdu3.zeroinfl,DEVACenAdu5.zeroinfl,DEVACenAdu6.zeroinfl,DEVACenAdu7.zeroinfl,
                        DEVACenAdu8.zeroinfl,DEVACenAdu9.zeroinfl,DEVACenAdu11.zeroinfl,DEVACenAdu12.zeroinfl,DEVACenAdu13.zeroinfl,
                        DEVACenAdu14.zeroinfl,DEVACenAdu15.zeroinfl)
write.csv(DEVACenAduTab,file="DEVACenAdu.csv")

#5 zeroinfl models for South and adult (DEVA)
DEVASouAdu1.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month, data = SouthDEVA, dist = "negbin")
DEVASouAdu2.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x, data = SouthDEVA, dist = "negbin")
DEVASouAdu3.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x, data = SouthDEVA, dist = "negbin")
DEVASouAdu4.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = SouthDEVA, dist = "negbin")
DEVASouAdu5.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x, data = SouthDEVA, dist = "negbin")
DEVASouAdu6.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x, data = SouthDEVA, dist = "negbin")
DEVASouAdu7.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = SouthDEVA, dist = "negbin")
DEVASouAdu8.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x, data = SouthDEVA, dist = "negbin")
DEVASouAdu9.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = SouthDEVA, dist = "negbin")
DEVASouAdu10.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month + AverageMinVaporPressure.x, data = SouthDEVA, dist = "negbin")
DEVASouAdu11.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x, data = SouthDEVA, dist = "negbin")
DEVASouAdu12.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x +AverageMaxVaporPressure.x, data = SouthDEVA, dist = "negbin")
DEVASouAdu13.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = SouthDEVA, dist = "negbin")
DEVASouAdu14.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ Monthly_Total_Precipitation, data = SouthDEVA, dist = "negbin")
DEVASouAdu15.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ Monthly_Total_Precipitation + Average_Daily_Max_Temperature_per_Month, data = SouthDEVA, dist = "negbin")

DEVASouAduTab=model.sel(DEVASouAdu1.zeroinfl,DEVASouAdu2.zeroinfl,DEVASouAdu3.zeroinfl,DEVASouAdu4.zeroinfl,DEVASouAdu5.zeroinfl,DEVASouAdu6.zeroinfl,DEVASouAdu7.zeroinfl,
                        DEVASouAdu8.zeroinfl,DEVASouAdu9.zeroinfl,DEVASouAdu10.zeroinfl,DEVASouAdu11.zeroinfl,DEVASouAdu12.zeroinfl,DEVASouAdu13.zeroinfl,
                        DEVASouAdu14.zeroinfl,DEVASouAdu15.zeroinfl)
write.csv(DEVASouAduTab,file="DEVASouAdu.csv")

###AMAM
#6 zeroinfl models with central and nymphs (AMAM)
AMAMCenNym1.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month, data = CentralAMAM, dist = "negbin")
AMAMCenNym2.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x, data = CentralAMAM, dist = "negbin")
AMAMCenNym3.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenNym4.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenNym5.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x, data = CentralAMAM, dist = "negbin")
AMAMCenNym6.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenNym7.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenNym8.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenNym9.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = CentralAMAM, dist = "negbin")
AMAMCenNym10.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month + AverageMinVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenNym11.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenNym12.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x +AverageMaxVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenNym13.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = CentralAMAM, dist = "negbin")
AMAMCenNym14.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ Monthly_Total_Precipitation, data = CentralAMAM, dist = "negbin")
AMAMCenNym15.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ Monthly_Total_Precipitation + Average_Daily_Max_Temperature_per_Month, data = CentralAMAM, dist = "negbin")

AMAMCenNymTab=model.sel(AMAMCenNym2.zeroinfl,AMAMCenNym3.zeroinfl,AMAMCenNym6.zeroinfl,AMAMCenNym7.zeroinfl,
                        AMAMCenNym8.zeroinfl,AMAMCenNym9.zeroinfl,AMAMCenNym10.zeroinfl,AMAMCenNym11.zeroinfl,AMAMCenNym12.zeroinfl,
                        AMAMCenNym14.zeroinfl,AMAMCenNym15.zeroinfl)
write.csv(AMAMCenNymTab,file="AMAMCenNym.csv")

#7. zeroinfl models with south and nymphs (AMAM)
AMAMSouNym1.zeroinfl = zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month, data = SouthAMAM, dist = "negbin")
AMAMSouNym2.zeroinfl = zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x, data = SouthAMAM, dist = "negbin")
AMAMSouNym3.zeroinfl = zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouNym4.zeroinfl = zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouNym5.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x, data = SouthAMAM, dist = "negbin")
AMAMSouNym6.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouNym7.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouNym8.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouNym9.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = SouthAMAM, dist = "negbin")
AMAMSouNym10.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month + AverageMinVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouNym11.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouNym12.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x +AverageMaxVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouNym13.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ AverageMinVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = SouthAMAM, dist = "negbin")
AMAMSouNym14.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ Monthly_Total_Precipitation, data = SouthAMAM, dist = "negbin")
AMAMSouNym15.zeroinfl <- zeroinfl(Rounded_Median_Daily_Nymphs_per_Month ~ Monthly_Total_Precipitation + Average_Daily_Max_Temperature_per_Month, data = SouthAMAM, dist = "negbin")

AMAMSouNymTab=model.sel(AMAMSouNym1.zeroinfl,AMAMSouNym2.zeroinfl,AMAMSouNym5.zeroinfl,AMAMSouNym6.zeroinfl,AMAMSouNym7.zeroinfl,
                        AMAMSouNym8.zeroinfl,AMAMSouNym9.zeroinfl,AMAMSouNym10.zeroinfl,AMAMSouNym11.zeroinfl,AMAMSouNym12.zeroinfl,AMAMSouNym13.zeroinfl,
                        AMAMSouNym14.zeroinfl,AMAMSouNym15.zeroinfl)
write.csv(AMAMSouNymTab,file="AMAMSouNym.csv")

#8. zeroinfl models for central and adults (AMAM)
AMAMCenAdu1.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month, data = CentralAMAM, dist = "negbin")
AMAMCenAdu2.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x, data = CentralAMAM, dist = "negbin")
AMAMCenAdu3.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenAdu4.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenAdu5.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x, data = CentralAMAM, dist = "negbin")
AMAMCenAdu6.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenAdu7.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenAdu8.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenAdu9.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = CentralAMAM, dist = "negbin")
AMAMCenAdu10.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month + AverageMinVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenAdu11.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenAdu12.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x +AverageMaxVaporPressure.x, data = CentralAMAM, dist = "negbin")
AMAMCenAdu13.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = CentralAMAM, dist = "negbin")
AMAMCenAdu14.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ Monthly_Total_Precipitation, data = CentralAMAM, dist = "negbin")
AMAMCenAdu15.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ Monthly_Total_Precipitation + Average_Daily_Max_Temperature_per_Month, data = CentralAMAM, dist = "negbin")

AMAMCenAduTab=model.sel(AMAMCenAdu1.zeroinfl,AMAMCenAdu2.zeroinfl,AMAMCenAdu5.zeroinfl,AMAMCenAdu6.zeroinfl,
                        AMAMCenAdu8.zeroinfl,AMAMCenAdu11.zeroinfl,AMAMCenAdu12.zeroinfl,
                        AMAMCenAdu14.zeroinfl,AMAMCenAdu15.zeroinfl)
write.csv(AMAMCenAduTab,file="AMAMCenAdu.csv")

#9 zeroinfl models for South and adult (AMAM)
AMAMSouAdu1.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month, data = SouthAMAM, dist = "negbin")
AMAMSouAdu2.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x, data = SouthAMAM, dist = "negbin")
AMAMSouAdu3.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouAdu4.zeroinfl = zeroinfl(Rounded_Median_Daily_Adults_per_Month ~  Average_Daily_Max_Temperature_per_Month + DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouAdu5.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x, data = SouthAMAM, dist = "negbin")
AMAMSouAdu6.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouAdu7.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ DewPoint.x + AverageMaxVaporPressure.x + AverageMinVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouAdu8.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouAdu9.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = SouthAMAM, dist = "negbin")
AMAMSouAdu10.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMaxVaporPressure.x + Average_Daily_Max_Temperature_per_Month + AverageMinVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouAdu11.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouAdu12.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x +AverageMaxVaporPressure.x, data = SouthAMAM, dist = "negbin")
AMAMSouAdu13.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ AverageMinVaporPressure.x + Average_Daily_Max_Temperature_per_Month, data = SouthAMAM, dist = "negbin")
AMAMSouAdu14.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ Monthly_Total_Precipitation, data = SouthAMAM, dist = "negbin")
AMAMSouAdu15.zeroinfl <- zeroinfl(Rounded_Median_Daily_Adults_per_Month ~ Monthly_Total_Precipitation + Average_Daily_Max_Temperature_per_Month, data = SouthAMAM, dist = "negbin")

AMAMSouAduTab=model.sel(AMAMSouAdu1.zeroinfl,AMAMSouAdu2.zeroinfl,AMAMSouAdu3.zeroinfl,AMAMSouAdu4.zeroinfl,AMAMSouAdu5.zeroinfl,AMAMSouAdu6.zeroinfl,AMAMSouAdu7.zeroinfl,
                        AMAMSouAdu8.zeroinfl,AMAMSouAdu9.zeroinfl,AMAMSouAdu10.zeroinfl,AMAMSouAdu11.zeroinfl,AMAMSouAdu12.zeroinfl,AMAMSouAdu13.zeroinfl,
                        AMAMSouAdu14.zeroinfl,AMAMSouAdu15.zeroinfl)
write.csv(AMAMSouAduTab,file="AMAMSouAdu.csv")




