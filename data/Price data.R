library(tidyr)  

price_data <- crossing(plastic_type = c("HDPE", "LDPE", "PET", "PP", "PS",
                                        "RHDPE", "RLDPE", "RPET", "RPP", "RHIPS"),
                       TIME_PERIOD = seq.Date(from = as.Date("2015-01-01"), 
                                              to = as.Date("2024-12-01"), 
                                              by = "month")) %>%
  mutate(TIME_PERIOD = format(TIME_PERIOD, "%Y-%m")) 

#price given in euro per tonne

#for HDPE (high density polyethylene)
price_data$price_euro[price_data$plastic_type == "HDPE"] <- c(NA, NA, 1260, 1460, 1630, NA, 1710, 1620, 1510, 1470, 1490, NA,  #2015
                                                               1525,1420, 1425, 1490, 1525, 1505, 1480, 1445, 1445, 1445, 1450, 1420, #2016
                                                               1445, 1500, 1530, 1515, 1470, 1430, 1370, 1375, 1410, NA, 1440, 1390, #2017
                                                               1410, 1420, NA, 1400, 1405, NA, 1470, 1465, 1465, NA, 1465, 1455, #2018
                                                               1385, 1365, 1365, 1400, 1440, 1470, 1385, 1385, 1345, NA, 1370, 1340, #2019
                                                               1345, 1345, NA, 1310, 1230, 1180, 1225, 1280, 1285, 1285, 1280, 1285, #2020
                                                               1345, 1435, 1580, 1760, 1980, 2020, 1970, 1870, 1830, 1800, 1830, 1920, #2021
                                                               1895, 1950, 1950, 2040, 2290, 2210, 2060, 1900, 1780, 1715, 1735, 1735, #2022
                                                               1720, 1660, 1715, 1735, 1695, 1655, 1575, 1530, 1590, 1660, NA, 1620, #2023
                                                               1590, 1700, 1780, 1795, 1765, 1735, 1735, 1765, 1765, 1730, 1715, 1705) #2024

    
#for LDPE (low density polyethylene)
price_data$price_euro[price_data$plastic_type == "LDPE"] <- c(NA, NA, 1355, 1530, 1670, 1760, 1745, 1745, 1595, 1495, 1530, 1575, #2015
                                                               1530, 1450, 1445, 1515, 1550, 1530, 1480, 1455, 1460, 1465, 1460, 1430, #2016
                                                               1470,1515, 1555, 1555, 1495, 1420, 1370, 1375, 1425, NA, 1460, 1430, #2017
                                                               1430, 1450, NA, 1425, 1405, 1395, 1445, 1435, 1425, 1410, 1400, 1390, #2018
                                                               1315, 1290, 1280, 1305, 1345, 1385, 1310, 1310, 1275, NA, 1290, 1260, #2019
                                                               1255, 1265, NA, 1225, 1150, 1105, 1160, 1245, 1245, 1245, 1235, 1245, #2020
                                                               1335, 1485, 1695, 2010, 2260, 2385, 2360, 2270, 2190, 2160, 2175, 2275, #2021
                                                               2245, 2245, 2205, 2335, 2555, 2495, 2325, 2165, 1850, 1955, 1935, 1955, #2022
                                                               1925, 1845, 1905, 1915, 1865, 1795, 1675, 1585, 1675, 1775, 1725, 1695, #2022
                                                               1675, 1820, 1900, 1930, 1900, 1875, 1875, 1925, 1935, 1905, 1895, 1895)#2023
 

#for PET (Polyethylene - trephtalene)
price_data$price_euro[price_data$plastic_type == "PET"] <- c(NA, NA, 1180, 1240, 1305, 1275, 1225, 1165, 1110, 1110, 1110, 1105, #2015
                                                              1070, 1035, 1055, 1095, 1100, 1090, 1075, 1060, 9900, 1000, 1010,  1040, #2016
                                                              1100, 1170, 1200, 1120, 1060, 1065, 1085, 1130, 1175, NA, 1155, 1125, #2017
                                                              1125, 1135, 1155, 1645, 1245, NA, 1395, 1395, 1450, NA, 1380, 1310, #2018
                                                              1285, 1280, 1275, 1290, 1270, 1250, 1175, 1155, 1170, NA, 1105, 1185, #2019
                                                              1060, 1065, NA, 1030, 985, 955, 960,975, 970, 920, 930, 930, #2020
                                                              950, 1020, 1120, 1300, 1400, 1385, 1345, 1335, 1375, 1400, 1565, 1685, #2021
                                                              1675, 1675, 1690, 1825, 1850, 1760, 1800, 1900, 1700, 1615, 1530, 1470, #2022
                                                              1450, 1360, 1290, 1290, 1290, 1235, 1175, 1175, 1195, 1230, 1210, 1195, #2023
                                                              1280, 1260, 1290, 1265, 1275, 1290, 1310, 1290, 1260, 1160, 1120, 1110) #2024
# for PP (polypropylene)
price_data$price_euro[price_data$plastic_type == "PP"] <- c(NA, NA, 1355, 1470, 1620, 1665, 1635, 1565, 1420, 1300, 1300, 1315, #2015
                                                              1255, 1200, 1205, 1275, 1285, 1295, 1285, 1285, 1300, 1315, 1325, 1300, #2016
                                                              1330, 1380, 1435, 1445, 1430, 1380, 1330, 1335, 1375, NA, 1405, 1390, #2017
                                                              1400, 1445, NA, 1420, 1425, 1435, 1485, 1485, 1485, 1495, 1490, 1490, #2018
                                                              1430, 1415, 1405, 1420, 1445, 1475, 1395, 1395, NA, NA, 1360, 1320, #2019
                                                              1320, 1320, NA, 1225, 1180, 1115, 1165, 1225, 1250, 1250, 1240, 1240, #2020
                                                              1290, 1405, 1615, 1890, 2165, 2215, 2235, 2175, 2095, 2165, 2165, 2255, #2021
                                                              2250, 2250, 2245, 2345, 2545, 2480, 2280, 2120, 2000, 1825, 1800, 1820, #2022
                                                              1800, 1720, 1780, 1800, 1755, 1715, 1605, 1530, 1595, 1655, 1605, 1575, #2023
                                                              1565, 1710, 1770, 1780, 1750, 1720, 1720, 1740, 1720, 1680, 1670, 1645) # 2024
                                                        

# for PS (polystyrene)
price_data$price_euro[price_data$plastic_type == "PS"] <- c(NA, NA, 1670, 1965, 1940, 1955, 1865, 1840, 1740, 1580, 1590, 1615, #2015
                                                                1620, 1580, 1665, 1765, 1770, 1790, 1665, 1665, 1660, 1620, 1630, 1770, #2016
                                                                1870,  2110, 2195, 1960, 1760, 1790, 1820, 1805, 1995, 1880, 1795, 1885, #2017
                                                                1895, 2010, NA, 2040, 1920, 1880, 1930, 1890, 1960, NA, 1895, 1760, #2018
                                                                1640, 1640, 1665, 1720, 1805, 1825, 1640, 1665, 1720, 1690, 1615, 1575, # 2019
                                                                1635, 1685, NA, 1570, 1340, 1320, 1370, 1435, 1435, 1430, 1420, 1490, # 2020
                                                                1650, 1750, 1780, 2280, 1580, 2665, 2315, 2055, 2075, 2040, 2070, 2340, #2021
                                                                2360, 2485, 2405, 2505, 2920, 2970, 2950, 3075, 2575, 2375, 2320, 2270, #2022
                                                                2170, 2270, 2260, 2170, 2180, 2200, 2070, 2020, 2020, 2280, 2140, 2020, #2023
                                                                2010, 2210, 2410, 2450, 2360, 2320, 2185, 2260, 2250, 2050, 2050, 2040) #2024


# for R-HDPE
price_data$price_euro[price_data$plastic_type == "RHDPE"] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2015
                                                                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2016
                                                                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2017
                                                                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2018
                                                                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2019
                                                                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2020
                                                                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 990, #2021
                                                                 990, 990, 1040, 1090, 1140, 1190, 1240, 1260, 1260, 1210, 1130, 1110, #2022
                                                                 1060, 1060, 1030, 990, 990, 910, 910, 845, 850, 870, 870, 870, #2023
                                                                 870, 880, 890, 910, 910, 920, 910, 910, 910, 910, 900, 900) # 2024

#for R-LDPE
price_data$price_euro[price_data$plastic_type == "RLDPE"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2015
                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2016
                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2017
                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2018
                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2019
                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2020
                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1385, #2021
                                                                  1445, 1435, 1465, 1565, 1665, 1695, 1755, 1785, 1770, 1670, 1610, 1610, #2022
                                                                  1535, 1520, 1480, 1430, 1410, 1390, 1330, 1230, 1230, 1240, 1210, 1190, #2023
                                                                  1190, 1255, 1295, 1315, 1350, 1320, 1320, 1330, 1320, 1320, 1310, 1300) #2024

#for R-PET
price_data$price_euro[price_data$plastic_type == "RPET"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2015
                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2016
                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2017
                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2018
                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2019
                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2020
                                                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 825, #2021
                                                                  825, 855, 880, 920, 970, 1070, 1170, 1270, 1290, 1260, 1230, 1200, #2022
                                                                  1100, 1040, 970, 930, 880, 830, 780, 680, 680, 655, 650, 640, #2023
                                                                  660, 705, 735, 785, 795, 785, 775, 775, 775, 765, 735, 720) #2024

#for R-PP
price_data$price_euro[price_data$plastic_type == "RPP"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2015
                                                                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2016
                                                                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2017
                                                                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2018
                                                                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2019
                                                                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2020
                                                                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1135, #2021
                                                                 1140, 1140, 1165, 1360, 1385, 1420, 1400, 1400, 1380, 1320, 1260, 1230, #2022
                                                                 1220, 1195, 1150, 1160, 1160, 1130, 1060, 960, 930, 915, 910, 915, #2023
                                                                 910, 940, 960, 970, 980, 960, 970, 970, 960, 960, 940, 930) # 2024

#for R-HIPS (high impact poly-styrene)    
price_data$price_euro[price_data$plastic_type == "RHIPS"] <-  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2015
                                                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2016
                                                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2017
                                                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2018
                                                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2019
                                                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, #2020
                                                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1120, #2021
                                                               1095, 1095, 1140, 1240, 1290, 1340, 1410, 1440, 1440, 1340, 1310, 1290, #2022
                                                               1240, 1240, 1230, 1240, 1220, 1190, 1160, 1110, 1060, 1060, 1050, 1050, #2023
                                                               1060, 1090, 1140, 1170, 1180, 1150, 1135, 1135, 1115, 1095, 1075, 1075) # 2024


# calculate yearly averages

# Compute yearly averages for each plastic type disregarding missing values

yearly_avg_prices <- price_data %>%
  mutate(TIME_PERIOD = substr(TIME_PERIOD, 1, 4)) %>% 
  group_by(plastic_type, TIME_PERIOD) %>%
  summarise(avg_price = mean(price_euro, na.rm = TRUE), .groups = "drop", na.rm = TRUE)

yearly_avg_prices <- yearly_avg_prices %>%
  pivot_wider(names_from = plastic_type, values_from = avg_price)

yearly_avg_prices <- yearly_avg_prices %>%
  mutate(TIME_PERIOD = as.integer(TIME_PERIOD))

price_data <- price_data %>%
  select(TIME_PERIOD,plastic_type, price_euro) %>%  
  pivot_wider(names_from = plastic_type, values_from = price_euro) 

##majority of plastics are produced from ethylene and propylene. These two feedstocks are particularly critical in the production of plastic packaging. 
##Approximately 40% of plastic use in Europe is used for Plastic packaging. Moreover plastic packaging is comprise nearly exclusively of the five major 
##thermoplastics PP, PET, polyethylene. natural gas liquids is the preferred input for ethylene production. Ethylene can also be produced from naphta. 
## Olefins are the basic chemical building blocks for a huge number of petrochemicals and pertochemical products. ethylene and propylene are the most important olefins.
##Source:https://www.ciel.org/wp-content/uploads/2017/09/Fueling-Plastics-Fossils-Plastics-Petrochemical-Feedstocks.pdf

# correlate prices of primary with secondary plastic --> 0.591

#HDPE and RHDPE --> 0.591
price_data %>%
  filter(TIME_PERIOD >= 2021-03) %>%
  summarise(correlation = cor(HDPE, RHDPE, use = "complete.obs"))

#LDPE and RLDPE --> 0.645
price_data %>%
  filter(TIME_PERIOD >= 2021-03) %>%
  summarise(correlation = cor(LDPE, RLDPE, use = "complete.obs"))

#PET and RPET --> 0.687
price_data %>%
  filter(TIME_PERIOD >= 2021-03) %>%
  summarise(correlation = cor(PET, RPET, use = "complete.obs"))

#PP and RPP --> 0.754
price_data %>%
  filter(TIME_PERIOD >= 2021-03) %>%
  summarise(correlation = cor(PP, RPP, use = "complete.obs"))

#PS and RHIPS --> 0.727
price_data %>%
  filter(TIME_PERIOD >= 2021-03) %>%
  summarise(correlation = cor(PS, RHIPS, use = "complete.obs"))

#load monthly production data to estimate cross price elasticities of supply with prices of secondary plastic. 
# The EU27 are an average of Germany, France, Spain, Greece, Italy and Lithuania. Problem is that I would only have datapoints from 2022-2024, 
# because production data is only collected until Dec. 2023. So only 24 observations.

monthly_production_index <- read.csv("estat_sts_inpr_m_plasticproducts_percentchange.csv", header = TRUE)

monthly_production_index <- monthly_production_index %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                                                 "freq", "indic_bt", "nace_r2", 
                                                                 "s_adj",  "unit", "CONF_STATUS"))

monthly_production_index  <- monthly_production_index  %>% rename("production_change" = "OBS_VALUE", 
                                                                "OBS_FLAG_prodchange" = "OBS_FLAG")

merged_price_data <- merge(price_data, monthly_production_percent, by = "TIME_PERIOD")

#correlate --> -0.1164
correlation_price_production <- merged_price_data %>%
  filter(TIME_PERIOD >= 2022) %>% 
  summarise(correlation = cor(RHDPE, production_change, use = "complete.obs"))

##regress. cannot add covariates because these are not collected on monthly basis. --> horrible fits. all highly insignificant.
model1_prices <- lm(production_change ~ log(RHDPE), data = merged_price_data, na.action = na.exclude)
summary(model1_prices)

model2_prices <- lm(production_change ~ log(RHIPS), data = merged_price_data, na.action = na.exclude)
summary(model2_prices)

model3_prices <- lm(production_change ~ log(RPET), data = merged_price_data, na.action = na.exclude)
summary(model3_prices)







