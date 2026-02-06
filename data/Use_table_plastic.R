setwd("~/Documents/MPP/Masterthesis/data")
##final dataset: 59 observations

#load use table plastic domestic
use_table_domestic <- read.csv("usetable_domestic_totaluse.csv", header = TRUE) #use tables of plastic products for all industries domestic in mio Euros
use_table_total <- read.csv("usetables_total_totaluse.csv", header = TRUE) #use tables of plastic products for all industries import+domestic in mio Euros
plastic_recycling <- read.csv("estat_env_wastrt_allcountries.csv", header = TRUE) #plastic waste recycling in tonnes
nama_gdp <- read.csv("estat_nama_gdp.csv") #annual GDP in chain linked volumes index = 2015
env_taxes <- read.csv("estat_env_taxes.csv") #annual environmental taxes in million EUR
recycling_rate <- read.csv("estat_env_waspacr_allcountries.csv", header = TRUE) ## plastic packaging recycling rate

#clean data

plastic_recycling <- plastic_recycling %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                        "freq", "unit", "hazard", 
                                        "wst_oper",  "waste", "CONF_STATUS"))

use_table_total <- use_table_total %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                                 "freq", "ind_use", "stk_flow", 
                                                 "prd_ava",  "unit", "CONF_STATUS"))

use_table_domestic <- use_table_domestic %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                                 "freq", "ind_use", "stk_flow", 
                                                 "prd_ava", "unit", "CONF_STATUS"))

recycling_rate <- recycling_rate %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                          "freq", "waste", "unit", "CONF_STATUS"))

plastic_recycling  <- plastic_recycling  %>% rename("recycling_tonne" = "OBS_VALUE", 
                                              "OBS_FLAG_recycling" = "OBS_FLAG")

recycling_rate  <- recycling_rate  %>% rename("recycling_rate" = "OBS_VALUE", 
                                              "OBS_FLAG_rate" = "OBS_FLAG")

use_table_domestic  <- use_table_domestic  %>% rename("plastic_mioEUR_dom" = "OBS_VALUE", 
                                                      "OBS_FLAG_usedom" = "OBS_FLAG")

use_table_total  <- use_table_total  %>% rename("plastic_mioEUR_tot" = "OBS_VALUE", 
                                                "OBS_FLAG_usetot" = "OBS_FLAG")


#clean up gdp data
nama_gdp <- nama_gdp %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                   "freq", "na_item",  
                                   "unit", "CONF_STATUS"))

nama_gdp <- nama_gdp %>% rename("OBS_FLAG_GDP" = "OBS_FLAG", 
                                "GDP_index2015" = "OBS_VALUE")

#clean up environmental taxes data

env_taxes <- env_taxes %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                     "freq", "tax", "nace_r2", 
                                     "unit", "CONF_STATUS"))

env_taxes <- env_taxes %>% rename("OBS_FLAG_envtax" = "OBS_FLAG", 
                                  "env_tax_mioEUR" = "OBS_VALUE")

#merge datasets

merged_usetables <- merge(plastic_recycling, use_table_domestic, by = c("geo", "TIME_PERIOD"))
merged_usetables <- merge(merged_usetables, use_table_total, by = c("geo", "TIME_PERIOD"))
merged_usetables <- merge(merged_usetables, recycling_rate, by = c("geo", "TIME_PERIOD"))

merged_usetables_balanced <- merged_usetables %>%
  filter(!geo %in% c("Belgium", "Bulgaria", "Greece", "Germany", "Ireland", "Latvia", "Lithuania", "Malta", "Montenegro",
                     "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Serbia", "Slovakia", "Slovenia",
                     "Spain", "Sweden", "Türkiye", "United Kingdom", "Cyprus", "European Union - 27 countries (from 2020)"))

merged_usetables_balanced <- merged_usetables_balanced %>%
  filter(!TIME_PERIOD %in% "2022")  
                                                              
merged_usetables_balanced <- merge(merged_usetables_balanced, nama_gdp, by = c("geo", "TIME_PERIOD"))
merged_usetables_balanced <- merge(merged_usetables_balanced, env_taxes, by = c("geo", "TIME_PERIOD"))

# Convert data to panel format with plm
usetables_panel <- pdata.frame(merged_usetables_balanced, index = c("geo","TIME_PERIOD"))

## analysis of correlation

#correlation of all variables
correlation_matrix_usetables <- cor(usetables_panel[sapply(usetables_panel, is.numeric)], use = "pairwise.complete.obs")
print(correlation_matrix_usetables)

#only for recycling and demand (log) --> 0.862 and 0.886
cor(log(usetables_panel$plastic_mioEUR_dom), log(usetables_panel$recycling_tonne), use = "complete.obs")
cor(log(usetables_panel$plastic_mioEUR_tot), log(usetables_panel$recycling_tonne), use = "complete.obs")

# recycling and demand (log) by country
correlation_usetables_dom <- usetables_panel %>%
  group_by(geo) %>%
  summarise(correlation = cor(log(plastic_mioEUR_dom), log(recycling_tonne), use = "complete.obs"))
print(correlation_usetables_dom)

correlation_usetables_tot <- usetables_panel %>%
  group_by(geo) %>%
  summarise(correlation = cor(log(plastic_mioEUR_tot), log(recycling_tonne), use = "complete.obs"))
print(correlation_usetables_tot)

#cross correlation --> 0.863

cross_correlation_usetables <- usetables_panel %>%
  summarise(correlation = cor(log(plastic_mioEUR_dom), log(lag_recycling_tonne), use = "complete.obs"))
print(cross_correlation_usetables)

##fixed effect reg

#using plm

# Fixed effects model (within estimator) domestic use --> insignificant: -0.046 (0.038), Adj R2: 0.118
model1_usetables <- plm(log(plastic_mioEUR_dom) ~ log(recycling_tonne) + GDP_index2015 + env_tax_mioEUR, 
              data = usetables_panel, 
              model = "within", effect = "twoways")  
summary(model1_usetables) 

#get clustered SEs
coeftest(model1_usetables, vcov = vcovHC, type = "HC1")

#fixed effects model total (domestic+imports) use -->  insignificant: -0.036 (0.029), Adj R2: 0.252
model2_usetables <- plm(log(plastic_mioEUR_tot) ~ log(recycling_tonne) + GDP_index2015 + env_tax_mioEUR, 
              data = usetables_panel, 
              model = "within", effect = "twoways")  
summary(model2_usetables) 

#get clustered SEs
coeftest(model2_usetables, vcov = vcovHC, type = "HC1")

#fixed effects model with recycling rate and domestic use --> insignificant: -0.013 (0.110), Adj R2: 0.08

model1_usetables_rate <- plm(log(plastic_mioEUR_dom) ~ log(recycling_rate) + GDP_index2015 + env_tax_mioEUR, 
                            data = usetables_panel, 
                            model = "within", effect = "twoways")  

summary(model1_usetables_rate)

#get clustered SEs
coeftest(model1_usetables_rate, vcov = vcovHC, type = "HC1")

#fixed effects model with recycling rate and total use -->  insignificant: -0.051 (0.091), Adj R2: 0.22
model2_usetables_rate <- plm(log(plastic_mioEUR_tot) ~ log(recycling_rate) + GDP_index2015 + env_tax_mioEUR, 
                            data = usetables_panel, 
                            model = "within", effect = "twoways")  

summary(model2_usetables_rate) 

#get clustered SEs
coeftest(model2_usetables_rate, vcov = vcovHC, type = "HC1")