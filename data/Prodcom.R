library(tidyr)
library(dplyr)

setwd("~/Documents/MPP/Masterthesis/data")
prodcom_total <- read.csv("data-18500097.csv")
recycling_allcountries <- read.csv("recycling_env_wastrt.csv", header=TRUE)
wastegeneration_allcountries <- read.csv("plastic waste generated.csv", header = TRUE)
packaging_recycling <- read.csv("estat_env_waspac.csv", header = TRUE)
packaging_generation <- read.csv("packaging waste generated.csv", header = TRUE) ##generation of packaging waste 
nama_gdp <- read.csv("estat_nama_gdp.csv") #annual GDP in chain linked volumes index = 2015
env_taxes <- read.csv("estat_env_taxes.csv") #annual environmental taxes in million EUR

prodcom_total <- prodcom_total %>%
  pivot_wider(names_from = INDICATORS_LAB, values_from = INDICATOR_VALUE)

prodcom_total <- prodcom_total %>%
  mutate(TIME_PERIOD = as.integer(sub(".*([0-9]{4})$", "\\1", PERIOD_LAB))) %>%
  select(-PERIOD_LAB) 

prodcom_total <- prodcom_total %>% rename("geo" = "DECL_LAB")

#clean recycling data

recycling_allcountries <- recycling_allcountries%>% select(-c("DATAFLOW", "LAST.UPDATE",
                                              "freq", "waste", "wst_oper", 
                                                 "CONF_STATUS", "unit", "hazard"))

recycling_allcountries <- recycling_allcountries %>% rename("recycling_tonne" = "OBS_VALUE", 
                                                "OBS_FLAG_recycling" = "OBS_FLAG")

wastegeneration_allcountries <- wastegeneration_allcountries %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                                                           "freq", "unit", "hazard", 
                                                                           "nace_r2", "waste", "CONF_STATUS"))

wastegeneration_allcountries <- wastegeneration_allcountries %>% rename("OBS_FLAG_gen" = "OBS_FLAG", 
                                                                        "plastic_generation" = "OBS_VALUE")

packaging_recycling <- packaging_recycling %>% select(-c("DATAFLOW", "LAST.UPDATE",
                                                         "freq", "waste", "wst_oper", 
                                                         "CONF_STATUS", "unit"))

packaging_recycling <- packaging_recycling %>% rename("packaging_recycling_tonne" = "OBS_VALUE", 
                                                      "OBS_FLAG_recycling" = "OBS_FLAG")

packaging_generation <- packaging_generation %>% select(-c("DATAFLOW", "LAST.UPDATE",
                                                           "freq", "waste", "wst_oper", 
                                                           "CONF_STATUS", "unit"))

packaging_generation <- packaging_generation %>% rename("OBS_FLAG_gen" = "OBS_FLAG", 
                                                        "packaging_generation" = "OBS_VALUE")


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


#merge
prodcom_df <- merge(prodcom_total, recycling_allcountries, by = c("geo", "TIME_PERIOD")) 
prodcom_packaging_df <- merge(prodcom_total, packaging_recycling, by = c("geo", "TIME_PERIOD"))

#remove NAs and 0
prodcom_df <- prodcom_df %>%
  filter(!(PRODQNT == 0 | is.na(PRODQNT) | recycling_tonne == 0 | is.na(recycling_tonne)))

prodcom_packaging_df <- prodcom_packaging_df %>%
  filter(!(PRODQNT == 0 | is.na(PRODQNT) | packaging_recycling_tonne == 0 | is.na(packaging_recycling_tonne)))

#remove unbalanced countries
prodcom_df <- prodcom_df %>%
  filter(!geo %in% c("Albania", "Bosnia and Herzegovina", "Ireland", "Denmark", "Estonia", "Serbia"))

prodcom_packaging_df <- prodcom_packaging_df %>%
  filter(!geo %in% c("Estonia", "Latvia", "Ireland", "Slovenia"))

#transform recycling kg into tonnes
prodcom_df$PRODQNT <-  as.numeric(prodcom_df$PRODQNT)
prodcom_df$PRODQNT <- prodcom_df$PRODQNT/1000
prodcom_df$QNTUNIT <- "tonnes"

prodcom_packaging_df$PRODQNT <-  as.numeric(prodcom_packaging_df$PRODQNT)
prodcom_packaging_df$PRODQNT <- prodcom_packaging_df$PRODQNT/1000
prodcom_packaging_df$QNTUNIT <- "tonnes"

##merge with GDP and environmental tax data
prodcom_df <- merge(prodcom_df, nama_gdp, by = c("geo", "TIME_PERIOD"))
prodcom_df <- merge(prodcom_df, env_taxes, by = c("geo", "TIME_PERIOD"))
prodcom_df <- merge(prodcom_df, wastegeneration_allcountries,  by = c("geo", "TIME_PERIOD"))

prodcom_packaging_df <- merge(prodcom_packaging_df, packaging_generation,  by = c("geo", "TIME_PERIOD"))
prodcom_packaging_df <- merge(prodcom_packaging_df, nama_gdp, by = c("geo", "TIME_PERIOD"))
prodcom_packaging_df <- merge(prodcom_packaging_df, env_taxes, by = c("geo", "TIME_PERIOD"))

## calculate recycling rate
prodcom_df$recycling_rate <- (prodcom_df$recycling_tonne/prodcom_df$plastic_generation)*100
prodcom_packaging_df$packaging_recycling_rate <- (prodcom_packaging_df$packaging_recycling_tonne/prodcom_packaging_df$packaging_generation)*100

##declare as panel data

prodcom_df <- pdata.frame(prodcom_df, index = c("geo","TIME_PERIOD"))
prodcom_packaging_df <- pdata.frame(prodcom_packaging_df, index = c("geo", "TIME_PERIOD"))

##inspect variables visually. both highly skewed to the left (right tail)
plot(
  hist(prodcom_df$PRODQNT), 
  main="Histogram of production quantity",
  xlab="Production Quantity (tonnes)", ylab="Density"
)

plot(
  hist(log(prodcom_df$PRODQNT)), 
  main="Histogram of production quantity",
  xlab="Production Quantity (tonnes)", ylab="Density"
)

plot(
  hist(prodcom_df$recycling_tonne), 
  main="Histogram of production quantity",
  xlab="Production Quantity (tonnes)", ylab="Density"
)

plot(
  hist(log(prodcom_df$recycling_tonne)), 
  main="Histogram of production quantity",
  xlab="Production Quantity (tonnes)", ylab="Density"
)

plot(
  hist(prodcom_df_model$plastic_generation), 
  main="Histogram of production quantity",
  xlab="Production Quantity (tonnes)", ylab="Density"
)

plot(
  hist(log(prodcom_df_model$plastic_generation)), 
  main="Histogram of production quantity",
  xlab="Production Quantity (tonnes)", ylab="Density"
)

plot(
  hist(prodcom_df_model$packaging_recycling_tonne), 
  main="Histogram of production quantity",
  xlab="Production Quantity (tonnes)", ylab="Density"
)

plot(
  hist(log(prodcom_df_model$packaging_recycling_tonne)), 
  main="Histogram of production quantity",
  xlab="Production Quantity (tonnes)", ylab="Density"
)

plot(
  hist(prodcom_df_model$packaging_recycling_rate), 
  main="Histogram of production quantity",
  xlab="Production Quantity (tonnes)", ylab="Density"
)

plot(
  hist(prodcom_df_model$packaging_generation), 
  main="Histogram of production quantity",
  xlab="Production Quantity (tonnes)", ylab="Density"
)

plot(
  hist(log(prodcom_df_model$packaging_generation)), 
  main="Histogram of production quantity",
  xlab="Production Quantity (tonnes)", ylab="Density"
)

#scatterplots
ggplot(data = prodcom_df, aes(x = log(recycling_tonne), y = log(PRODQNT))) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid") +
  labs(title = "Scatterplot of Production Quantity over Recycling (PRODCOM)",
       x = "Plastic Recycling (Log)",
       y = "Plastic Production (Log)")

##fixed effects - double log model
model1_prodcom <- plm(log(PRODQNT) ~ log(recycling_tonne) + GDP_index2015 + log(env_tax_mioEUR), 
                        data = prodcom_df, 
                        model = "within", effect = "twoways")  

summary(model1_prodcom, vcov = function(x) vcovCR(x, cluster = "geo", type = "CR1"))
model1_prodcom_se <- coeftest(model1_prodcom, vcovHC, cluster = "group", method = "arellano", type = "HC2")
print(model1_prodcom_se)

## test error normality
fitted_values <- as.numeric(fitted(model1_prodcom))
residuals_values <- as.numeric(residuals(model1_prodcom))

plot(density(residuals_values))
plot(fitted_values, residuals_values,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted (plm Model)",
     pch = 20, col = "blue")
abline(h = 0, lty = 2, col = "red") 

# test heteroskedasticity and autocorrelation --> no serial correlation, but heteroskedasticity
pdwtest(model1_prodcom)
bptest(model1_prodcom)


##fixed effects model of log production quantity and recycling rate --> insignificant
model2_prodcom <- plm(log(PRODQNT) ~ recycling_rate + GDP_index2015 + log(env_tax_mioEUR), 
                      data = prodcom_df, 
                      model = "within", effect = "twoways")  

summary(model2_prodcom, vcov = function(x) vcovCR(x, cluster = "geo", type = "CR1"))
model2_prodcom_se <- coeftest(model2_prodcom, vcovHC, cluster = "group", method = "arellano", type = "HC2")
print(model2_prodcom_se)

## test error normality
fitted_values <- as.numeric(fitted(model2_prodcom))
residuals_values <- as.numeric(residuals(model2_prodcom))

plot(density(residuals_values))
plot(fitted_values, residuals_values,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted (plm Model)",
     pch = 20, col = "blue")
abline(h = 0, lty = 2, col = "red") 

# test heteroskedasticity and autocorrelation --> no serial correlation, but heteroskedasticity
pdwtest(model2_prodcom)
bptest(model2_prodcom)

## fixed effects model of waste generation and recycling --> significant and positive: 0.29 (0.14), but very bad model fit
model3_prodcom <- plm(log(plastic_generation) ~ log(recycling_tonne) + GDP_index2015 + log(env_tax_mioEUR), 
                        data = prodcom_df, 
                        model = "within", effect = "twoways")  

summary(model3_prodcom, vcov = function(x) vcovCR(x, cluster = "geo", type = "CR1"))
model3_prodcom_se <- coeftest(model3_prodcom, vcovHC, cluster = "group", method = "arellano", type = "HC2")
print(model3_prodcom_se)

## test error normality
fitted_values <- as.numeric(fitted(model3_prodcom))
residuals_values <- as.numeric(residuals(model3_prodcom))

plot(density(residuals_values))
plot(fitted_values, residuals_values,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted (plm Model)",
     pch = 20, col = "blue")
abline(h = 0, lty = 2, col = "red") 

# test heteroskedasticity and autocorrelation --> no serial correlation, but heteroskedasticity
pdwtest(model3_prodcom)
bptest(model3_prodcom)

# fixed effects regression of production quantity on packaging recycling quantity
model4_prodcom <- plm(log(PRODQNT) ~ log(packaging_recycling_tonne) + GDP_index2015 + log(env_tax_mioEUR), 
                      data = prodcom_packaging_df, 
                      model = "within", effect = "twoways")  

summary(model4_prodcom, vcov = function(x) vcovCR(x, cluster = "geo", type = "CR1"))
model4_prodcom_se <- coeftest(model4_prodcom, vcovHC, cluster = "group", method = "arellano", type = "HC2")
print(model4_prodcom_se)

# fixed effects regression of production quantity on packaging recycling rate
model5_prodcom <- plm(log(PRODQNT) ~ log(packaging_recycling_rate) + GDP_index2015 + log(env_tax_mioEUR), 
                      data = prodcom_packaging_df, 
                      model = "within", effect = "twoways")  

summary(model5_prodcom, vcov = function(x) vcovCR(x, cluster = "geo", type = "CR1"))
model5_prodcom_se <- coeftest(model5_prodcom, vcovHC, cluster = "group", method = "arellano", type = "HC2")
print(model5_prodcom_se)

# fixed effects regression of waste generation on packaging recycling quantity
model6_prodcom <- plm(log(packaging_generation) ~ log(packaging_recycling_tonne) + GDP_index2015 + log(env_tax_mioEUR), 
                       data = prodcom_packaging_df, 
                       model = "within", effect = "twoways")  

summary(model6_prodcom, vcov = function(x) vcovCR(x, cluster = "geo", type = "CR1"))
model6_prodcom_se <- coeftest(model6_prodcom, vcovHC, cluster = "group", method = "arellano", type = "HC2")
print(model6_prodcom_se)