library(dplyr, exclude = c("lag", "filter"))
library(ggplot2)
library(lmtest)
library(corrplot)
library(plm)
library(car)
library(panelr)
library(scales)
library(clubSandwich)

##set working directory

setwd("~/Documents/MPP/Masterthesis/data")

##final dataset: 47 obsesrvations

##data cleaning

#flag glossary: p = provisional value, i = value imputed by eurostat or other 
#receiving agencies, d = definition differs, e = estimated, b = break in timeseries

recycling_plastic <- read.csv("estat_env_wastrt_recycled_plastic.csv", header = TRUE) #plastic waste recycling in tonnes
plastic_production_2015 <-read.csv("estat_sts_inpr_a_index2015.csv", header = TRUE) #primary plastic production index 2015
nama_gdp <- read.csv("estat_nama_gdp.csv") ##annual GDP in chain linked volumes index = 2015
nama_gdp_mioeur <- read.csv("estat_nama_10_gdp_chainlinked_mioeur.csv") ##annual GDP in chain linked volumes million euros
env_taxes <- read.csv("estat_env_taxes.csv") ## annual environmental taxes in million EUR
plastic_generation <- read.csv("plastic waste generated.csv", header = TRUE)

#clean up production data
plastic_production_2015 <- plastic_production_2015 %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                                  "freq", "indic_bt", "nace_r2", 
                                                  "s_adj", "unit", "CONF_STATUS"))

plastic_production_2015 <- plastic_production_2015 %>% rename("OBS_FLAG2015" = "OBS_FLAG", 
                                              "production_index2015" = "OBS_VALUE")


#clean up recycling data
recycling_plastic <- recycling_plastic %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                                 "freq", "unit", "hazard", 
                                                 "wst_oper", "waste", "CONF_STATUS"))

recycling_plastic <- recycling_plastic %>% rename("OBS_FLAG_recycling" = "OBS_FLAG", 
                                              "recycling_tonne" = "OBS_VALUE")


# clean up waste generation data
plastic_generation <- plastic_generation %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                                       "freq", "unit", "hazard", 
                                                       "nace_r2", "waste", "CONF_STATUS"))

plastic_generation <- plastic_generation %>% rename("OBS_FLAG_gen" = "OBS_FLAG", 
                                                    "plastic_generation" = "OBS_VALUE")


#clean up gdp data
nama_gdp <- nama_gdp %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                    "freq", "na_item",  
                                    "unit", "CONF_STATUS"))

nama_gdp <- nama_gdp %>% rename("OBS_FLAG_GDP" = "OBS_FLAG", 
                                "GDP_index2015" = "OBS_VALUE")

nama_gdp_mioeur <- nama_gdp_mioeur  %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                                  "freq", "na_item",  
                                                  "unit", "CONF_STATUS"))

nama_gdp_mioeur <- nama_gdp_mioeur %>% rename("OBS_FLAG_GDP" = "OBS_FLAG", 
                                "GDP_mioeur" = "OBS_VALUE")

#clean up environmental taxes data

env_taxes <- env_taxes %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                    "freq", "tax", "nace_r2", 
                                   "unit", "CONF_STATUS"))

env_taxes <- env_taxes %>% rename("OBS_FLAG_envtax" = "OBS_FLAG", 
                                "env_tax_mioEUR" = "OBS_VALUE")

#merge datasets
merged_df <- merge(recycling_plastic, plastic_production_2015, by = c("geo", "TIME_PERIOD"))
merged_df <- merge(merged_df, plastic_generation, by = c("geo", "TIME_PERIOD"))
merged_df <- merge(merged_df, nama_gdp, by = c("geo", "TIME_PERIOD"))
merged_df <- merge(merged_df, nama_gdp_mioeur, by = c("geo", "TIME_PERIOD"))
merged_df <- merge(merged_df, env_taxes, by = c("geo", "TIME_PERIOD"))


#create datasets without observations from Turkey
merged_df_noT <- merged_df %>%
  filter(geo != "Türkiye")

## calculate recycling rate and rescale recycling variable
merged_df$recycling_rate <- (merged_df$recycling_tonne/merged_df$plastic_generation)*100
merged_df$recycling_kilotonne <- (merged_df$recycling_tonne/1000)
merged_df_noT$recycling_rate <- (merged_df_noT$recycling_tonne/merged_df_noT$plastic_generation)*100

#create index of recycling
merged_df <- merged_df %>%
  group_by(geo) %>% 
  mutate(recycling_2016 = recycling_tonne[TIME_PERIOD == 2016], 
         recycling_index = (recycling_tonne / recycling_2016) * 100) %>%
  ungroup() 

#dedclare as panel data
pdata <- pdata.frame(merged_df, index = c("geo","TIME_PERIOD"))
pdata_noT <- pdata.frame(merged_df_noT, index = c("geo","TIME_PERIOD"))

##data summary

summary(merged_df)
sd(merged_df$production_index2015)

## Explore data visually

#look at distribution of independent and dependent variable. Even when I log transform my independent variable, it is not normally distributed???
plot(density(merged_df$recycling_tonne), 
     main = "Density Plot Recycling", 
     xlab = "Recycling (in tonnes)", 
     col = "blue")

plot(density(log(merged_df$recycling_tonne)), 
     main = "Density Plot Recycling", 
     xlab = "Recycling (in tonnes)", 
     col = "blue")

plot(density(merged_df$production_index2015), 
     main = "Density Plot of Plastic Production", 
     xlab = "Production (Index 2015 = 100)", 
     col = "blue")

## plot recycling development by country on one graph. without the log greece has 
# almost no change in recycling but with the log it has one of the highest change in recycling.
ggplot(merged_df, aes(x = TIME_PERIOD, y = log(recycling_tonne), color = geo)) + 
  geom_line(alpha = 0.7) + 
  geom_point(alpha = 0.5) +  
  theme_minimal() +
  scale_x_continuous(breaks = seq(2010, 2022, by = 2)) + 
  labs(title = "Plastic Production & Recycling Trends Over Time",
       x = "Year") +
  labs(title = "Recycling Trends Over Time by Country",
       x = "Year",
       y = "Recycling (Log)",
       color = "Country") +
  theme(legend.position = "bottom")  

#plot production development on one graph
ggplot(merged_df, aes(x = TIME_PERIOD, y =production_index2015, color = geo)) + 
  geom_line(alpha = 0.7) +  
  geom_point(alpha = 0.5) +  
  theme_minimal() +
  scale_x_continuous(breaks = seq(2010, 2022, by = 2)) + 
  labs(title = "Plastic Production & Recycling Trends Over Time",
       x = "Year") +
  labs(title = "Production Trends Over Time by Country",
       x = "Year",
       y = "Production (QIndex=2015)",
       color = "Country") +
  theme(legend.position = "bottom") 

##plot recycling and production on one graph

#plot production and recycling in index form on one plot

ggplot(merged_df, aes(x = TIME_PERIOD)) +
  geom_line(aes(y = production_index2015, color = "Production (Index 2015=100)"), linewidth = 1) +
  geom_line(aes(y = recycling_index, color = "Recycling (Index 2016=100)"), linewidth = 1) +
  facet_wrap(~ geo, scales = "free_y") +  
  theme_minimal() +
  scale_y_continuous(
    name = "Production (index 2015 = 100",
    sec.axis = sec_axis(~ ., name = "Recycling (Index 2016=100)")
  ) +
  scale_x_continuous(breaks = seq(2010, 2022, by = 2)) + 
  labs(title = "Plastic Production & Recycling Trends Over Time",
       x = "Year") +
  scale_color_manual(values = c("Production (Index 2015=100)" = "red", "Recycling (Index 2016=100)" = "blue")) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

##scatterplots

#plot production over recycling (log) 
ggplot(merged_df, aes(x= (recycling_tonne/1000), y= production_index2015, color = 'red')) +
  geom_point() +
  geom_smooth(method = "lm")
  theme_minimal() +
  labs(title = "Production vs. Recycling",
       x = "Recycling (Log)",
       y = "Production (Index 2015 = 100)")  

# plot production over recycling (log) grouped by country
ggplot(merged_df, aes(x = (recycling_tonne/1000), y = production_index2015, color = as.factor(geo))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "solid")
  theme_minimal() +
    labs(title = "Production vs. Recycling Quantities by Year",
         x = "Recycling (Log)",
         y = "Production (Index 2015=100)",
         color = "geo")

#plot production over recycling (index)
ggplot(merged_df, aes(x= recycling_index, y= production_index2015, color = 'red')) +
    geom_point() +
    theme_minimal() +
    labs(title = "Production vs. Recycling",
         x = "Recycling (Index 2016 = 100)",
         y = "Production (Index 2015 = 100)")

ggplot(merged_df, aes(x = recycling_index, y = production_index2015, color = as.factor(geo))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "solid") +
  theme_minimal() +
  labs(title = "Production vs. Recycling Quantities by Year",
       x = "Recycling (Index 2016 = 100)",
       y = "Production (Index 2015=100)",
       color = "geo")

#plot production over environmental tax revenue
ggplot(merged_df, aes(x= recycling_index, y= production_index2015, color = 'red')) +
  geom_point() +
  theme_minimal() +
  labs(title = "Production vs. Recycling",
       x = "Recycling (Index 2016 = 100)",
       y = "Production (Index 2015 = 100)")

ggplot(model_df, aes(x = env_tax_mioEUR, y = production_index2015, color = as.factor(geo))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "solid") +
  theme_minimal() +
  labs(title = "Production vs. Recycling Quantities by Year",
       x = "Environmental taxes",
       y = "Production (Index 2015=100)",
       color = "geo")


## plot 10-year differences in scatterplot

#compute differences. When I take the log of the recycling variable it kind of distorts recycling because greece turns from almost no change in recycling to having the largest log difference. 
merged_df$lnrecycling <- log(merged_df$recycling_tonne)

data_diff <- merged_df %>%
  filter(TIME_PERIOD %in% c(2012, 2022)) %>% 
  group_by(geo) %>%  
  summarize(
    production_diff2015 = production_index2015[TIME_PERIOD == 2022] - production_index2015[TIME_PERIOD == 2012],
    recycling_diff = lnrecycling[TIME_PERIOD == 2022] - lnrecycling[TIME_PERIOD == 2012]) %>%
  ungroup()


#plot differences

# production difference and recycling difference. I cannot take the log because the differences are negative values. however if I compute the differences of the log over time, I get very weird values.
ggplot(data_diff, aes(x = recycling_diff, y = production_diff2015, label = geo)) +
  geom_point() +
  geom_text(vjust = -0.5, size = 3) + 
  theme_minimal() +
  labs(title = "Production and Recycling Differences over 10 years", 
       x = "Recycling Difference (Log)", 
       y = "Production Difference")

## Seems like Turkey is an outlier. Turkey's recycling apparently exploded between 2012 and 2022. General pattern is that: if recycling volume 
## increases, production volume decreases. Furthermore, my problem is that the independent variable barely varies over time. No variation in the IV is very bad.
## In almost all countries, production decreased over time.

## analysis of correlation

#only for log of recycling and production --> -0.375
cor(merged_df$production_index2015, log(merged_df$recycling_tonne), use = "complete.obs")

#only for log of recycling and production, no Turkey --> -0.682
cor(merged_df_noT$production_index2015, log(merged_df_noT$recycling_tonne), use = "complete.obs")

# recycling (log) and production by country --> positive and negative values
correlation <- merged_df %>%
  group_by(geo) %>%
  summarise(correlation = cor(production_index2015, log(recycling_tonne), use = "complete.obs"))
print(correlation)

# recycling (index) and production --> positive correlation
cor(merged_df$production_index2015, merged_df$recycling_index, use = "complete.obs")

# recycling (index) and production by country --> positive and negative values
correlation <- merged_df %>%
  group_by(geo) %>%
  summarise(correlation = cor(production_index2015, recycling_index, use = "complete.obs"))
print(correlation)

##correlate waste generation with recycling --> 0.599
correlation_plastic_gen <- cor(merged_df$plastic_generation, log(merged_df$recycling_tonne), use = "complete.obs")
print(correlation_plastic_gen)

# correlate by country --> all positive
correlation_plastic_gen_bycountry <- merged_df %>%
  group_by(geo) %>%
  summarise(correlation = cor(log(recycling_tonne), plastic_generation, use = "complete.obs"))

print(correlation_plastic_gen_bycountry)

##correlate waste generation with recycling rate
correlation_plastic_gen <- cor(merged_df$plastic_generation, log(merged_df$recycling_rate), use = "complete.obs")
print(correlation_plastic_gen)

# correlate by country
correlation_plastic_rate_gen_bycountry <- merged_df %>%
  group_by(geo) %>%
  summarise(correlation = cor(log(recycling_rate), plastic_generation, use = "complete.obs"))

print(correlation_plastic_rate_gen_bycountry)

#cross correlation --> -0.58
cross_correlation <- pdata %>%
  summarise(correlation = cor(production_index2015, log(lag_recycling_tonne), use = "complete.obs"))
print(cross_correlation)

#cross correlation, no Turkey -0.717
cross_correlation_noT <- pdata_noT %>%
  summarise(correlation = cor(production_index2015, log(lag_recycling_tonne), use = "complete.obs"))
print(cross_correlation_noT)

#when using the previous years' recycling quantity the correlation is still negative but only at 0.58
# nevertheless correlation does not imply causation at all, but the less than 1 decrease in production can be explained by other factors

##Regressions

# Fixed effects model with production of plastic and recycling (log) --> insignificant 1.54 (3.2), Adj R2: 0.376, residuals relatively normal
model1 <- plm(production_index2015 ~ log(recycling_tonne) + GDP_index2015 + log(env_tax_mioEUR), 
             data = pdata, 
             model = "within", effect = "twoway")  

## produce summary for  R squared and coeftest for the correct standard errors.
summary(model1, vcov = function(x) vcovCR(x, cluster = "geo", type = "CR1"))
model1_se <- coeftest(model1, vcovHC, cluster = "group", method = "arellano", type = "HC2")

#plot to check normality of residuals
fitted_values <- as.numeric(fitted(model1))
residuals_values <- as.numeric(residuals(model1))

plot(density(residuals_values))
plot(fitted_values, residuals_values,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted (plm Model)",
     pch = 20, col = "blue")
abline(h = 0, lty = 2, col = "red") 

#durbin watson test for serial correlation in the error terms. https://rdrr.io/rforge/plm/man/pdwtest.html
# H0 = no serial correlation --> p value = 0.242 --> no serial correlation
pdwtest(model1)

#breusch pagan test for heteroskedasticity
# H0 = homoskedasticity --> p value = 0.797 --> homoskedasticity
bptest(model1)

#same model but without observations from turkey
model1_noT <- plm(production_index2015 ~ log(recycling_tonne) + GDP_index2015 + log(env_tax_mioEUR), 
              data = pdata_noT, 
              model = "within", effect = "twoway")  

summary(model1_noT, vcov = function(x) vcovHC(x, cluster = "group", method = "arellano", type = "HC2"))
model1_se_noT <- coeftest(model1_noT, vcovHC, cluster = "group", method = "arellano", type = "HC2")

#plot to check normality of residuals
fitted_values <- as.numeric(fitted(model1_noT))
residuals_values <- as.numeric(residuals(model1_noT))

plot(density(residuals_values))
plot(fitted_values, residuals_values,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted (plm Model)",
     pch = 20, col = "blue")
abline(h = 0, lty = 2, col = "red") 

#durbin watson test for serial correlation in the error terms. https://rdrr.io/rforge/plm/man/pdwtest.html
# H0 = no serial correlation --> p value = 0.316 --> no serial correlation
pdwtest(model1_noT)

#breusch pagan test for heteroskedasticity
# H0 = homoskedasticity --> p value = 0.4228 --> my error terms are homoskedastic
bptest(model1_noT)

## fixed effects model of production and recycling rate: significant and moderate model fit
model2 <- plm(production_index2015 ~ log(recycling_rate) + GDP_index2015 + log(env_tax_mioEUR),
                  data = pdata, 
                  model = "within", effect = "twoways")  

summary(model2, vcov = function(x) vcovCR(x, cluster = "geo", type = "CR1"))
model2_se <- coeftest(model2, vcovHC, cluster = "group", method = "arellano", type = "HC2")
print(model2_se)

fitted_values <- as.numeric(fitted(model2))
residuals_values <- as.numeric(residuals(model2))

plot(density(residuals_values))
plot(fitted_values, residuals_values,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted (plm Model)",
     pch = 20, col = "blue")
abline(h = 0, lty = 2, col = "red")  

#Durbin Watson test for heteroskedasticity: p = 0.2214 --> homoskedastic
pdwtest(model2)

#Breusch Pagan test for serial correlation in error terms p = 0.0679 --> no serial correlation
bptest(model2)

#same model but without turkey -_> insignificant coefficient, negative model fit
model2_noT <- plm(production_index2015 ~ log(recycling_rate) + GDP_index2015 + log(env_tax_mioEUR), 
                   data = pdata_noT, 
                   model = "within", effect = "twoways")  

summary(model2_noT, vcov = function(x) vcovCR(x, cluster = "geo", type = "CR1"))
model2_se_noT <- coeftest(model2_noT, vcovHC, cluster = "group", method = "arellano", type = "HC2")

# fixed effects model with waste generation and recycling quantities  --> 
model3 <- plm(log(plastic_generation) ~ log(recycling_tonne) + GDP_index2015 + env_tax_mioEUR, 
                  data = pdata, 
                  model = "within", effect = "twoways")  

summary(model3, vcov = function(x) vcovCR(x, cluster = "geo", type = "CR1"))
model3_se <- coeftest(model3, vcovHC, cluster = "group", method = "arellano", type = "HC2")

fitted_values <- as.numeric(fitted(model3))
residuals_values <- as.numeric(residuals(model3))

plot(density(residuals_values))
plot(fitted_values, residuals_values,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted (plm Model)",
     pch = 20, col = "blue")
abline(h = 0, lty = 2, col = "red")  

#Durbin Watson test for heteroskedasticity: p = 0.000000186 --> heteroskedasticity!
pdwtest(model3)

#Breusch Pagan test for serial correlation in error terms p = 0.505 --> no serial correlation
bptest(model3)

#same model without Turkey --> insignificant and very bad model fit

model3_noT <- plm(log(plastic_generation) ~ log(recycling_rate) + GDP_index2015 + log(env_tax_mioEUR), 
              data = pdata_noT, 
              model = "within", effect = "twoways")  

summary(model3_noT, vcov = function(x) vcovCR(x, cluster = "geo", type = "CR1"))
model3_se_noT <- coeftest(model1, vcovHC, cluster = "group", method = "arellano", type = "HC2")

fitted_values <- as.numeric(fitted(model3_noT))
residuals_values <- as.numeric(residuals(model3_noT))

plot(density(residuals_values))
plot(fitted_values, residuals_values,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted (plm Model)",
     pch = 20, col = "blue")
abline(h = 0, lty = 2, col = "red")  

#Durbin Watson test for heteroskedasticity: p = 0.000001385 --> heteroskedasticity!
pdwtest(model3_noT)

#Breusch Pagan test for serial correlation in error terms p = 0.232 --> no serial correlation
bptest(model3_noT)