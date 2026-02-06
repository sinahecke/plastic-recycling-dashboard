## do analysis with data on plastic packaging

library(dplyr, exclude = c("lag", "filter"))
library(ggplot2)
library(lmtest)
library(corrplot)
library(plm)
library(car)
library(panelr)
library(scales)
library(clubSandwich)
library(hrbrthemes)

##set working directory

setwd("~/Documents/MPP/Masterthesis/data")

##final dataset: 64 observations

##open file

# production data only on Germany, Spain, Greece, Ireland, Italy, Türkiye from around 2009 - 2023
# recycling data on all MS, except Turkey, until around 2022

packaging_recycling <- read.csv("estat_env_waspac.csv", header = TRUE) ## plastic packaging recycling overall in tonne
production_index2015 <- read.csv("estat_sts_inpr_a_index2015.csv", header = TRUE)
plastic_generation <- read.csv("plastic waste generated.csv", header = TRUE)
packaging_generation <- read.csv("packaging waste generated.csv", header = TRUE)
nama_gdp <- read.csv("estat_nama_gdp.csv") #annual GDP in chain linked volumes index = 2015
nama_gdp_mioeur <- read.csv("estat_nama_10_gdp_chainlinked_mioeur.csv") ##annual GDP in chain linked volumes million euros
env_taxes <- read.csv("estat_env_taxes.csv") #annual environmental taxes in million EUR

#clean data

production_index2015 <- production_index2015 %>% select(-c("DATAFLOW", "LAST.UPDATE",  
                                                           "freq", "indic_bt", "nace_r2", 
                                                           "s_adj", "unit", "CONF_STATUS"))

production_index2015 <- production_index2015 %>% rename("production_index2015" = "OBS_VALUE", 
                                                        "production_FLAG" = "OBS_FLAG")

packaging_recycling <- packaging_recycling %>% select(-c("DATAFLOW", "LAST.UPDATE",
                                                          "freq", "waste", "wst_oper", 
                                                           "CONF_STATUS", "unit"))

packaging_recycling <- packaging_recycling %>% rename("recycling_tonne" = "OBS_VALUE", 
                                                      "OBS_FLAG_recycling" = "OBS_FLAG")
  
packaging_generation <- packaging_generation %>% select(-c("DATAFLOW", "LAST.UPDATE",
                                                           "freq", "waste", "wst_oper", 
                                                           "CONF_STATUS", "unit"))

packaging_generation <- packaging_generation %>% rename("OBS_FLAG_gen" = "OBS_FLAG", 
                                                        "packaging_generation" = "OBS_VALUE")

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


# remove EU27 aggregate observations
packaging_recycling <- packaging_recycling %>% filter(geo %in% c("Germany", "Spain", "France", "Netherlands", "Greece", "Italy"))

##merge datasets

#merged_packaging_df includes France (2009-2022), Germany (2009 - 2022), Greece (2009-2022), Italy (2009-2022), Spain(2009-2022). Total: 91 obs.
merged_packaging_df <- merge(packaging_recycling, production_index2015, by = c("geo", "TIME_PERIOD"))
merged_packaging_df <- merge(merged_packaging_df, nama_gdp, by = c("geo", "TIME_PERIOD"))
merged_packaging_df <- merge(merged_packaging_df, nama_gdp_mioeur, by = c("geo", "TIME_PERIOD"))
merged_packaging_df <- merge(merged_packaging_df, env_taxes, by = c("geo", "TIME_PERIOD"))
merged_packaging_df <- merge(merged_packaging_df, plastic_generation, by = c("geo", "TIME_PERIOD"), all.x = TRUE)
merged_packaging_df <- merge(merged_packaging_df, packaging_generation, by = c("geo", "TIME_PERIOD"))

merged_packaging_df$recycling_rate <- (merged_packaging_df$recycling_tonne/merged_packaging_df$packaging_generation)*100

##declare as panel data

panel_packaging_df <- pdata.frame(merged_packaging_df, index = c("geo", "TIME_PERIOD"))

## summary statistics

summary(panel_packaging_df)
pdim(panel_packaging_df)

# explore data distribution
# Plot the density distribution of recycling
plot(
  density(log(merged_packaging_df$recycling_tonne)), 
  main="Density Plot of Packaging Recycling Quantity",
  xlab="Recycling Quantity", ylab="Density"
)

plot(
  density(merged_packaging_df$recycling_scaled), 
  main="Density Plot of Packaging Recycling Quantity",
  xlab="Recycling Quantity", ylab="Density"
)


# Plot the density distribution of recycling rate (ln). When I take the log of the recycling rate, it is more skewed
plot(
  density(log(merged_packaging_df$recycling_rate)), 
  main="Density Plot of Packaging Recycling rate (Ln)",
  xlab="Recycling rate", ylab="Density"
)

# Plot the density distribution of production
plot(
  density(merged_packaging_df$production_index2015), 
  main="Density Plot of Packaging Production Quantity",
  xlab="Production (Index = 2015)", ylab="Density"
)


##plot data

#plot recycling development over time
ggplot(merged_packaging_df, aes(x = TIME_PERIOD, y = recycling_tonne, color = geo)) + 
  geom_line(alpha = 0.7) +  
  geom_point(alpha = 0.5) +  
  theme_minimal() +
  scale_x_continuous(breaks = seq(2010, 2022, by = 2)) + 
  scale_y_continuous(breaks = seq(0, 1500000, by = 500000), 
                     labels = label_number(scale = 1e-6, suffix = " million")) +
  labs(title = "Recycling Trends Over Time by Country (Packaging)",
       x = "Year",
       y = "Recycling Quantity",
       color = "Country") +
  theme(legend.position = "bottom")  

#plot production over time
ggplot(merged_packaging_df, aes(x = TIME_PERIOD, y=production_index2015, color = geo)) + 
  geom_line(alpha = 0.7) +  
  geom_point(alpha = 0.5) +  
  theme_minimal() +
  scale_x_continuous(breaks = seq(2010, 2022, by = 2)) + 
  labs(title = "Production Trends Over Time by Country",
       x = "Year",
       y = "Production Volume",
       color = "Country") +
  theme(legend.position = "bottom")  

## plot both variables over time on x axis
# transform recycling data into index format.

merged_packaging_df <- merged_packaging_df %>%
  group_by(geo) %>% 
  mutate(recycling_2015 = recycling_tonne[TIME_PERIOD == 2015], 
         recycling_index = (recycling_tonne / recycling_2015) * 100) %>%
  ungroup()  

#plot production and recycling in index form on one plot

ggplot(merged_packaging_df, aes(x = TIME_PERIOD)) +
  geom_line(aes(y = production_index2015, color = "Production (Index 2015=100)"), linewidth = 1) +
  geom_line(aes(y = recycling_index, color = "Recycling (Index 2015=100)"), linewidth = 1) +
  facet_wrap(~ geo, scales = "free_y") +  
  theme_minimal() +
  scale_y_continuous(
    name = "Production (index 2015 = 100",
    sec.axis = sec_axis(~ ., name = "Recycling (Index 2015=100)")
  ) +
  scale_x_continuous(breaks = seq(2010, 2022, by = 2)) + 
  labs(title = "Packaging Production & Recycling Trends Over Time",
       x = "Year") +
  scale_color_manual(values = c("Production (Index 2015=100)" = "red", "Recycling (Index 2015=100)" = "blue")) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

## scatterplots

#production and recycling tonne
ggplot(merged_packaging_df, aes(x= log(recycling_tonne), y= production_index2015, color = 'red')) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Production vs. Recycling (Packaging)",
       x = "Recycling (Ln)",
       y = "Production (Index 2015 = 100)")

ggplot(merged_packaging_df, aes(x = log(recycling_tonne), y = production_index2015, color = as.factor(geo))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "solid") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Production vs. Recycling (Packaging)",
       x = "Recycling (Ln)",
       y = "Production (Index 2015 = 100)",
       color = "geo")

#production and recycling rate
ggplot(merged_packaging_df, aes(x= log(recycling_rate), y= production_index2015, color = 'red')) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Production vs. Recycling Rate (Packaging)",
       x = "Recycling Rate (Ln)",
       y = "Production (Index 2015 = 100)")

ggplot(merged_packaging_df, aes(x = log(recycling_rate), y = production_index2015, color = as.factor(geo))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "solid") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Production vs. Recycling Rate (Packaging)",
       x = "Recycling Rate (Ln)",
       y = "Production (Index 2015 = 100)",
       color = "geo")

# packaging waste generation and recycling

ggplot(merged_packaging_df, aes(x= log(recycling_tonne), y= log(packaging_generation), color = 'red')) +
  geom_point() +
  geom_smooth(method = lm)+
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Waste Generation vs. Recycling (Packaging)",
       x = "Recycling (Ln)",
       y = "Waste Generation (Ln)")

ggplot(merged_packaging_df, aes(x = log(recycling_tonne), y = log(packaging_generation), color = as.factor(geo))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "solid") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Waste Generation vs. Recycling (Packaging)",
       x = "Recycling (Ln)",
       y = "Waste Generation (Ln)",
       color = "geo")

##scatterplot of 10 year differences

#compute differences
data_diff_packaging <- merged_packaging_df %>%
  filter(TIME_PERIOD %in% c(2010, 2022)) %>% 
  group_by(geo) %>%  
  summarize(
    production_diff = production_index2015[TIME_PERIOD == 2022] - production_index2015[TIME_PERIOD == 2010],
    recycling_diff = log(recycling_tonne[TIME_PERIOD == 2022]) - log(recycling_tonne[TIME_PERIOD == 2010])) 

#plot differences
# production in index2015 and recycling (ln)
ggplot(data_diff_packaging, aes(x = recycling_diff, y = production_diff, label = geo)) +
  geom_point() +
  geom_smooth(method = lm, color = "black") +
  geom_text(vjust = -0.5, size = 3) + 
  theme_minimal() +
  scale_x_continuous(limits = c(0.12, 0.7)) + 
  labs(title = "Production and Recycling Differences over 12 years", 
       x = "Recycling Difference (Ln)", 
       y = "Production Difference")

##analysis of correlation. normal correlation analysis probably compares datapoints across groups and does not account for panel data structure

#correlate production and recycling quantities --> -0.5731
cor(merged_packaging_df$production_index2015, log(merged_packaging_df$recycling_tonne), use = "complete.obs")

# correlate by country, because I want to see within-country correlations --> all extremely positive
merged_packaging_df %>%
  group_by(geo) %>%
  summarise(correlation = cor(log(recycling_tonne), production_index2015, use = "complete.obs"))

#correlate production and recycling rate --> -0.029
cor(merged_packaging_df$production_index2015, log(merged_packaging_df$recycling_rate), use = "complete.obs")

# correlate production and recycling rate by country 
merged_packaging_df %>%
  group_by(geo) %>%
  summarise(correlation = cor(log(recycling_rate), production_index2015, use = "complete.obs"))

##correlate waste generation with recycling
cor(log(merged_packaging_df$packaging_generation), log(merged_packaging_df$recycling_tonne), use = "complete.obs")

# correlate waste generation with recycling by country
correlation_packaging_gen_bycountry <- merged_packaging_df %>%
  group_by(geo) %>%
  summarise(correlation = cor(log(recycling_tonne), packaging_generation, use = "complete.obs"))
print(correlation_packaging_gen_bycountry)

##two way fixed effect regression 

## run data on plastic packaging recycling but with plastics overall production as outcome. --> positive and significant coefficient, moderate model fit
model1_packaging <- plm(production_index2015 ~ log(recycling_tonne) + GDP_index2015 + log(env_tax_mioEUR),
                        data = panel_packaging_df, 
                        model = "within", effect = "twoways")  

# cluster-robust SEs --> one sided p-value = 0.0021205
summary(model1_packaging)
m1_pck <- coeftest(model1_packaging, vcov. = function(x) plm::vcovHC(x, cluster = "group", method = "arellano", type = "HC2"))

#only robust SEs --> one-sided p-value = 0.00003351
m1_pck_robust <- coeftest(model1_packaging, vcov. = function(x) plm::vcovHC(x, method = "white1", type = "HC2"))

## test error normality
fitted_values <- as.numeric(fitted(model1_packaging))
residuals_values <- as.numeric(residuals(model1_packaging))

plot(density(residuals_values))
plot(fitted_values, residuals_values,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted (plm Model)",
     pch = 20, col = "blue")
abline(h = 0, lty = 2, col = "red") 

# durbin watson test --> p value = 0.00375: autocorrelation
pdwtest(model1_packaging)

#breusch pagan test --> p value = 0.3431: homoskedasticity
bptest(model1_packaging)

#multicollinearity test only works with lm models
model1_packaging_lm <- lm(production_index2015 ~ log(recycling_tonne) + GDP_index2015 + log(env_tax_mioEUR),
                        data = panel_packaging_df)  
vif(model1_packaging_lm) 

# since my error terms are heteroskedastic and I have serial correlation present in my data, 
# the feasible generalized least squares is perhaps a better method. However pggls does not allow two way FEs,
# but only time or individual. FGLS model has a much better fit despite only individual FEs

#fixed effect regression with production and recycling rate --> insignificant coefficient, negative r squared
model2_packaging <- plm(production_index2015 ~ log(recycling_rate) + GDP_index2015 + log(env_tax_mioEUR), 
                                 data = panel_packaging_df, 
                                 model = "within", effect = "twoways")  

#cluster-robust SEs --> one-sided p-value = 0.05713
summary(model2_packaging)
m2_pck <- coeftest(model2_packaging, vcov. = function(x) plm::vcovHC(x, cluster = "group", method = "arellano", type = "HC2"))

#only robust SEs --> one-sided p-value = 0.02087
m2_pck_robust <- coeftest(model2_packaging, vcov. = function(x) plm::vcovHC(x, method = "white1", type = "HC2"))

fitted_values <- as.numeric(fitted(model2_packaging))
residuals_values <- as.numeric(residuals(model2_packaging))

plot(density(residuals_values))
plot(fitted_values, residuals_values,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted (plm Model)",
     pch = 20, col = "blue")
     abline(h = 0, lty = 2, col = "red") 

#durbin watson test for serial correlation in the error terms 
# H0 = no serial correlation --> p value = 0.0001812 --> serial correlation 
pdwtest(model2_packaging)

#breusch pagan test for heteroskedasticity 
#H0 = homoskedasticity --> p value = 0.00219 --> heteroskedasticity
bptest(model2_packaging)

#multicollinearity test. Only works with lm models.
model2_packaging_lm <- lm(production_index2015 ~ log(recycling_rate) + GDP_index2015 + log(env_tax_mioEUR), 
                          data = panel_packaging_df) 
vif(model2_packaging_lm)

#fixed effect regression with waste generation and recycling 
model3_packaging <- plm(log(packaging_generation) ~ log(recycling_tonne) + GDP_index2015 + log(env_tax_mioEUR), 
                            data = panel_packaging_df, 
                            model = "within", effect = "twoways")  

#cluster robust SEs --> one-sided p-value = 0.000821
summary(model3_packaging) 
m3_pck <- coeftest(model3_packaging, vcov. = function(x) plm::vcovHC(x, cluster = "group", method = "arellano", type = "HC2"))

#only robust SEs --> one-sided p-value = 0.00027155
m3_pck_robust <- coeftest(model3_packaging, vcov. = function(x) plm::vcovHC(x, method = "white1", type = "HC2"))

fitted_values <- as.numeric(fitted(model1_packaging_gen))
residuals_values <- as.numeric(residuals(model1_packaging_gen))

plot(density(residuals_values))
plot(fitted_values, residuals_values,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted (plm Model)",
     pch = 20, col = "blue")
abline(h = 0, lty = 2, col = "red") 

#durbin watson test for serial correlation in the error terms 
# H0 = no serial correlation --> p value = 0.000000000019 --> strong serial correlation 
pdwtest(model3_packaging)

#breusch pagan test for heteroskedasticity 
#H0 = homoskedasticity --> p value = 0.0578 --> homoskedasticity
bptest(model3_packaging)

#multicollinearity test. Only works with lm models.
model3_packaging_lm <- lm(log(packaging_generation) ~ log(recycling_tonne) + GDP_index2015 + log(env_tax_mioEUR), 
                        data = panel_packaging_df)  

vif(model3_packaging_lm)

##sensitivity checks --> run all models without covariates

model4_packaging <- plm(production_index2015 ~ log(recycling_tonne),
                                   data = panel_packaging_df, 
                                   model = "within", effect = "twoways")  

m4_pck <- coeftest(model4_packaging, vcov. = function(x) plm::vcovHC(x, cluster = "group", method = "arellano", type = "HC2")) #one-sided p-value = 0.001998

model5_packaging <- plm(production_index2015 ~ log(recycling_rate), 
                        data = panel_packaging_df, 
                        model = "within", effect = "twoways")  

# one sided p-value: 0.230
m5_pck <- coeftest(model5_packaging, vcov. = function(x) plm::vcovHC(x, cluster = "group", method = "arellano", type = "HC2")) #one-sided p-value = 0.23015

model6_packaging <- plm(log(packaging_generation) ~ log(recycling_tonne), 
                        data = panel_packaging_df, 
                        model = "within", effect = "twoways")  

m6_pck <- coeftest(model6_packaging, vcov. = function(x) plm::vcovHC(x, cluster = "group", method = "arellano", type = "HC2")) #one-sided p-value = 0.0118