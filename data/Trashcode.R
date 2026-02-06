#Trash code


merged_packaging_df <- merge(merged_packaging_df, sts_inpr_a_packaging_2021, by = c("geo", "TIME_PERIOD"))

merged_packaging_df <- merged_packaging_df  %>% rename(packaging_recycling_tonne = OBS_VALUE.x,
                                                       production_packaging_index2015 = OBS_VALUE.y,
                                                       production_packaging_index2021 = OBS_VALUE)

merged_packaging_df <- merged_packaging_df %>% select(-c("DATAFLOW.x",  "LAST.UPDATE.x", "freq.x", "unit.x",
                                                         "wst_oper", "waste",
                                                         "CONF_STATUS.x", "DATAFLOW.y", "LAST.UPDATE.y", "freq.y", 
                                                         "indic_bt.x", "nace_r2.x", "s_adj.x", "unit.y", 
                                                         "CONF_STATUS.y", "DATAFLOW", "LAST.UPDATE", "freq",
                                                         "indic_bt.y", "nace_r2.y", "s_adj.y", "unit", "CONF_STATUS"))

merged_packaging_df <- merged_packaging_df  %>% rename(OBS_FLAG2021 = OBS_FLAG.y,
                                                       OBS_FLAGpercent = OBS_FLAG,
                                                       OBS_FLAGrecycling = OBS_FLAG.x)

## panel is unbalanced: Spain misses 2019 and greece misses 2020. Declare as missing observation??
merged_packaging_df <- merged_packaging_df %>%
  bind_rows(data.frame(geo = "Spain", TIME_PERIOD = 2019, packaging_recycling_tonne = NA,  production_packaging_index2015 = NA, production_packaging_index2021 = NA)) %>%
  bind_rows(data.frame(geo = "Greece", TIME_PERIOD = 2020, packaging_recycling_tonne = NA,  production_packaging_index2015 = NA, production_packaging_index2021 = NA))

merged_packaging_df <- merged_packaging_df %>%
  arrange(geo, TIME_PERIOD)

## env_wastrt_packaginginMS data on waste recycling within the member state unit = tonne
env_wastrt_packaginginMS <- env_wastrt_packaginginMS %>% filter(geo %in% c("Germany", "Spain", "France", "Netherlands", "Greece", "Italy", "Türkiye"))
env_wastrt_packaginginMS <- env_wastrt_packaginginMS %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                                                   "freq", "waste", "nace_r2", 
                                                                   "s_adj", "unit", "CONF_STATUS"))

#plot recycling quantities for plastic overall

pdata %>%
  line_plot(recycling_tonne, 
            add.mean = TRUE, 
            alpha = 0.2)

pdata %>%
  line_plot(recycling_percent, 
            add.mean = TRUE, 
            alpha = 0.2)
pdata %>%
  line_plot(production_percent, 
            add.mean = TRUE, 
            alpha = 0.2)

## plot recycling data by country for packaging

country_packaging_recycling <- ggplot(merged_packaging_df, aes(x = TIME_PERIOD, y = recycling_tonne, group = geo, color = geo)) +
  geom_line() +  
  geom_point() +  
  facet_wrap(~ geo, scales = "free_y") + 
  scale_x_continuous(breaks = seq(2009, 2022, by = 1)) +
  theme_minimal() +  
  labs(
    title = "Evolution of Packaging Recycling Volumes Over Time",
    x = "Year",
    y = "Packaging Recycling Volume (in tonnes)"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

country_packaging_lnrecycling <-ggplot(merged_packaging_df, aes(x = TIME_PERIOD, y = lnrecycling_tonne, group = geo, color = geo)) +
  geom_line() +  
  geom_point() +  
  facet_wrap(~ geo, scales = "free_y") + 
  scale_x_continuous(breaks = seq(2009, 2022, by = 1)) +
  theme_minimal() +  
  labs(
    title = "Evolution of Packaging Recycling Volumes Over Time",
    x = "Year",
    y = "Packaging Recycling Volume (in tonnes)"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

##plot recycling data by country for packaging overall
country_recycling <-ggplot(merged_df, aes(x = TIME_PERIOD, y = recycling_tonne, group = geo, color = geo)) +
  geom_line() +  
  geom_point() +  
  facet_wrap(~ geo, scales = "free_y") + 
  scale_x_continuous(breaks = seq(2010, 2022, by = 2)) +
  theme_minimal() +  
  labs(
    title = "Evolution of Recycling Volumes Over Time",
    x = "Year",
    y = "Recycling Volume (in tonnes)"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


country_recycling_ln <-ggplot(merged_df, aes(x = TIME_PERIOD, y = lnrecycling_tonne, group = geo, color = geo)) +
  geom_line() +  
  geom_point() +  
  facet_wrap(~ geo, scales = "free_y") + 
  scale_x_continuous(breaks = seq(2010, 2022, by = 2)) +
  theme_minimal() +  
  labs(
    title = "Evolution of Recycling Volumes Over Time",
    x = "Year",
    y = "Recycling Volume (in ln)"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


##scatterplots for plastic overall

## scatterplot recycling and production by country

#withln and index2015
timeline_lnrecycling_production2015 <- ggplot(merged_df, aes(x=lnrecycling_tonne, y = production_index2015)) +
  geom_point() +
  geom_line()+
  facet_wrap(~geo) +
  theme_minimal()

#withoutln and index2015
timeline_recycling_production2015 <-ggplot(merged_df, aes(x=recycling_tonne, y = production_index2015)) +
  geom_point() +
  geom_line() +
  facet_wrap(~geo) +
  theme_minimal()

# withoutln and percent
timeline_recycling_productionperc <-ggplot(merged_df, aes(x=recycling_tonne, y = production_percent)) +
  geom_point() +
  geom_line() +
  facet_wrap(~geo) +
  theme_minimal()

#withln and percent
timeline_lnrecycling_productionperc <-ggplot(merged_df, aes(x=lnrecycling_tonne, y = production_percent)) +
  geom_point() +
  geom_line() +
  facet_wrap(~geo) +
  theme_minimal()


## scatterplot recycling and production packaging by country

#withln and index2015
timeline_packaging_lnrecycling_production2015 <- ggplot(merged_packaging_df, aes(x=lnrecycling_tonne, y = production_index2015)) +
  geom_point() +
  geom_line()+
  facet_wrap(~geo) +
  theme_minimal()

#without ln and index2015
timeline_packaging_recycling_production2015 <- ggplot(merged_packaging_df, aes(x=recycling_tonne, y = production_index2015)) +
  geom_point() +
  geom_line()+
  facet_wrap(~geo) +
  theme_minimal()

##analysis of correlation
# Calculate correlation for Germany using dplyr
germany_correlation <- merged_df %>%
  filter(geo == "Germany") %>%  # Filter for Germany
  summarise(correlation = cor(production_percent, lnrecycling_tonne, use = "complete.obs"))

germany_correlation_inpercent <- merged_df %>%
  filter(geo == "Germany") %>%  # Filter for Germany
  summarise(correlation = cor(production_percent, recycling_percent, use = "complete.obs"))

print(germany_correlation)
print(germany_correlation_inpercent)

#scatterplot for Germany with OLS line
merged_df %>%
  filter(geo == "Germany") %>%
  ggplot(aes(x = production_index2015, y = lnrecycling_tonne)) +
  geom_point(color = "blue", alpha = 0.7) +  
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  theme_minimal() +
  labs(
    title = "Correlation between Plastic Production and Recycling in Germany",
    x = "Plastic Production (index2015)",
    y = "Log of Recycling Volume"
  )

merged_df %>%
  filter(geo == "Germany") %>%
  ggplot(aes(x = production_percent, y = lnrecycling_tonne)) +
  geom_point(color = "blue", alpha = 0.7) +  
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  theme_minimal() +
  labs(
    title = "Correlation between Plastic Production and Recycling in Germany",
    x = "Plastic Production (%change)",
    y = "Log of Recycling Volume"
  )

merged_df %>%
  filter(geo == "Germany") %>%
  ggplot(aes(x = production_percent, y = recycling_percent)) +
  geom_point(color = "blue", alpha = 0.7) +  
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  theme_minimal() +
  labs(
    title = "Correlation between Plastic Production and Recycling in Germany",
    x = "Plastic Production (%change)",
    y = "Recycling Volume (%change)"
  )

## fixed effect reg using least squares dummy variable model from https://www.princeton.edu/~otorres/Panel101R.pdf
fixed.dum <-lm(production_index2015 ~ lnrecycling_tonne + factor(geo) - 1, data=merged_df)
summary(fixed.dum)

fixed.dum.noTurkey <- lm(production_index2015 ~ lnrecycling_tonne + factor(geo) - 1, data=merged_df_noT)
summary(fixed.dum.noTurkey)

# predicted outcome

production_predicted <- fixed.dum$fitted

#plot results
scatterplot(merged_df$production_index2015 ~ merged_df$lnrecycling_tonne|merged_df$geo, boxplots=FALSE, xlab="recycling", ylab="predicted production",smooth=FALSE)
scatterplot(merged_df$production_index2015 ~ merged_df$recycling_tonne|merged_df$geo, boxplots=FALSE, xlab="recycling", ylab="predicted production",smooth=FALSE)
scatterplot(merged_df1$production_index2015 ~ merged_df1$recycling_tonne|merged_df1$geo, boxplots=FALSE, xlab="recycling", ylab="predicted production",smooth=FALSE)


# load use tables data to compute elasticities of demand. Problem is that use table data is annual, whereas price data is monthly. If I take yearly values I have barely matching observations.
use_table_domestic <- read.csv("estat_naio_10_cp1610_usetable_plastic_domestic.csv", header = TRUE) #use tables of plastic products for all industries domestic
use_table_total <- read.csv("estat_naio_10_cp1610_usetable_plastic_total.csv", header = TRUE) #use tables of plastic products for all industries domestic+imports

use_table_domestic <- use_table_domestic %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                                       "freq", "ind_use", "stk_flow", 
                                                       "prd_ava",  "CONF_STATUS"))

use_table_total <- use_table_total %>% select(-c("DATAFLOW", "LAST.UPDATE", 
                                                 "freq", "ind_use", "stk_flow", 
                                                 "prd_ava",  "CONF_STATUS"))

use_table_domestic  <- use_table_domestic  %>% rename("plastic_mioEUR_dom" = "OBS_VALUE", 
                                                      "OBS_FLAG_usedom" = "OBS_FLAG")

use_table_total  <- use_table_total  %>% rename("plastic_mioEUR_tot" = "OBS_VALUE", 
                                                "OBS_FLAG_usetot" = "OBS_FLAG")

use_table_domestic <- use_table_domestic %>%
  filter(geo %in% c("European Union - 27 countries (from 2020)", "Euro area – 20 countries (from 2023)", "Euro area - 19 countries  (2015-2022)")) 

## regress production on price change in secondary prices (take log of prices). Absolutely horrible model fit
## Add covariates: GDP and environmental taxes

model1_prices <- lm(production_percentchange ~ log(RHDPE), data = merged_price_data, na.action = na.exclude)
summary(model1_prices)


#alternatively: waste data, but this is only collected on an annual basis

## load import. Imports of waste, parings and scrap of plastic, from the extra EU area into the EU area. 

plastic_imports_EU <- read.csv("estat_plastic_imports_ EU.csv", row.names = NULL)
plastic_imports_EU <- plastic_imports_EU %>% select(-c("row.names", "DATAFLOW", "LAST.UPDATE", "reporter", "partner", "flow", "product", "freq", "OBS_VALUE"))

colnames(plastic_imports_EU)[c(1, 2)] <- c("TIME_PERIOD", "imports_100kg")

plastic_imports_EU <- plastic_imports_EU %>%
  mutate(
    import_tonnes = imports_100kg / 10
  )