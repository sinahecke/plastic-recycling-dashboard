library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(stargazer)
library(GGally)

##plotting

##descriptives

#density plots

#pairs plot for plastic sample

ggpairs(merged_df[, c("recycling_tonne", "recycling_rate", "production_index2015", "plastic_generation")],
        upper = list(continuous = wrap("cor", size = 5)), 
        lower = list(continuous = "smooth"),
        diag = list(continuous = "densityDiag"),
        title = "Pairs plot of plastic sample",
        columnLabels = c("Recycling", "Recycling Rate", "Production", "Waste Gen."),
        )

plastic_pairs_plot <- merged_df %>%
  mutate(
    recycling = log(recycling_tonne), 
    recycling_rate = log(recycling_rate), 
    production_index2015 = production_index2015,
    plastic_generation = log(plastic_generation),
  ) %>%
  select(recycling, recycling_rate, production_index2015, plastic_generation)

ggpairs(plastic_pairs_plot,
        upper = list(continuous = wrap("cor", size = 5)), 
        lower = list(continuous = "smooth"),
        diag = list(continuous = "densityDiag"),
        title = "Pairs plot of plastic sample (Ln)",
        columnLabels = c("Recycling", "Recycling Rate", "Production", "Waste Gen."))


# pairs plot for packaging sample
ggpairs(merged_packaging_df[, c("recycling_tonne", "recycling_rate", "production_index2015", "packaging_generation")],
        upper = list(continuous = wrap("cor", size = 5)), 
        lower = list(continuous = "smooth"),
        diag = list(continuous = "densityDiag"),
        title = "Pairs plot of packaging sample",
        columnLabels = c("Recycling", "Recycling Rate", "Production", "Waste Gen."),
)

packaging_pairs_plot <- merged_packaging_df %>%
  mutate(
    recycling = log(recycling_tonne), 
    recycling_rate = log(recycling_rate), 
    production_index2015 = production_index2015,
    plastic_generation = log(packaging_generation),
  ) %>%
  select(recycling, recycling_rate, production_index2015, packaging_generation)

ggpairs(plastic_pairs_plot,
        upper = list(continuous = wrap("cor", size = 5)), 
        lower = list(continuous = "smooth"),
        diag = list(continuous = "densityDiag"),
        title = "Pairs plot of plastic sample (Ln)",
        columnLabels = c("Recycling", "Recycling Rate", "Production", "Waste Gen."))

# Plot the density distribution of recycling
plot(
  density(pdata$recycling_tonne), 
  main="Density Plot of Plastic Recycling Quantity",
  xlab="Recycling Quantity", ylab="Density"
)

plot(
  density(log(pdata$recycling_tonne)), 
  main="Density Plot of Plastic Recycling Quantity (Ln)",
  xlab="Recycling Quantity", ylab="Density"
)

# Plot the density distribution of production
plot(
  density(pdata$production_index2015), 
  main="Density Plot of Plastic Production Quantity",
  xlab="Production (Index = 2015)", ylab="Density"
)

plot(
  density(pdata$plastic_generation), 
  main="Density Plot of Plastic Waste Generation",
  xlab="Generation Quantity", ylab="Density"
)

plot(
  density(panel_packaging_df$recycling_tonne), 
  main="Density Plot of Plastic Packaging Recycling Quantity",
  xlab="Recycling Quantity", ylab="Density"
)

plot(
  density(log(panel_packaging_df$recycling_tonne)), 
  main="Density Plot of Plastic Packaging Recycling Quantity (Ln)",
  xlab="Recycling Quantity", ylab="Density"
)

plot(
  density(panel_packaging_df$plasticproduction_index2015), 
  main="Density Plot of Plastic Packaging Production Quantity",
  xlab="Production (Index = 2015)", ylab="Density"
)

plot(
  density(log(panel_packaging_df$plastic_generation)), 
  main="Density Plot of Plastic Waste Generation (Ln)",
  xlab="Generation Quantity", ylab="Density"
)

#plot my control variables
plot(
  density(log(merged_df$env_tax_mioEUR)), 
  main="Density Plot of Environmental Taxes (Ln)",
  xlab="Taxes (in mio EUR)", ylab="Density"
)

plot(
  density(merged_df$GDP_index2015), 
  main="Density Plot of GDP",
  xlab="GDP (Index 2015 = 100)", ylab="Density"
)

##using ExPanDaR package to do descriptive analysis of panel data

violin_graph <- prepare_by_group_violin_graph(merged_df, by_var = "geo", var = "recycling_tonne",
                                     order_by_mean = TRUE)
print(violin_graph)

t <- prepare_correlation_table(plastic_pairs_plot, bold = 0.01, format="html")

t$kable_ret %>%
  kable_styling("condensed", full_width = F, position = "center")

# QQ plots to inspect normality. Systematic Patterns: Look for clear trends, 
# such as clusters or s-shaped curves, which suggest skewness or heavy tails. 
# Skewed Data: Points systematically deviate from the line, curving upward or downward.
#Heavy Tails: Points deviate at the extremes (ends) of the distribution.

qqnorm(merged_df$recycling_tonne, pch = 1, frame = FALSE)
qqline(merged_df$recycling_tonne, col = "steelblue", lwd = 2)

qqnorm(log(merged_df$recycling_tonne), pch = 1, frame = FALSE)
qqline(log(merged_df$recycling_tonne), col = "steelblue", lwd = 2)

qqnorm(merged_df$production_index2015, pch = 1, frame = FALSE)
qqline(merged_df$production_index2015, col = "steelblue", lwd = 2)

qqnorm(merged_packaging_df$recycling_tonne, pch = 1, frame = FALSE)
qqline(merged_packaging_df$recycling_tonne, col = "steelblue", lwd = 2)

qqnorm(merged_packaging_df$recycling_index, pch = 1, frame = FALSE)
qqline(merged_packaging_df$recycling_index, col = "steelblue", lwd = 2)

qqnorm(log(merged_packaging_df_noG$recycling_tonne), pch = 1, frame = FALSE)
qqline(log(merged_packaging_df_noG$recycling_tonne), col = "steelblue", lwd = 2)

qqnorm(merged_packaging_df_noG$production_index2015, pch = 1, frame = FALSE)
qqline(merged_packaging_df_noG$production_index2015, col = "steelblue", lwd = 2)

##--> perhaps remove greece as an outlier

merged_packaging_df_noG <- merged_packaging_df %>%
  filter(geo %in% c("Germany", "Spain", "France", "Italy"))

#plot production and recycling in index form on one plot (plastic)

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

#plot production and recycling in index form on one plot (packaging)
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

#scatterplots

##production and recycling plastic sample overall and by country
ggplot(merged_df, aes(x = log(recycling_tonne), y = production_index2015, color = geo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "solid") +  # country-specific lines
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8, aes(group = 1), alpha = 0.3, linetype = "dashed") + # overall line
  ggtitle("Scatterplot of plastic production \nover plastic recycling") +
  labs(
       x = "Recycling (Ln)",
       y = "Production", 
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "bottom") 

# production and recycling packaging sample overall and by country
ggplot(merged_packaging_df, aes(x = log(recycling_tonne), y = production_index2015, color = geo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "solid") +  # country-specific lines
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8, aes(group = 1), alpha = 0.3, linetype = "dashed") + # overall line
  ggtitle("Scatterplot of plastic production \nover packaging recycling") +
  labs(
       x = "Recycling (Ln)",
       y = "Production", 
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "bottom") 

#  production and recycling rate plastic sample overall and by country
ggplot(merged_df, aes(x = log(recycling_rate), y = production_index2015, color = geo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "solid") +  # country-specific lines
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8, aes(group = 1), alpha = 0.3, linetype = "dashed") + # overall line
  ggtitle("Scatterplot of plastic production \nover the plastic recycling rate") +
  labs(
       x = "Recycling Rate (Ln)",
       y = "Production", 
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "bottom") 

#  production and recycling rate packaging sample overall and by country
ggplot(merged_packaging_df, aes(x = log(recycling_rate), y = production_index2015, color = geo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "solid") +  # country-specific lines
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8, aes(group = 1), alpha = 0.3, linetype = "dashed") + # overall line
  ggtitle("Scatterplot of plastic production over \nthe packaging recycling rate") +
  labs(
       x = "Recycling Rate (Ln)",
       y = "Production",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "bottom")

# waste generation and recycling plastic sample overall and by country
ggplot(merged_df, aes(x = log(recycling_tonne), y = log(plastic_generation), color = geo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "solid") +  # country-specific lines
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8, aes(group = 1), alpha = 0.3, linetype = "dashed") + # overall line
  ggtitle("Scatterplot of plastic waste generation \nover plastic recycling") +
  labs(
       x = "Recycling (Ln)",
       y = "Waste Generation",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "bottom") 

# waste generation and recycling packaging sample overall and by country
ggplot(merged_packaging_df, aes(x = log(recycling_tonne), y = log(packaging_generation), color = geo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = geo), linetype = "solid") +  # country-specific lines
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8, aes(group = 1), alpha = 0.3, linetype = "dashed") + # overall line
  ggtitle("Scatterplot of packaging waste generation \nover packaging recycling") +
  labs(
       x = "Recycling (Ln)",
       y = "Waste Generation",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "bottom")

##scatterplots with ExPanDar package

prepare_scatter_plot(merged_df, x="recycling_tonne", y="production_index2015", color="geo", size = "?",  loess = 1)


##regression output

#plastic dataset. Perhaps present results from OLS as well as FGLS models. Stargazer does not support pggls models
# if I only add the coeftest coefficients, I cannot show the R2 of the models.

#this does not include the robust standard errors 
#I think I need to replace the asteriks
covariate_labels_model1 <- c("Recycling (ln)", "GDP (ln)", "Environmental Taxes (EUR)")  # for model 1
covariate_labels_model2 <- c("Recycling Rate (ln)", "GDP (ln)", "Environmental Taxes (EUR)")  # for model 2
covariate_labels_model3 <- c("Recycling (ln)", "GDP (ln)", "Environmental Taxes (EUR)")  # for model 3

#regression table for plastic models
#one sided p-values
p_onesided_plastic1 <- c(0.312, 1.269e-05, 0.8275)
p_onesided_plastic2 <- c(0.01835, 9.606e-11, 0.7851)
p_onesided_plastic3 <- c(0.032, 0.60430, 0.25619)

stargazer(m1, m2, m3, type = "html", out = "Plastic_models.html", header = FALSE,
          title = "Regression Output from FE regression models (Plastic Sample)",
          dep.var.caption = "Dependent variable",
          dep.var.labels.include = FALSE,
          column.labels=c("Production", "Production", "Waste Gen. (Ln)."),
          covariate.labels=c("Recycling (Ln)", "Recycling rate (Ln)", 
                             "GDP","Environmental taxes"),
          p = list(p_onesided_plastic1, p_onesided_plastic2, p_onesided_plastic3),
          add.lines = list(c("two-way FE", "Yes", "Yes", "Yes"), 
                           c("Model", "Within", "Within", "Within"),
                           c("R squared", "00", "00", "00"), 
                           c("Adj. R-squared", "00", "00", "00")))

#regression table for packaging models
p_onesided_packaging1 <- c(0.0021205, 0.002590, 0.643167)
p_onesided_packaging2 <- c(0.05713, 0.0002034, 0.8064954)
p_onesided_packaging3 <- c(0.000821, 1.181e-12, 0.472590)

stargazer(m1_pck, m2_pck, m3_pck, type = "html", out = "Packaging_models.html", header = FALSE,
          title = "FE regression results (Packaging Sample)",
          dep.var.caption = "Dependent variable",
          dep.var.labels.include = FALSE,
          column.labels=c("Production", "Production", "Waste Gen. (Ln)"),
          covariate.labels=c("Recycling (Ln)", "Recycling Rate (Ln)", "GDP", "Environmental taxes"),
          p = list(p_onesided_packaging1, p_onesided_packaging2, p_onesided_packaging3),
          add.lines = list(c("Two-way FE", "Yes", "Yes", "Yes"), 
                           c("Model", "Within", "Within", "Within"),
                           c("R-squared", "00", "00", "00"),
                           c("Adj. R-squared", "00", "00", "00")))

#regression table for plastic and packaging models in one. stargazer cannot handle more than 5 coeftest objects
stargazer(model1_se, model2_se, model3_se, model1_packaging_se, model2_packaging_se, model3_packaging_se, type = "html", out = "Packaging_models.html", header = FALSE,
          title = "FE regression results",
          dep.var.caption = "Dependent variable",
          dep.var.labels.include = FALSE,
          column.labels=c("Production", "Production", "Waste Generation", "Production", "Production", "Waste Generation"),
          covariate.labels=c("Recycling (Ln)", "Recycling Rate (Ln)", "GDP", "Taxes"),
          add.lines = list(c("Two-way FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), 
                           c("Model", "Within", "Within", "Within", "Within", "Within", "Within"),
                           c("R-squared", "0.576", " 0.598", "0.433", "0.299", "0.18", "0.44"), 
                           c("Adj. R-squared", "0.377", "0.409", "0.167", "0.0489", "-0.111", "0.236")))


##sensitivity check models without covariates for plastic sample
stargazer(m4, m5, m6, type = "html", out = "Plastic_sensitivitymodels.html", header = FALSE,
          title = "FE regression results (Plastic Sample) sensitivity check",
          dep.var.caption = "Dependent variable",
          dep.var.labels.include = FALSE,
          column.labels=c("Production", "Production", "Waste Gen. (Ln)"),
          covariate.labels=c("Recycling (Ln)", "Recycling Rate (Ln)"),
          add.lines = list(c("Two-way FE", "Yes", "Yes", "Yes"), 
                           c("Model", "Within", "Within", "Within")))

##sensitivity check models without covariates for packaging sample
stargazer(m4_pck, m5_pck, m6_pck, type = "html", out = "Packaging_sensitivitymodels.html", header = FALSE,
          title = "FE regression results (Packaging Sample) sensitivity check",
          dep.var.caption = "Dependent variable",
          dep.var.labels.include = FALSE,
          column.labels=c("Production", "Production", "Waste Gen. (Ln)"),
          covariate.labels=c("Recycling (Ln)", "Recycling Rate (Ln)"),
          add.lines = list(c("Two-way FE", "Yes", "Yes", "Yes"), 
                           c("Model", "Within", "Within", "Within")))

##sensitivity check models with only robust, not clustered SE for plastic sample
stargazer(m1_robust, m2_robust, m3_robust, type = "html", out = "Plastic_robustmodels.html", header = FALSE,
          title = "FE regression results (Plastic Sample) robust SE",
          dep.var.caption = "Dependent variable",
          dep.var.labels.include = FALSE,
          column.labels=c("Production", "Production", "Waste Gen. (Ln)"),
          covariate.labels=c("Recycling (Ln)", "Recycling Rate (Ln)", "GDP", "environmental tax"),
          add.lines = list(c("Two-way FE", "Yes", "Yes", "Yes"), 
                           c("Model", "Within", "Within", "Within")))


##sensitivity check models with only robust, not clustered SE for packaging sample
stargazer(m1_pck_robust, m2_pck_robust, m3_pck_robust, type = "html", out = "Packaging_robustmodels.html", header = FALSE,
          title = "FE regression resuts (Packaging Sample) robust SE",
          dep.var.caption = "Dependent variable",
          dep.var.labels.include = FALSE,
          column.labels=c("Production", "Production", "Waste Gen. (Ln)"),
          covariate.labels=c("Recycling (Ln)", "Recycling Rate (Ln)", "GDP", "environmental tax"),
          add.lines = list(c("Two-way FE", "Yes", "Yes", "Yes"), 
                           c("Model", "Within", "Within", "Within")))

#sensitivity check models without Turkey observation
p_onesided_noT1 <- c(0.10987, 0.01862, 0.65462)
p_onesided_noT2 <- c(0.16244, 0.04755, 0.61917)
p_onesided_noT3 <- c(0.20575, 0.7838, 0.2380)

stargazer(m1_noT, m2_noT, m3_noT, type = "html", out = "Plastic_models_noT.html", header = FALSE,
          title = "Regression Output from FE regression models (Plastic Sample)",
          dep.var.caption = "Dependent variable",
          dep.var.labels.include = FALSE,
          column.labels=c("Production", "Production", "Waste Gen. (Ln)."),
          covariate.labels=c("Recycling (Ln)", "Recycling rate (Ln)", 
                             "GDP","Environmental taxes"),
          p = list(p_onesided_noT1, p_onesided_noT2, p_onesided_noT3),
          add.lines = list(c("two-way FE", "Yes", "Yes", "Yes"), 
                           c("Model", "Within", "Within", "Within")))

#checking normality of standard errors

resid_m1 <- residuals(model1)
qqnorm(resid_m1, main = "Q-Q Plot of Model 1 (Plastic)")

resid_m2 <- residuals(model2)
qqnorm(resid_m2, main = "Q-Q Plot of Model 2 (Plastic)")

resid_m3 <- residuals(model3)
qqnorm(resid_m3, main = "Q-Q Plot of Model 3 (Plastic)")

resid_m1_pck <- residuals(model1_packaging)
qqnorm(resid_m1_pck, main = "Q-Q Plot of Model 1 (Packaging)")

resid_m2_pck <- residuals(model2_packaging)
qqnorm(resid_m2_pck, main = "Q-Q Plot of Model 2 (Packaging)")

resid_m3_pck <- residuals(model3_packaging)
qqnorm(resid_m3_pck, main = "Q-Q Plot of Model 3 (Packaging)")

##shapiro wilk test. Null hypothesis is normality
shapiro.test(resid_m2)

shapiro.test(resid_m3)

shapiro.test(resid_m3_pck)

##coefficient plot

models <- list(m1, m2, m3)
terms  <- c("log(recycling_tonne)", "log(recycling_rate)", "log(recycling_tonne)")
labels <- c("Production ~ Recycling Quantity", 
            "Production ~ Recycling Rate", 
            "Waste ~ Recycling Quantity")

coef_df <- map2(models, seq_along(models), ~ tidy(.x, conf.int = TRUE) %>%
                  filter(term == terms[.y]) %>%
                  mutate(model = labels[.y])) %>%
  bind_rows()

# Plotting
ggplot(coef_df, aes(x = estimate, y = model)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Effect of Recycling on Production and Waste",
       x = "Coefficient Estimate", y = NULL) +
  theme_minimal()


models_pck <- list(m1_pck, m2_pck, m3_pck)
terms  <- c("log(recycling_tonne)", "log(recycling_rate)", "log(recycling_tonne)")
labels <- c("Production ~ Recycling Quantity", 
            "Production ~ Recycling Rate", 
            "Waste ~ Recycling Quantity")

coef_df_pck <- map2(models_pck, seq_along(models), ~ tidy(.x, conf.int = TRUE) %>%
                  filter(term == terms[.y]) %>%
                  mutate(model = labels[.y])) %>%
  bind_rows()

# Plotting
ggplot(coef_df_pck, aes(x = estimate, y = model)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Effect of Packaging Recycling on Production and Waste",
       x = "Coefficient Estimate", y = NULL) +
  theme_minimal()


#waterfall plot

# Load necessary libraries
# Load necessary packages
library(tidyverse)

# Original dataframe
df <- data.frame(
  Plastic_Type = c("Secondary Plastic", "Primary Plastic"),
  Production = c(5, -3),
  Rebound = c(0, -2)
)

# Reshape and preserve the sign
df_long <- df %>%
  pivot_longer(cols = c(Production, Rebound), 
               names_to = "Metric", 
               values_to = "Value") %>%
  mutate(
    # Metric as factor with desired order
    Metric = factor(Metric, levels = c("Production", "Rebound")),
    # Order to stack correctly by reversing Rebound for negatives
    StackOrder = case_when(
      Metric == "Production" ~ 1,
      Metric == "Rebound" ~ 2
    )
  )

# Plot with correct stacking
ggplot(df_long, aes(x = Plastic_Type, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(title = "",
       x = "",
       y = "") +
  scale_fill_manual(values = c("Production" = "steelblue4", "Rebound" = "navyblue")) +
  theme_minimal() +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "top",
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14))

ggplot(df_long, aes(x = Plastic_Type, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(title = "",
       x = "",
       y = "") +
  scale_fill_manual(values = c("Production" = "slategray1", "Rebound" = "darkseagreen")) +
  theme_minimal() +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "top",
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14))

ggplot(df_long, aes(x = Plastic_Type, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(title = "",
       x = "",
       y = "") +
  scale_fill_manual(values = c("Production" = "darkolivegreen", "Rebound" = "navyblue")) +
  theme_minimal() +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "top",
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14))

ggplot(df_long, aes(x = Plastic_Type, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(title = "",
       x = "",
       y = "") +
  scale_fill_manual(values = c("Production" = "steelblue4", "Rebound" = "darkseagreen")) +
  theme_minimal() +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "top",
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14))

ggplot(df_long, aes(x = Plastic_Type, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(title = "",
       x = "",
       y = "") +
  scale_fill_manual(values = c("Production" = "navyblue", "Rebound" = "azure3")) +
  theme_minimal() +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "top",
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14))


ggplot(df_long, aes(x = Plastic_Type, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(title = "",
       x = "",
       y = "") +
  scale_fill_manual(values = c("Production" = "darkolivegreen", "Rebound" = "azure3")) +
  theme_minimal() +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "top",
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14))