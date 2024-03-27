#### Final code using wrangled datasheets 
#### for data analysis and visualisation

### Libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2) 
library(gridExtra)
library(corrplot)
library(lme4)

### Load datafiles
setwd("/exports/csce/datastore/geos/groups/gcel/for_Tegan/diss_github")
anomalies <- read.csv("Data/finalanomalies.csv", header = TRUE)
data2000 <- read.csv("Data/data2000-2005.csv", header = TRUE)
data2015 <- read.csv("Data/data2015-2020.csv", header = TRUE)

# Anomalies ####

# Data wrangling ####

anomalies2015 <- anomalies %>%
  filter(year >= 2015 & year <= 2020)
data2015$week <- week(data2015$date)
merged2015x <- merge(anomalies2015, data2015, by = c("year", "week"))
merged2015 <- merged2015x %>%
  arrange(date)

anomalies2000 <- anomalies %>%
  filter(year >= 2000 & year <= 2005)
data2000$week <- week(data2000$date)
merged2000x <- merge(anomalies2000, data2000, by = c("year", "week"))
merged2000 <- merged2000x %>%
  arrange(date)

fully_merged <- rbind(merged2000, merged2015)
View(fully_merged_summer)
fully_merged_summer <- fully_merged %>%
  filter(week >= 18 & week <= 36) %>%
  select(!c(X.1, order, day, doy, X.x, X.y))

fully_merged_long <- fully_merged_summer %>%
  pivot_longer(cols = c(sm_z_scores, temp_z_scores),
               names_to = "driver",
               values_to = "zscore")

fully_merged_long$driver <- as.factor(fully_merged_long$driver)


# Subset data for drought years and non-drought years
drought_years <- fully_merged_summer %>% filter(year %in% c(2003, 2018))
non_drought_years <- fully_merged_summer %>% filter(!year %in% c(2003, 2018))
drought_years2 <- fully_merged %>% filter(year %in% c(2003, 2018))
non_drought_years2 <- fully_merged %>% filter(!year %in% c(2003, 2018))

# Calculate average values for non-drought years by week
average_values <- non_drought_years %>%
  group_by(week) %>%
  summarise(avg_tempAnomaly = mean(temp_z_scores),
            avg_smAnomaly = mean(sm_z_scores),
            avg_mod_gpp = mean(mod_gpp))

average_drought_values <- drought_years %>%
  group_by(week) %>%
  summarise(avg_tempAnomaly = mean(temp_z_scores),
            avg_smAnomaly = mean(sm_z_scores),
            avg_mod_gpp = mean(mod_gpp))

# Merge the average values back into the dataset
fully_merged_summer2 <- fully_merged_summer %>%
  left_join(average_drought_values, by = "week")


## Regression analysis ####

# Multiple linear regression
model_both <- lm(mod_gpp ~ temp_z_scores + sm_z_scores, data = fully_merged_summer)
summary(model_both)
plot(model_both)

# Model for the effect of temperature on GPP
model_temp <- lm(mod_gpp ~ tempAnomaly, data = fully_merged_summer)
summary(model_temp)
plot(model_temp)

# Model for the effect of soil moisture on GPP
model_sm <- lm(mod_gpp ~ smAnomaly, data = fully_merged_summer)
summary(model_sm)

# Mixed effect with random effect of time as year


mixed_model <- lmer(mod_gpp ~ smAnomaly + tempAnomaly + (1 | year), data = fully_merged_summer)
summary(mixed_model)
mixed_modelSM <- lmer(mod_gpp ~ smAnomaly + (1 | year), data = fully_merged_summer)
summary(mixed_modelSM)




# Calculate correlation coefficients
correlation_matrix <- cor(fully_merged_summer[c("temp_z_scores", "sm_z_scores", "mod_gpp")])

# Print correlation coefficients
print(correlation_matrix)
# Visualize correlation matrix
corrplot(correlation_matrix, method = "circle", type = "upper", tl.cex = 0.8)

# Visualise 

anomaly_colour_palette <- c("#00BA38", "darkgreen", "deepskyblue", "blue3", "orangered", "orange2")

# SUMMER anomaly timeseries
ggplot() +
  # geom_ribbon(data = fully_merged_summer, aes(ymin = mod_gpp - mod_gpp_unc95, ymax = mod_gpp + mod_gpp_unc95 ), fill = "#5D1CAD", alpha = 0.3) +
  geom_line(data = average_drought_values, aes(x = week, y = avg_tempAnomaly, colour = "T Anomaly (Drought)")) +
  geom_line(data = average_drought_values, aes(x = week, y = avg_smAnomaly, colour = "SM Anomaly (Drought)")) +
  # geom_line(data = average_drought_values, aes(x = week, y = avg_mod_gpp, colour = "GPP (Drought)")) +
  geom_line(data = average_values, aes(x = week, y = avg_tempAnomaly, colour = "T Anomaly (Non-Drought)"), linetype = "dashed") +
  geom_line(data = average_values, aes(x = week, y = avg_smAnomaly, colour = "SM Anomaly (Non-Drought)"), linetype = "dashed") +
  # geom_line(data = average_values, aes(x = week, y = avg_mod_gpp, colour = "GPP (Non-Drought)"), linetype = "dashed") +
  geom_hline(yintercept = 0, size = 0.4, colour = "black") +
  labs(x = "Week", y = "Value", colour = "Variable") +
  scale_colour_manual(values = anomaly_colour_palette) +
  scale_x_continuous(breaks = c(18, 22, 27, 32, 36),
                     labels = c("May", "June", "Jul", "Aug", "Sep")) +
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 9), 
        legend.text = element_text(size = 11))


### Plotting anomaly relationships ####

smcor <- cor(fully_merged_summer$mod_gpp, fully_merged_summer$sm_z_scores, use = "complete.obs")
tempcor <- cor(fully_merged_summer$mod_gpp, fully_merged_summer$temp_z_scores, use = "complete.obs")

# Square the correlation coefficient to get R^2
sm_r_squared <- smcor^2
temp_r_squared <- tempcor^2

# Print the values
print(paste("R^2 value:", round(sm_r_squared, 3)))


rsquared <- function(x, y) {
  lm_model <- lm(y ~ x)
  summary(lm_model)$r.squared
}

rsquared_values <- fully_merged_long %>%
  group_by(driver) %>%
  summarise(r_squared = rsquared(zscore, mod_gpp))


anomaly_cor_gpp <- ggplot(fully_merged_long, aes(x = zscore, y = mod_gpp, colour = driver)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_colour_manual(
    values = c("#1ea7f7b1", "#f2a60ebb"), 
    labels = c("sm z-score", "temp z-score")) +
  labs(x = "Z-score", 
       y = "GPP (gC/m²/day)", 
       colour = "Driver") +
  # geom_text(aes(x = -3, y = 15), 
            # label = paste("R =", round(gpprmse2000, 2)), 
            # hjust = 0, vjust = 1,
            # size = 4, 
            # colour = "#5D1CAD") +
  theme(legend.position = "right", 
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9), 
    legend.text = element_text(size = 11)) +
  stat_smooth(method = "lm", se = FALSE)

plot(anomaly_cor_gpp)

ggsave("anomalies_correlation_gpp.png", path = "Plots", plot = anomaly_cor_gpp, width = 8, height = 5, dpi = 500)


# add R squared values next to the lines

library(ggplot2)
library(dplyr)

# Example data
# fully_merged_long <- data.frame(zscore = rnorm(100), mod_gpp = rnorm(100), driver = rep(c("sm_z_scores", "temp_z_scores"), 50))

# Calculate the linear models
lm_sm1 <- lm(mod_gpp ~ sm_z_scores, data = fully_merged_summer)
lm_temp1 <- lm(mod_gpp ~ temp_z_scores, data = fully_merged_summer)

summary(lm_sm1)
summary(lm_temp1)


# Extract R-squared values and p-values 
rsquared_sm <- summary(lm_sm1)$r.squared
rsquared_temp <- summary(lm_temp1)$r.squared

pvalue_sm <- summary(lm_sm1)$coefficients[2, "Pr(>|t|)"]
pvalue_temp <- summary(lm_temp1)$coefficients[2, "Pr(>|t|)"]


# Create the ggplot
ggplot(fully_merged_long, aes(x = zscore, y = mod_gpp, colour = driver)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, aes(group = driver)) +
  scale_colour_manual(values = c("dodgerblue", "orange")) +
  labs(
    x = "Z-score",
    y = "GPP (gC/m²/day)",
    colour = "Driver"
  ) +
  theme(
    legend.position = "right",
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    legend.text = element_text(size = 11)
  ) +
  stat_smooth(method = "lm", se = FALSE) 


# RQ1: Modelling ecosystem productivity response to 2 major drought events ####
# a) plotting timeseries of modelled and obs GPP over time (5 years) 
# ALSO need to include my calculations of annual GPP here!

gppdrought2003 <- ggplot(data2000, aes(x = day)) +
  geom_ribbon(aes(ymin = obs_gpp - obs_gpp_unc, ymax = obs_gpp + obs_gpp_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_gpp - mod_gpp_unc95, ymax = mod_gpp + mod_gpp_unc95 ), fill = "#5D1CAD", alpha = 0.3) +
  geom_line(aes(y = mod_gpp, colour = "Mod"), linewidth = 0.6) +
  geom_point(aes(y = obs_gpp, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "GPP (gC/m²/day)", colour = "Data:") +
  scale_color_manual(values = c("Mod" = "#5D1CAD", "Obs" = "#FF730085", "Obs unc" ="#FF73001F")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_x_continuous(breaks = c(182, 553, 917, 1281, 1645, 2009), 
                     labels = c("2000", "2001", "2002", "2003", "2004", "2005"),
                     expand = c(0, 0),
                     limits = c(0,2184)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,17))

gppdrought2018 <- ggplot(data2015, aes(x = day)) +
  geom_ribbon(aes(ymin = obs_gpp - obs_gpp_unc, ymax = obs_gpp + obs_gpp_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_gpp - mod_gpp_unc95, ymax = mod_gpp + mod_gpp_unc95 ), fill = "#5D1CAD", alpha = 0.3) +
  geom_line(aes(y = mod_gpp, colour = "Mod"), linewidth = 0.6) +
  geom_point(aes(y = obs_gpp, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "GPP (gC/m²/day)", colour = "Data:") +
  scale_color_manual(values = c("Mod" = "#5D1CAD", "Obs" = "#FF730085", "Obs unc" ="#FF73001F")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_x_continuous(breaks = c(182, 553, 917, 1281, 1645, 2009), 
                     labels = c("2015", "2016", "2017", "2018", "2019", "2020"),
                     expand = c(0, 0),
                     limits = c(0,2184)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,17))

laidrought2003 <- ggplot(data2000, aes(x = day)) +
  geom_ribbon(aes(ymin = obs_lai - obs_lai_unc, ymax = obs_lai + obs_lai_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_lai - mod_lai_unc95, ymax = mod_lai + mod_lai_unc95 ), fill = "#5D1CAD", alpha = 0.3) +
  geom_line(aes(y = mod_lai, colour = "Mod"), size = 0.6) +
  geom_point(aes(y = obs_lai, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "LAI (m² / m²)", colour = "Data:") +
  scale_color_manual(values = c("Mod" = "#5D1CAD", "Obs" = "#FF730085", "Obs unc" ="#FF73001F")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_x_continuous(breaks = c(182, 553, 917, 1281, 1645, 2009), 
                     labels = c("2000", "2001", "2002", "2003", "2004", "2005"),
                     expand = c(0, 0),
                     limits = c(0,2184)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,8))

laidrought2018 <- ggplot(data2015, aes(x = day)) +
  geom_ribbon(aes(ymin = obs_lai - obs_lai_unc, ymax = obs_lai + obs_lai_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_lai - mod_lai_unc95, ymax = mod_lai + mod_lai_unc95 ), fill = "#5D1CAD", alpha = 0.3) +
  geom_line(aes(y = mod_lai, colour = "Mod"), size = 0.6) +
  geom_point(aes(y = obs_lai, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "LAI (m² / m²)", colour = "Data:") +
  scale_color_manual(values = c("Mod" = "#5D1CAD", "Obs" = "#FF730085", "Obs unc" ="#FF73001F")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_x_continuous(breaks = c(182, 553, 917, 1281, 1645, 2009), 
                     labels = c("2015", "2016", "2017", "2018", "2019", "2020"),
                     expand = c(0, 0),
                     limits = c(0,2184)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,8))

recodrought2003 <- ggplot(data2000, aes(x = day)) +
  geom_ribbon(aes(ymin = obs_reco - obs_reco_unc, ymax = obs_reco + obs_reco_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_reco - mod_reco_unc95, ymax = mod_reco + mod_reco_unc95 ), fill = "#5D1CAD", alpha = 0.3) +
  geom_line(aes(y = mod_reco, colour = "Mod"), size = 0.6) +
  geom_point(aes(y = obs_reco, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "Reco (gC/m²/day)", colour = "Data:") +
  scale_color_manual(values = c("Mod" = "#5D1CAD", "Obs" = "#FF730085", "Obs unc" ="#FF73001F")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_x_continuous(breaks = c(182, 553, 917, 1281, 1645, 2009), 
                     labels = c("2000", "2001", "2002", "2003", "2004", "2005"),
                     expand = c(0, 0),
                     limits = c(0,2184)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,8))

recodrought2018 <- ggplot(data2015, aes(x = day)) +
  geom_ribbon(aes(ymin = obs_reco - obs_reco_unc, ymax = obs_reco + obs_reco_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_reco - mod_reco_unc95, ymax = mod_reco + mod_reco_unc95 ), fill = "#5D1CAD", alpha = 0.3) +
  geom_line(aes(y = mod_reco, colour = "Mod"), size = 0.6) +
  geom_point(aes(y = obs_reco, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "Reco (gC/m²/day)", colour = "Data:") +
  scale_color_manual(values = c("Mod" = "#5D1CAD", "Obs" = "#FF730085", "Obs unc" ="#FF73001F")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_x_continuous(breaks = c(182, 553, 917, 1281, 1645, 2009), 
                     labels = c("2015", "2016", "2017", "2018", "2019", "2020"),
                     expand = c(0, 0),
                     limits = c(0,2184)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,8))

plot(gppdrought2003)
plot(gppdrought2018)
plot(laidrought2003)
plot(laidrought2018)
plot(recodrought2003)
plot(recodrought2018)

ggsave("gpp2000_timeseries.png", path = "Plots", plot = gppdrought2003, width = 7, height = 5, dpi = 500)
ggsave("gpp2015_timeseries.png", path = "Plots", plot = gppdrought2018, width = 7, height = 5, dpi = 500)
ggsave("lai2000_timeseries.png", path = "Plots", plot = laidrought2003, width = 7, height = 5, dpi = 500)
ggsave("lai2015_timeseries.png", path = "Plots", plot = laidrought2018, width = 7, height = 5, dpi = 500)
ggsave("reco2000_timeseries.png", path = "Plots", plot = recodrought2003, width = 7, height = 5, dpi = 500)
ggsave("reco2015_timeseries.png", path = "Plots", plot = recodrought2018, width = 7, height = 5, dpi = 500)

# b) RMSE // R squared calculations and plotting correlation! -> appendix?

gppcorrelation2000 <- cor(data2000$mod_gpp, data2000$obs_gpp, use = "complete.obs")
obs_gpp2000 <- data2000$obs_gpp[!is.na(data2000$obs_gpp)]

# Square the correlation coefficient to get R^2
gppr_squared2000 <- gppcorrelation2000^2

gpprmse2000 <- sqrt(mean((data2000$obs_gpp - data2000$mod_gpp)^2, na.rm = TRUE))
lairmse2000 <- sqrt(mean((data2000$obs_lai - data2000$mod_lai)^2, na.rm = TRUE))
recormse2000 <- sqrt(mean((data2000$obs_reco - data2000$mod_reco)^2, na.rm = TRUE))

gpprelative_rmse2000 <- gpprmse2000 / (max(obs_gpp2000) - min(obs_gpp2000))

# Print the values
print(paste("R^2 value:", round(gppr_squared2000, 3)))
print(gpprmse2000)
print(gpprelative_rmse2000)

# Plot the relationships
gppcorrelation2000 <- ggplot(data2000, aes(x = mod_gpp, y = obs_gpp)) +
  geom_point(colour = "#5A00C75E") +
  labs(x = "Modelled GPP₂₀₀₃ (gC/m²/day)", y = "Observed GPP₂₀₀₃ (gC/m²/day)") +
  geom_abline(intercept = 0, slope = 1, color = "#5D1CAD", size = 0.6) +
  # geom_abline(intercept = 0, slope = max(data2000$obs_gpp, na.rm = TRUE) / max(data2000$mod_gpp, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 0, 
                y = 13), 
            label = paste("RMSE =", round(gpprmse2000, 2)), 
            hjust = 0, vjust = 1,
            size = 4, 
            fontface = "bold", 
            colour = "#5D1CAD") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9)) +
  scale_x_continuous(limits = c(0,16)) +
  scale_y_continuous(limits = c(0,16))

plot(gppcorrelation2000)

laicorrelation2000 <- ggplot(data2000, aes(x = mod_lai, y = obs_lai)) +
  geom_point(colour = "#059C0093") +
  labs(x = "Modelled LAI₂₀₀₃ (m² / m²)", y = "Observed LAI₂₀₀₃ (m² / m²)") +
  geom_abline(intercept = 0, slope = 1, color = "darkgreen", size = 0.6) +
  # geom_abline(intercept = 0, slope = max(data2000$obs_lai, na.rm = TRUE) / max(data2000$mod_lai, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 0, 
                y = 6), 
            label = paste("RMSE =", round(lairmse2000, 2)), 
            hjust = 0, vjust = 1,
            size = 4, 
            fontface = "bold", 
            colour = "darkgreen") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9)) +
  scale_x_continuous(limits = c(0,6)) +
  scale_y_continuous(limits = c(0,7))

plot(laicorrelation2000)

recocorrelation2000 <- ggplot(data2000, aes(x = mod_reco, y = obs_reco)) +
  geom_point(colour = "#eb1d129b") +
  labs(x = "Modelled Reco₂₀₀₃ (gC/m²/day)", y = "Observed Reco₂₀₀₃ (gC/m²/day)") +
  geom_abline(intercept = 0, slope = 1, color = "#ba0d04e5", size = 0.6) +
  # geom_abline(intercept = 0, slope = max(data2000$obs_reco, na.rm = TRUE) / max(data2000$mod_reco, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 0, 
                y = 6.5), 
            label = paste("RMSE =", round(recormse2000, 2)), 
            hjust = 0, vjust = 1,
            size = 4, 
            fontface = "bold", 
            colour = "#ba0d04e5") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9)) +
  scale_x_continuous(limits = c(0,6)) +
  scale_y_continuous(limits = c(0,8))

plot(recocorrelation2000)


## Same for 2015-2020
# Linear model (R squared; correlation test)
obs_gpp2015 <- data2015$obs_gpp[!is.na(data2015$obs_gpp)]
# Calculate correlation coefficient
gppcorrelation2015 <- cor(data2015$mod_gpp, data2015$obs_gpp, use = "complete.obs")
# Square the correlation coefficient to get R^2
gppr_squared2015 <- gppcorrelation2015^2

gpprmse2015 <- sqrt(mean((data2015$obs_gpp - data2015$mod_gpp)^2, na.rm = TRUE))
lairmse2015 <- sqrt(mean((data2015$obs_lai - data2015$mod_lai)^2, na.rm = TRUE))
recormse2015 <- sqrt(mean((data2015$obs_reco - data2015$mod_reco)^2, na.rm = TRUE))

gpprelative_rmse2015 <- 1.338 / (max(obs_gpp2015) - min(obs_gpp2015))

# Print the values
print(paste("R^2 value:", round(gppr_squared2015, 3)))
print(gpprmse2015)
print(gpprelative_rmse2015)

# Plot the relationships
gppcorrelation2015 <- ggplot(data2015, aes(x = mod_gpp, y = obs_gpp)) +
  geom_point(colour = "#5A00C75E") +
  labs(x = "Modelled GPP₂₀₁₈ (gC/m²/day)", y = "Observed GPP₂₀₁₈ (gC/m²/day)") +
  geom_abline(intercept = 0, slope = 1, color = "#5D1CAD", size = 0.6) +
  # geom_abline(intercept = 0, slope = max(data2015$obs_gpp, na.rm = TRUE) / max(data2015$mod_gpp, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 0, 
                y = 13), 
            label = paste("RMSE =", round(gpprmse2015, 2)), 
            hjust = 0, vjust = 1,
            size = 4, 
            fontface = "bold", 
            colour = "#5D1CAD") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9)) +
  scale_x_continuous(limits = c(0,16)) +
  scale_y_continuous(limits = c(0,16))

plot(gppcorrelation2015)

laicorrelation2015 <- ggplot(data2015, aes(x = mod_lai, y = obs_lai)) +
  geom_point(colour = "#059C0093") +
  labs(x = "Modelled LAI₂₀₁₈ (m² / m²)", y = "Observed LAI₂₀₁₈ (m² / m²)") +
  geom_abline(intercept = 0, slope = 1, color = "darkgreen", size = 0.6) +
  # geom_abline(intercept = 0, slope = max(data2015$obs_lai, na.rm = TRUE) / max(data2015$mod_lai, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 0, 
                y = 6), 
            label = paste("RMSE =", round(lairmse2015, 2)), 
            hjust = 0, vjust = 1,
            size = 4, 
            fontface = "bold", 
            colour = "darkgreen") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9))+
  scale_x_continuous(limits = c(0,6)) +
  scale_y_continuous(limits = c(0,7))

plot(laicorrelation2015)

recocorrelation2015 <- ggplot(data2015, aes(x = mod_reco, y = obs_reco)) +
  geom_point(colour = "#eb1d129b") +
  labs(x = "Modelled Reco₂₀₁₈ (gC/m²/day)", y = "Observed Reco₂₀₁₈ (gC/m²/day)") +
  geom_abline(intercept = 0, slope = 1, color = "#ba0d04e5", size = 0.6) +
  # geom_abline(intercept = 0, slope = max(data2015$obs_reco, na.rm = TRUE) / max(data2015$mod_reco, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 0, 
                y = 6.5), 
            label = paste("RMSE =", round(recormse2015, 2)), 
            hjust = 0, vjust = 1,
            size = 4, 
            fontface = "bold", 
            colour = "#ba0d04e5") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9)) +
  scale_x_continuous(limits = c(0,6)) +
  scale_y_continuous(limits = c(0,8))

plot(recocorrelation2015)

# combine the correlation plots
combined_cor_plots <- grid.arrange(
  gppcorrelation2000, recocorrelation2000, laicorrelation2000, 
  gppcorrelation2015, recocorrelation2015, laicorrelation2015, 
  nrow = 2, 
  layout_matrix = rbind(c(1,2,3), c(4,5,6)), 
  heights = c(1,1))


ggsave("correlation_plots.png", path = "Plots", plot = combined_cor_plots, width = 10, height = 7, dpi = 500)


# Statistical test to assess whether gpp is significantly different by year:

lm_annualgpp2000 <- lm(mod_gpp ~ mod_lai, data = data2000)
lm_annualgpp2015 <- lm(mod_gpp ~ obs_gpp, data = data2015)
lm_annualgppboth <- lm(mod_gpp ~ year, data = combinedyears)

summary(lm_annualgpp2015)




### RQ2: Drivers vs response variables ####
# should i maybe include more drivers, such as VPD; also respiration??









### RQ3: Drivers vs response variables ####







