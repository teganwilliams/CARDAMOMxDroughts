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
data2000 <- read.csv("Data/newdata2000-2005.csv", header = TRUE)
data2015 <- read.csv("Data/newdata2015-2020.csv", header = TRUE)
datafull <- read.csv("Data/newdatafull.csv", header = TRUE)
datasim <- read.csv("validationdata.csv", header = TRUE)
met <- read.csv("droughtmetdata.csv", header = TRUE)

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

needrought2003 <- ggplot(data2000, aes(x = day)) +
  geom_ribbon(aes(ymin = obs_nee - obs_nee_unc, ymax = obs_nee + obs_nee_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_nee - mod_nee_unc95, ymax = mod_nee + mod_nee_unc95 ), fill = "#5D1CAD", alpha = 0.3) +
  geom_line(aes(y = mod_nee, colour = "Mod"), size = 0.6) +
  geom_point(aes(y = obs_nee, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "NEE (gC/m²/day)", colour = "Data:") +
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
                     limits = c(-15,8))

needrought2018 <- ggplot(data2015, aes(x = day)) +
  geom_ribbon(aes(ymin = obs_nee - obs_nee_unc, ymax = obs_nee + obs_nee_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_nee - mod_nee_unc95, ymax = mod_nee + mod_nee_unc95 ), fill = "#5D1CAD", alpha = 0.3) +
  geom_line(aes(y = mod_nee, colour = "Mod"), size = 0.6) +
  geom_point(aes(y = obs_nee, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "NEE (gC/m²/day)", colour = "Data:") +
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
                     limits = c(-15,5))

plot(gppdrought2003)
plot(gppdrought2018)
plot(laidrought2003)
plot(laidrought2018)
plot(needrought2003)
plot(needrought2018)

ggsave("gpp2000_timeseries.png", path = "Plots", plot = gppdrought2003, width = 7, height = 5, dpi = 500)
ggsave("gpp2015_timeseries.png", path = "Plots", plot = gppdrought2018, width = 7, height = 5, dpi = 500)
ggsave("lai2000_timeseries.png", path = "Plots", plot = laidrought2003, width = 7, height = 5, dpi = 500)
ggsave("lai2015_timeseries.png", path = "Plots", plot = laidrought2018, width = 7, height = 5, dpi = 500)
ggsave("nee2000_timeseries.png", path = "Plots", plot = needrought2003, width = 7, height = 5, dpi = 500)
ggsave("nee2015_timeseries.png", path = "Plots", plot = needrought2018, width = 7, height = 5, dpi = 500)


# Full run rmse values
datafull1 <- datafull %>%
  filter(year >= 2000 & year <= 2005)

gpprmse1 <- sqrt(mean((datafull1$obs_gpp - datafull1$mod_gpp)^2, na.rm = TRUE))
lairmse1 <- sqrt(mean((datafull1$obs_lai - datafull1$mod_lai)^2, na.rm = TRUE))
neermse1 <- sqrt(mean((datafull1$obs_nee - datafull1$mod_nee)^2, na.rm = TRUE))

print(gpprmse1)
print(lairmse1)
print(neermse1)

datafull2 <- datafull %>%
  filter(year >= 2015 & year <= 2020)

gpprmse2 <- sqrt(mean((datafull2$obs_gpp - datafull2$mod_gpp)^2, na.rm = TRUE))
lairmse2 <- sqrt(mean((datafull2$obs_lai - datafull2$mod_lai)^2, na.rm = TRUE))
neermse2 <- sqrt(mean((datafull2$obs_nee - datafull2$mod_nee)^2, na.rm = TRUE))

print(gpprmse2)
print(lairmse2)
print(neermse2)

# Validation (simulation)

gpprmsesim <- sqrt(mean((datasim$obs_gpp - datasim$mod_gpp_sim)^2, na.rm = TRUE))
lairmsesim <- sqrt(mean((datasim$obs_lai - datasim$mod_lai_sim)^2, na.rm = TRUE))
print(gpprmsesim)
print(lairmsesim)


# b) RMSE // R squared calculations and plotting correlation! -> appendix?

gppcorrelation2000 <- cor(data2000$mod_gpp, data2000$obs_gpp, use = "complete.obs")
obs_gpp2000 <- data2000$obs_gpp[!is.na(data2000$obs_gpp)]

# Square the correlation coefficient to get R^2
gppr_squared2000 <- gppcorrelation2000^2

gpprmse2000 <- sqrt(mean((data2000$obs_gpp - data2000$mod_gpp)^2, na.rm = TRUE))
lairmse2000 <- sqrt(mean((data2000$obs_lai - data2000$mod_lai)^2, na.rm = TRUE))
neermse2000 <- sqrt(mean((data2000$obs_nee - data2000$mod_nee)^2, na.rm = TRUE))

gpprelative_rmse2000 <- gpprmse2000 / (max(obs_gpp2000) - min(obs_gpp2000))

# Print the values
print(paste("R^2 value:", round(gppr_squared2000, 3)))
print(gpprmse2000)
print(neermse2000)
print(lairmse2000)
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
neermse2015 <- sqrt(mean((data2015$obs_nee - data2015$mod_nee)^2, na.rm = TRUE))

gpprelative_rmse2015 <- 1.338 / (max(obs_gpp2015) - min(obs_gpp2015))

# Print the values
print(paste("R^2 value:", round(gppr_squared2015, 3)))
print(neermse2015)
print(gpprmse2015) 
print(lairmse2015)
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









drivers <- read.csv("rq2data.csv", header = TRUE)
boxplot(drivers$mod_gpp, main="Boxplot of mod_gpp") # visualise data


drivers <- drivers %>%
  mutate(condition = ifelse(year %in% c(2003, 2018, 2019, 2020), "drought", "normal"))

# drought years only 
drought <- drivers %>%
  filter(year == 2003 | year == 2018 | year == 2019 | year == 2020)

non_drought <- drivers %>%
  filter(!(year %in% c(2003, 2018, 2019, 2020)))

model_mixed_all <- lmer(mod_gpp ~ maxT + sm1 + sm2 + sm3 + swr + vpd + (1|year) + (1|doy), data = drought)
summary(model_mixed_all)

residuals_mixed_all <- resid(model_mixed_all)
shapiro_test <- shapiro.test(residuals_mixed_all)
print(shapiro_test) # residuals are normally distributed
qqnorm(residuals_mixed_all) 
qqline(residuals_mixed_all) # residuals fit qq line

# same but for non-drought years

model_mixed_non <- lmer(mod_gpp ~ maxT + sm1 + sm2 + sm3 + swr + vpd + (1|year) + (1|doy), data = non_drought_clean)
summary(model_mixed_non)

residuals_mixed_non <- resid(model_mixed_non)
shapiro_test <- shapiro.test(residuals_mixed_non)
print(shapiro_test) # residuals are normally distributed
qqnorm(residuals_mixed_non) 
qqline(residuals_mixed_non) # residuals fit qq line

# remove outliers for normality
outliers <- which(residuals_mixed_non > quantile(residuals_mixed_non, 0.97) | 
                    residuals_mixed_non < quantile(residuals_mixed_non, 0.03))

# Remove extreme outliers from the data
non_drought_clean <- non_drought[-outliers, ]


# reporting results
results_table <- data.frame(
  Variable = c("maxT", "airT", "sm1", "sm2", "sm3", "swr", "precip"),
  Coefficient = c(-1.64487, 1.84993, 0.20857, -0.22874, 0.09273, 0.37563, 8860.31294),
  T_Value = c(-4.326, 4.655, 3.870, -5.918, 1.927, 6.474, 1.393),
  P_Value = c(2.31e-05, 5.60e-06, 0.000143, 1.25e-08, 0.055265, 6.17e-10, 0.164945)
)

results_table



ggplot(drivers, aes(x = maxT, y = mod_gpp, group = condition, colour = condition)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between GPP and maxT") +
  theme_minimal()

ggplot(drivers, aes(x = sm2, y = mod_gpp, group = condition, colour = condition)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between GPP and sm2") +
  theme_minimal()

ggplot(drivers, aes(x = swr, y = mod_gpp, group = condition, colour = condition)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between GPP and swr") +
  theme_minimal()

ggplot(drivers, aes(x = vpd, y = mod_gpp, group = condition, colour = condition)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between GPP and vpd") +
  theme_minimal()

ggplot(drivers, aes(x = mod_gpp, y = precip, group = condition, colour = condition)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between GPP and precip") +
  theme_minimal()

ggplot(drivers, aes(x = mod_gpp, y = sm1, group = condition, colour = condition)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between GPP and sm1") +
  theme_minimal()

ggplot(drivers, aes(x = mod_gpp, y = sm3, group = condition, colour = condition)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between GPP and sm3") +
  theme_minimal()


library(MASS)
# Apply Box-Cox transformation with lambda = 2
drivers$mod_gpp_boxcox <- drivers$mod_gpp^2

# Fit the linear model with Box-Cox transformed mod_gpp
model_lm_boxcox <- lm(mod_gpp_boxcox ~ maxT + sm1 + sm2 + sm3 + swr + vpd, data = drivers)
# Check the normality of the residuals
residuals_boxcox <- resid(model_lm_boxcox)
qqnorm(residuals_boxcox)
qqline(residuals_boxcox)

# Fit the linear mixed model with Box-Cox transformed log_mod_gpp_boxcox
model_mixed_boxcox <- lmer(mod_gpp_boxcox ~ maxT + sm1 + sm2 + sm3 + swr + vpd + (1|year) + (1|doy), data = drivers)
summary(model_mixed_boxcox)

# Check the normality of the residuals 
residuals_mixed_boxcox <- resid(model_mixed_boxcox)
qqnorm(residuals_mixed_boxcox)
qqline(residuals_mixed_boxcox)

# finding outliers
boxplot(drivers$mod_gpp, main="Boxplot of mod_gpp")

extreme_outliers <- which(residuals_mixed_boxcox > quantile(residuals_mixed_boxcox, 0.975) | 
                            residuals_mixed_boxcox < quantile(residuals_mixed_boxcox, 0.010))

# Remove extreme outliers from the data
drivers_clean <- drivers[-extreme_outliers, ]

model_mixed_boxcox_clean <- lmer(mod_gpp_boxcox ~ maxT + sm2 + sm3 + swr + vpd + (1|year) + (1|doy), data = drivers_clean)
summary(model_mixed_boxcox_clean)
residuals_mixed_boxcox_clean <- resid(model_mixed_boxcox_clean)
qqnorm(residuals_mixed_boxcox_clean)
qqline(residuals_mixed_boxcox_clean)

#  almost normality
ggplot(drivers_clean, aes(x = mod_gpp_boxcox)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  labs(title = "Distribution of mod_gpp",
       x = "mod_gpp",
       y = "Frequency") +
  theme_minimal()
shapiro_test <- shapiro.test(drivers_clean$mod_gpp_boxcox)
print(shapiro_test)

# model time!
model_mixed_boxcox_clean <- lmer(mod_gpp_boxcox ~ maxT + sm1 + sm2 + sm3 + swr + vpd + (1|year) + (1|doy), data = drivers_clean)

# Check the diagnostics
summary(model_mixed_boxcox_clean)


# mixed effect model on drivers
library(nlme)
shapiro.test()
model_mixed <- lmer(mod_gpp ~ maxT+ sm1 + sm2 + sm3 + swr + vpd +(1|year) + (1|doy), data = drivers)
model_null <- lmer(mod_gpp ~ (1|year) + (1|doy), data = drivers)

residuals_mixed <- resid(model_mixed)
qqnorm(residuals_mixed)
qqline(residuals_mixed)




summary(model_mixed)
summary(model_null)

ggplot(non_drought, aes(x = mod_gpp)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of mod_gpp",
       x = "mod_gpp",
       y = "Frequency") +
  theme_minimal()
shapiro_test <- shapiro.test(non_drought$mod_gpp)
shapiro_test <- shapiro.test(drivers$log_mod_gpp)
print(shapiro_test)




# drought years NOT INCLUDED
model_mixed2 <- lmer(mod_gpp ~ airT + maxT+ sm1 + sm2 + sm3 + swr + (1|year) + (1|doy), data = non_drought)
model_null2 <- lmer(mod_gpp ~ (1|year) + (1|doy), data = drivers)
summary(model_mixed2)
summary(model_null2)

# Perform Likelihood Ratio Test
lrt <- anova(model_mixed, model_null)
print(lrt)





# Data for drought
data_drought <- data.frame(
  Driver = c("maxT", "sm1", "sm2", "sm3", "swr", "vpd"),
  Estimate = c(0.4891, 0.1719, -0.2200, 0.0017, 0.5677, -9.4547),
  SE = c(0.1231, 0.1356, 0.0926, 0.0908, 0.0905, 1.4347),
  Dataset = "Drought"
)

# Data for non_drought
data_non_drought <- data.frame(
  Driver = c("maxT", "sm1", "sm2", "sm3", "swr", "vpd"),
  Estimate = c(0.1569, 0.0351, -0.0524, 0.0034, 0.5717, -4.7522),
  SE = c(0.0343, 0.0277, 0.0195, 0.0310, 0.0251, 0.4744),
  Dataset = "Non-Drought"
)

# Combining data
data_combined <- rbind(data_drought, data_non_drought)

# Plot
ggplot(data_combined, aes(x = Driver, y = Estimate, fill = Dataset)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = Estimate - 1.96*SE, ymax = Estimate + 1.96*SE), 
                position = position_dodge(0.7), width = 0.25) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red") +
  labs(title = "Effect of Drivers on GPP", y = "Estimate", x = "Driver", fill = "Dataset") +
  theme_minimal() +
  theme(legend.position = "top")






### RQ3: Drivers vs response variables ####







