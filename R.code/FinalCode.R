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
setwd("/exports/csce/datastore/geos/groups/gcel/for_Tegan/droughts")
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

# Testing for inter-annual variability

# merge both gpp datasets
gpp_all <- rbind(data2000, data2015)
gpp_all$group <- ifelse(gpp_all$year %in% c(2003, 2018), as.character(gpp_all$year), "other")

gpp_all$group <- as.factor(gpp_all$group)
gpp_all_summer <- gpp_all %>%
  filter(doy >= 210 & doy <= 259)

gpp_2003 <- gpp_all_summer %>%
  filter(year >= 2000 & year <= 2005)
gpp_2018 <- gpp_all_summer %>%
  filter(year >= 2015 & year <= 2020)

gpp_2003$group <- as.factor(gpp_2003$group)

kruskal_test_2003 <- kruskal.test(mod_gpp ~ group, data = gpp_2003)
print(kruskal_test_2003)

kruskal_test_2018 <- kruskal.test(mod_gpp ~ group, data = gpp_2018)
print(kruskal_test_2018)

kruskal_test_fullyear <- kruskal.test(mod_gpp ~ group, data = gpp_all)
print(kruskal_test_fullyear)


pairwise_comp2003 <- pairwise.wilcox.test(gpp_2003$mod_gpp, gpp_2003$group, p.adjust.method = "bonferroni")
print(pairwise_comp2003)

pairwise_comp2018 <- pairwise.wilcox.test(gpp_2018$mod_gpp, gpp_2018$group, p.adjust.method = "bonferroni")
print(pairwise_comp2018)

palette2003 <- c("#96DB6B", "#CCCCCCA2")
palette2018 <- c("#F2E857", "#CCCCCCA2")

variability2003 <- ggplot(gpp_2003, aes(x = year, y = mod_gpp, group = year)) +
  geom_boxplot(aes(fill = group)) +
  labs(x = "Year",
       y = "GPP (gC/m²/day)") +
  scale_fill_manual(values = palette2003) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = 'none') +
  scale_x_continuous(breaks = c(2000, 2001, 2002, 2003, 2004, 2005))

plot(variability2003)

variability2018 <- ggplot(gpp_2018, aes(x = year, y = mod_gpp, group = year)) +
  geom_boxplot(aes(fill = group)) +
  labs(x = "Year",
       y = "GPP (gC/m²/day)") +
  scale_fill_manual(values = palette2018) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                 axis.title = element_text(size=11),
                 axis.text = element_text(size=9),
                 legend.title = element_text(size = 11, face = "bold"),
                 legend.text = element_text(size = 11),
                 legend.position = 'none') +
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020))

# combine the inter-annual variability plots
combined_variability_plots <- grid.arrange(
  variability2003, variability2018,
  nrow = 1, 
  layout_matrix = rbind(c(1,2)), 
  heights = c(1))

ggsave("interannual_variability.png", path = "Plots", plot = combined_variability_plots, width = 10, height = 5, dpi = 500)



# Levene's test for homogeneity of variances
levene_test <- car::leveneTest(mod_gpp ~ group, data = gpp_2003)
print(levene_test)


### Calculate mean and standard deviation of annual GPP
mean_gpp <- aggregate(mod_gpp ~ year, data = gpp_all, FUN = mean)
sd_gpp <- aggregate(mod_gpp ~ year, data = gpp_all, FUN = sd)
annual_gpp <- gpp_all %>%
  group_by(year) %>%
  summarise(annual_gpp = mean(mod_gpp, na.rm = TRUE)*365)

annual_gpp <- gpp_all %>%
  group_by(year) %>%
  summarise(
    annual_gpp = mean(mod_gpp, na.rm = TRUE) * 365,
    annual_unc = sqrt(sum(mod_gpp_unc95^2))
  )

# Merge the mean and standard deviation data
summary_stats <- merge(mean_gpp, sd_gpp, by = "year", suffixes = c("_mean", "_sd"))

annual <- merge(summary_stats, annual_gpp, by = "year")

annual1 <- filter(annual, year >= 2000 & year <=2005)
five_year_mean1 <- mean(annual1$annual_gpp, na.rm = TRUE)
annual1$mean <- five_year_mean1
annual1$percent_variation <- (annual1$annual_gpp / annual1$mean) * 100 - 100

annual2 <- filter(annual, year >= 2015 & year <=2020)
five_year_mean2 <- mean(annual2$annual_gpp, na.rm = TRUE)
annual2$mean <- five_year_mean2
annual2$percent_variation <- (annual2$annual_gpp / annual2$mean) * 100 - 100


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
print(lairmsesim) # missing modelled lai and nee in dataset!


# RMSE // R squared calculations and plotting correlation! -> appendix?

gppcorrelation2000 <- cor(data2000$mod_gpp, data2000$obs_gpp, use = "complete.obs")
obs_gpp2000 <- data2000$obs_gpp[!is.na(data2000$obs_gpp)]

# Square the correlation coefficient to get R^2
gppr_squared2000 <- gppcorrelation2000^2

gpprmse2000 <- sqrt(mean((data2000$obs_gpp - data2000$mod_gpp)^2, na.rm = TRUE))
lairmse2000 <- sqrt(mean((data2000$obs_lai - data2000$mod_lai)^2, na.rm = TRUE))
neermse2000 <- sqrt(mean((data2000$obs_nee - data2000$mod_nee)^2, na.rm = TRUE))

# Print the values
print(paste("R^2 value:", round(gppr_squared2000, 3)))
print(gpprmse2000)
print(neermse2000)
print(lairmse2000)

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

neecorrelation2000 <- ggplot(data2000, aes(x = mod_nee, y = obs_nee)) +
  geom_point(colour = "#eb1d129b") +
  labs(x = "Modelled NEE₂₀₀₃ (gC/m²/day)", y = "Observed NEE₂₀₀₃ (gC/m²/day)") +
  geom_abline(intercept = 0, slope = 1, color = "#ba0d04e5", size = 0.6) +
  # geom_abline(intercept = 0, slope = max(data2000$obs_reco, na.rm = TRUE) / max(data2000$mod_reco, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 0, 
                y = 6.5), 
            label = paste("RMSE =", round(neermse2000, 2)), 
            hjust = 0, vjust = 1,
            size = 4, 
            fontface = "bold", 
            colour = "#ba0d04e5") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9)) +
  scale_x_continuous(limits = c(-10,6)) +
  scale_y_continuous(limits = c(-11,6))

plot(neecorrelation2000)


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


#### RQ1 visualisation ####

palette_gpp <- c("#4CBF00", "#EBC800", "#CCCCCCA2")
palette_gpp2 <- c("#3EA85A", "#D6A400", "#CCCCCCA2")
palette2003 <- c("#96DB6B", "#CCCCCCA2")
palette2018 <- c("#F2E857", "#CCCCCCA2")

gpp_variation_plot <- ggplot(gpp_all, aes(x = doy, y = mod_gpp, colour = group, group = year)) +
  geom_line(linewidth = 0.9) +
  labs(title = "",
       x = "Time (month)",
       y = "Weekly GPP (gC/m²/day)",
       colour = "Year:") +
  scale_colour_manual(values = palette_gpp) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(28, 56, 91, 119, 154, 182, 217, 245, 273, 308, 336), 
                     labels = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                     expand = c(0, 0),
                     limits = c(7, 366)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,15))

plot(gpp_variation_plot)

# Save the plot as a PNG file to GitHub
ggsave("gpp_variation_plot.png", path = "Plots", plot = gpp_variation_plot, width = 7, height = 5, dpi = 500)



### RQ2: Drivers vs response variables ####
drivers <- read.csv("rq2data.csv", header = TRUE)
boxplot(drivers$mod_gpp, main="Boxplot of mod_gpp") # visualise data

drivers <- drivers %>%
  mutate(condition = ifelse(year %in% c(2003, 2018), "drought", "normal"))

# drought years only 
drought <- drivers %>%
  filter(year == 2003 | year == 2018)

non_drought <- drivers %>%
  filter(!(year %in% c(2003, 2018)))


## Scale values to use glmer 

# Rescale the continuous variables
drivers$maxT.2 <- scale(drivers$maxT)
drivers$sm1.2 <- scale(drivers$sm1)
drivers$sm2.2 <- scale(drivers$sm2)
drivers$sm3.2 <- scale(drivers$sm3)
drivers$vpd.2 <- scale(drivers$vpd)
drivers$swr.2 <- scale(drivers$swr)

# Fit the model again
model_lm_drought <- lm(mod_gpp ~ airT + maxT + minT + sm1 + sm2 + sm3 + vpd + swr, data = drought)
summary(model_lm_drought)

model_lm_non <- lm(mod_gpp ~ maxT + sm1 + sm2 + sm3 + vpd + swr, data = non_drought)
summary(model_lm_non)


model_mixed <- lmer(mod_gpp ~ maxT.2 + sm1.2 + sm2.2 + sm3.2 + vpd.2 + swr.2 + (1|year), data = drivers)
model_drought <- lmer(mod_gpp ~ maxT.2 + sm1.2 + sm2.2 + sm3.2 + vpd.2 + swr.2 + (1|doy),  data = drought)
model_non <-  lmer(mod_gpp ~ maxT.2 + sm1.2 + sm2.2 + sm3.2 + vpd.2 + swr.2 + (1|doy), data = non_drought)

summary(model_mixed)
summary(model_drought)
summary(model_non)

shapiro_test <- shapiro.test(resid(model_lm_drought))
print(shapiro_test)
qqnorm(resid(model_lm_drought))
qqline(resid(model_lm_drought))
plot(model_drought)

shapiro_test <- shapiro.test(resid(model_non))
print(shapiro_test)
qqnorm(resid(model_non))
qqline(resid(model_non))
plot(model_non)


model_mixed_drought <- lmer(mod_gpp ~ maxT + sm1 + sm2 + sm3 + vpd + swr + (1|doy), data = drought)
summary(model_mixed_drought)
model_mixed_non <- lmer(mod_gpp ~  airT + maxT + minT + sm1 + sm2 + sm3 + vpd + swr + (1|doy), data = non_drought)
summary(model_mixed_non)

AIC_lm <- AIC(model_lm_drought)
AIC_mixed <- AIC(model_mixed_drought)

AIC_lm <- AIC(model_lm_non)
AIC_mixed <- AIC(model_mixed_non)

# Print AIC values
print(AIC_lm) # lm better for drought years
print(AIC_mixed) # lmer better fit for non-drought years

residuals_mixed <- resid(model_mixed_non)
shapiro_test <- shapiro.test(residuals_mixed)
print(shapiro_test) # residuals are normally distributed
qqnorm(residuals_mixed) 
qqline(residuals_mixed)

model_mixed_drought <- lmer(mod_gpp ~ minT + maxT + sm1 + sm2 + sm3 + vpd + (1|doy), data = drought)
summary(model_mixed_drought)

residuals_mixed_drought <- resid(model_mixed_drought)
shapiro_test <- shapiro.test(residuals_mixed_drought)
print(shapiro_test) # residuals are normally distributed
qqnorm(residuals_mixed_all) 
qqline(residuals_mixed_all) # residuals fit qq line

# same but for non-drought years
model_mixed_non <- lmer(mod_gpp ~ airT + maxT + sm1 + sm2 + sm3 + vpd + swr +(1|doy), data = non_drought)
summary(model_mixed_non)

residuals_mixed_non <- resid(model_mixed_non)
shapiro_test <- shapiro.test(residuals_mixed_non)
print(shapiro_test) # residuals are normally distributed
qqnorm(residuals_mixed_non) 
qqline(residuals_mixed_non) # residuals fit qq line

anova(model_mixed_non)
anova(model_mixed_drought)

# null models
model_nulldrought <- lmer(mod_gpp ~ (1|doy), data = drought)
model_nullnon <- lmer(mod_gpp ~ (1|doy), data = non_drought)

residuals_null <- resid(model_nullnon)
qqnorm(residuals_null)
qqline(residuals_null)

summary(model_nulldrought)
summary(model_nullnon)
summary(model_mixed_drought)
summary(model_mixed_non)



## Using 2000-2005 vs 2015-2020
library(lme4)

# Rescale the continuous variables
drivers$maxT.2 <- scale(drivers$maxT)
drivers$airT.2 <- scale(drivers$airT)
drivers$sm1.2 <- scale(drivers$sm1)
drivers$sm2.2 <- scale(drivers$sm2)
drivers$sm3.2 <- scale(drivers$sm3)
drivers$vpd.2 <- scale(drivers$vpd)
drivers$swr.2 <- scale(drivers$swr)

drivers2000 <- non_drought %>%
  filter(year >= 2000 & year <= 2005) 

drivers2015 <- non_drought %>%
  filter(year >= 2015 & year <= 2020)

model_null <- lm(mod_gpp ~ 1, data = drought)

model_sm2 <- lm(mod_gpp ~ sm2, data = drought)
summary(model_sm2)

model_lm_drought <- lm(mod_gpp ~  maxT + airT + sm1 + sm2 + sm3 + vpd + swr, data = drought)
summary(model_lm_drought)

model_lm_drought2 <- lm(mod_gpp ~  airT + sm1 + sm2 + sm3 + vpd + swr, data = drought)
summary(model_lm_drought2)

model_lm_drought4 <- lm(mod_gpp ~  maxT + sm1 + sm2 + sm3 + vpd + swr, data = drought)
summary(model_lm_drought4)

model_lm_drought3 <- lm(mod_gpp ~ maxT * airT * sm1 * sm2 * sm3 + swr + vpd, data = drought)
summary(model_lm_drought3)
plot(model_lm_drought3)
shapiro.test(resid(model_lm_drought3))

plot(mod_gpp ~ maxT, data = drought)
abline(model_lm_drought3)

AIC(model_null, model_lm_drought, model_lm_drought2, model_lm_drought3, model_lm_drought4)
autoplot((model_lm_drought3))

AIC_lmd <- AIC(model_lm_drought)
AIC_lmd2 <- AIC(model_lm_drought2)
AIC_lmd3 <- AIC(model_lm_drought3)
AIC_nulld <- AIC(model_nulldrought)

print(AIC_lmd) 
print(AIC_lmd2) 
print(AIC_lmd3)
print(AIC_nulld) 



model_lm2000 <- lm(mod_gpp ~ maxT + sm1 + sm2 + sm3 + vpd + swr, data = drivers2000)
summary(model_lm2000)

model_lm2015 <- lm(mod_gpp ~ maxT + sm1 + sm2 + sm3 + vpd + swr, data = drivers2015)
summary(model_lm2015)

model_mixed2000 <- lmer(mod_gpp ~ maxT.2 * airT.2 * sm1.2 * sm2.2 * sm3.2 + swr.2 + vpd.2 + (1|doy), data = drivers2000)
model_mixed2015 <- lmer(mod_gpp ~ maxT.2 * airT.2 + sm1.2 * sm2.2 * sm3.2 + swr.2 + vpd.2 + (1|doy),  data = drivers2015)

model_mixed2000_drought <-  lmer(mod_gpp ~ maxT + sm1 + sm2 + sm3 + vpd + swr + (1|year), data = drivers2000)

summary(model_mixed2000)
summary(model_mixed2015)

shapiro_test <- shapiro.test(resid(model_lm_drought3))
print(shapiro_test)
qqnorm(resid(model_lm_drought3))
qqline(resid(model_lm_drought3))

shapiro_test <- shapiro.test(resid(model_mixed2000))
print(shapiro_test)
qqnorm(resid(model_mixed2000))
qqline(resid(model_mixed2000))

shapiro_test <- shapiro.test(resid(model_mixed2015))
print(shapiro_test)
qqnorm(resid(model_mixed2015))
qqline(resid(model_mixed2015))

# null models
model_nulldrought <- lm(mod_gpp ~ 1, data = drought)
summary(model_nulldrought)

model_null2000 <- lmer(mod_gpp ~ (1|doy), data = drivers2000)
model_null2015 <- lmer(mod_gpp ~ (1|doy), data = drivers2015)

summary(model_null2000)
summary(model_null2015)

AIC_lmd <- AIC(model_lm_drought)
AIC_nulld <- AIC(model_nulldrought)
AIC_mixed2000 <- AIC(model_mixed2000)
AIC_null2000 <- AIC(model_null2000)
AIC_mixed2015 <- AIC(model_mixed2015)
AIC_null2015 <- AIC(model_null2015)

# Print AIC values
print(AIC_lmd) 
print(AIC_nulld) 
print(AIC_mixed2000)
print(AIC_null2000)
print(AIC_mixed2015)
print(AIC_null2015)
# -> all AIC null values are significantly higher than the final models


### END HERE !


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



#### method by only looking at differences in gpp rather than whole values

# Calculate the average mod_gpp for each doy (within 5-year periods)

drivers3 <- drivers %>%
  filter(!doy == 126)

drivers4 <- drivers3 %>%
  mutate(month = case_when(
    doy %in% 126:151 ~ "May",
    doy %in% 152:181 ~ "June",
    doy %in% 182:212 ~ "July",
    doy %in% 213:243 ~ "August",
    doy %in% 244:273 ~ "September",
    TRUE ~ NA_character_
  ))

drivers2000 <- drivers4 %>%
  filter(year >= 2000 & year<= 2005)

drivers2015 <- drivers4 %>%
  filter(year >= 2015 & year<= 2020)

avg_gpp2000 <- drivers2000 %>%
  group_by(doy) %>%
  summarise(avg_mod_gpp = mean(mod_gpp, na.rm = TRUE))

avg_gpp2015 <- drivers %>%
  group_by(doy) %>%
  summarise(avg_mod_gpp = mean(mod_gpp, na.rm = TRUE))

drivers2000$mean <- rep(avg_gpp2000$avg_mod_gpp, length.out = nrow(drivers2000))
drivers2015$mean <- rep(avg_gpp2015$avg_mod_gpp, length.out = nrow(drivers2015))

#if using full 12 years for mean #
avg_gpp <- drivers4 %>%
  group_by(doy) %>%
  summarise(avg_mod_gpp = mean(mod_gpp, na.rm = TRUE))
# Merge the average mod_gpp back to the original data frame
drivers4$mean <- rep(avg_gpp$avg_mod_gpp, length.out = nrow(drivers4))
drivers4$diffGPP <- drivers4$mod_gpp - drivers4$mean

# Calculate the relative difference
drivers2000$diffGPP <- drivers2000$mod_gpp - drivers2000$mean
drivers2015$diffGPP <- drivers2015$mod_gpp - drivers2015$mean

# Combine 2000-2005 and 2015-2020
drivers2 <- rbind(drivers2000, drivers2015)

boxplot(drivers2$diffGPP, main="Boxplot of mod_gpp") # visualise dat
shapiro_test <- shapiro.test(drivers2$diffGPP)
print(shapiro_test)

drought <- drivers %>%
  filter(year == 2003 | year == 2018)
non_drought <- drivers %>%
  filter(!(year %in% c(2003, 2018)))


drought2 <- drivers4 %>%
  filter(year == 2003 | year == 2018)
non_drought2 <- drivers4 %>%
  filter(!(year %in% c(2003, 2018)))
non_drought2000 <- drivers2 %>%
  filter((year %in% c(2000, 2001, 2002, 2004, 2005)))
non_drought2015 <- drivers2 %>%
  filter((year %in% c(2015, 2016, 2017, 2019, 2020)))

drivers4$month <- as.factor(drivers4$month)

#all data
model_mixed_diff <- lmer(diffGPP ~ maxT + sm1 + sm2 + sm3 + swr + vpd + (1|month), data = drivers4)
summary(model_mixed_diff)
residuals_mixed_diff <- resid(model_mixed_diff)
shapiro_test <- shapiro.test(residuals_mixed_diff)
print(shapiro_test) # residuals are normally distributed
qqnorm(residuals_mixed_diff) 
qqline(residuals_mixed_diff)

drought2$month <- as.factor(drought2$month)

# drought years only
model_diffdrought <- lmer(mod_gpp ~  maxT + sm1 + sm2 + sm3 + swr + vpd + (1|month), data = drought2)
summary(model_diffdrought)
residuals_diffdrought <- resid(model_diffdrought)
shapiro_test <- shapiro.test(residuals_diffdrought)
print(shapiro_test) # residuals are normally distributed
qqnorm(residuals_diffdrought) 
qqline(residuals_diffdrought)


# filter outliers from non-drought model residuals to pass tests
model_diffnon1 <- lmer(mod_gpp ~ maxT + sm1 + sm2 + sm3 + swr + vpd + (1|month), data = non_drought2)
residuals <- resid(model_diffnon1)
# Identify outliers
outliers <- boxplot.stats(residuals)$out
# Remove outliers from the dataset
non_drought2_filt <- non_drought2[-which(residuals %in% outliers), ]

model_diffnon <- lmer(mod_gpp ~ maxT + sm1 + sm2 + sm3 + swr + vpd + (1|month), data = non_drought2_filt)
summary(model_diffnon)

residuals_diffnon <- resid(model_diffnon)
shapiro_test <- shapiro.test(residuals_diffnon)
print(shapiro_test) # residuals are almost normally distributed
qqnorm(residuals_diffnon) 
qqline(residuals_diffnon) # passes qq test





# NEW PLOTS

# Predicted values using lm and lmer models
drivers$pred_gpp_drought <- predict(model_lm_drought3, newdata = drivers, re.form = NA)
drivers$pred_gpp_2000 <- predict(model_mixed2000, newdata = drivers, re.form = NA)
drivers$pred_gpp_2015 <- predict(model_mixed2015, newdata = drivers, re.form = NA)

drivers$group <- ifelse(drivers$year %in% c(2000, 2001, 2002, 2004, 2005), "2000-2005",
                        ifelse(drivers$year %in% c(2015, 2016, 2017, 2019, 2020), "2015-2020",
                               ifelse(drivers$year %in% c(2003, 2018), "2003 & 2018", NA)))

palette_drivers <- c("#96DB6B", "#FF8400E0", "#F2E857", "#FF8400E0", "#96DB6B", "#F2E857")

maxTplot <- ggplot(drivers, aes(x = maxT, y = mod_gpp, colour = group)) + 
  geom_point(aes(colour = group, shape = condition)) +
  geom_smooth(aes(y = pred_gpp_drought, colour ="LM drought years"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = pred_gpp_2000, colour ="LMER 2000-2005"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "Max T(°C)", y = "GPP (gC/m²/day)")

plot(maxTplot)

airTplot <- ggplot(drivers, aes(x = airT, y = mod_gpp, colour = group)) + 
  geom_point(aes(colour = group, shape = condition)) +
  geom_smooth(aes(y = pred_gpp_drought, colour ="LM drought years"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = pred_gpp_2000, colour ="LMER 2000-2005"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "Air T(°C)", y = "GPP (gC/m²/day)")

swrplot <- ggplot(drivers, aes(x = swr, y = mod_gpp, colour = group)) + 
  geom_point(aes(colour = group, shape = condition)) +
  geom_smooth(aes(y = pred_gpp_drought, colour ="LM drought years"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = pred_gpp_2000, colour ="LMER 2000-2005"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SWR (MJ/m²/day)", y = "GPP (gC/m²/day)")

sm1plot <- ggplot(drivers, aes(x = sm1, y = mod_gpp, colour = group)) + 
  geom_point(aes(colour = group, shape = condition)) +
  geom_smooth(aes(y = pred_gpp_drought, colour ="LM drought years"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = pred_gpp_2000, colour ="LMER 2000-2005"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SM1 (?)", y = "GPP (gC/m²/day)")

sm2plot <- ggplot(drivers, aes(x = sm2, y = mod_gpp, colour = group)) + 
  geom_point(aes(colour = group, shape = condition)) +
  geom_smooth(aes(y = pred_gpp_drought, colour ="LM drought years"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = pred_gpp_2000, colour ="LMER 2000-2005"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SM2 (?)", y = "GPP (gC/m²/day)")

sm3plot <- ggplot(drivers, aes(x = sm3, y = mod_gpp, colour = group)) + 
  geom_point(aes(colour = group, shape = condition)) +
  geom_smooth(aes(y = pred_gpp_drought, colour ="LM drought years"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = pred_gpp_2000, colour ="LMER 2000-2005"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SM3 (?)", y = "GPP (gC/m²/day)")

vpdplot <- ggplot(drivers, aes(x = vpd, y = mod_gpp, colour = group)) + 
  geom_point(aes(colour = group, shape = condition)) +
  geom_smooth(aes(y = pred_gpp_drought, colour ="LM drought years"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = pred_gpp_2000, colour ="LMER 2000-2005"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "VPD (kPa)", y = "GPP (gC/m²/day)")


# combine the correlation plots
combined_rq2_plots <- grid.arrange(
  maxTplot, vpdplot, swrplot, 
  sm1plot, sm2plot, sm3plot, 
  nrow = 2, 
  layout_matrix = rbind(c(1,2,3), c(4,5,6)), 
  heights = c(1,1))

# Save the plot as a PNG file to GitHub
setwd("/exports/csce/datastore/geos/groups/gcel/for_Tegan/droughts")
ggsave("rq2_plots.png", path = "Plots", plot = combined_rq2_plots, width = 10, height = 7, dpi = 500)





# Plotting (BEFORE)

drivers$group <- ifelse(drivers$year %in% c(2000, 2001, 2002, 2004, 2005), "2000-2005",
                        ifelse(drivers$year %in% c(2015, 2016, 2017, 2019, 2020), "2015-2020",
                               ifelse(drivers$year %in% c(2003, 2018), "2003 & 2018", NA)))

palette_drivers <- c("#96DB6B", "#FF8400E0", "#F2E857")
palette_gpp <- c("#4CBF00", "#EBC800", "#CCCCCCA2")
palette_gpp2 <- c("#3EA85A", "#D6A400", "#CCCCCCA2")
palette2003 <- c("#96DB6B", "#CCCCCCA2")
palette2018 <- c("#F2E857", "#CCCCCCA2")

maxTplot <- ggplot(drivers, aes(x = maxT, y = mod_gpp, group = group, colour = group, shape = condition)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(colour = group)) +
  scale_color_manual(values =  palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "Max temperature (°C)", y = "GPP (gC/m²/day)")
plot(maxTplot)


sm2plot <- ggplot(drivers, aes(x = sm2, y = mod_gpp, group = group, colour = group, shape = condition)) + 
  geom_point() +
  geom_smooth(method = "glm", se = FALSE, aes(colour = group)) +
  scale_color_manual(values =  palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SM2 (?)", y = "GPP (gC/m²/day)") 

swrplot <- ggplot(drivers, aes(x = swr, y = mod_gpp, group = group, colour = group, shape = condition)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(colour = group)) +
  scale_color_manual(values =  palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SWR (MJ/m²/day)", y = "GPP (gC/m²/day)") 

vpdplot <- ggplot(drivers, aes(x = vpd, y = mod_gpp, group = group, colour = group, shape = condition)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(colour = group)) +
  scale_color_manual(values =  palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "VPD (kPa)", y = "GPP (gC/m²/day)")

plot(vpdplot)

sm1plot <- ggplot(drivers, aes(x = sm1, y = mod_gpp, group = group, colour = group, shape = condition)) + 
  geom_point() +
  geom_smooth(method = "glm", se = FALSE, aes(colour = group)) +
  scale_color_manual(values =  palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SM1 (?)", y = "GPP (gC/m²/day)")  

sm3plot <- ggplot(drivers, aes(x = sm3, y = mod_gpp, group = group, colour = group, shape = condition)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(colour = group)) +
  scale_color_manual(values =  palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +  # Change point shapes
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SM3 (?)", y = "GPP (gC/m²/day)")  

sm2_lm <- lm(mod_gpp ~ sm2, data = drought)
plot(sm2_lm)
shapiro.test(resid(sm2_lm)) #non-normal



# combine the correlation plots
combined_rq2_plots <- grid.arrange(
  maxTplot, vpdplot, swrplot, 
  sm1plot, sm2plot, sm3plot, 
  nrow = 2, 
  layout_matrix = rbind(c(1,2,3), c(4,5,6)), 
  heights = c(1,1))


# Save the plot as a PNG file to GitHub
setwd("/exports/csce/datastore/geos/groups/gcel/for_Tegan/droughts")
ggsave("rq2_plots.png", path = "Plots", plot = combined_rq2_plots, width = 10, height = 7, dpi = 500)



#### NMDS











### RQ3: Drivers vs response variables ####







