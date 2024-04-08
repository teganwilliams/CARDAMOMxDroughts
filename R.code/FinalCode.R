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
sim <- read.csv("Data/simulationdata.csv", header = TRUE)
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
gpp_all$group <- ifelse(gpp_all$year %in% c(2003, 2018), as.character(gpp_all$year), "Non-drought")


gpp_all$group <- as.factor(gpp_all$group)

gpp_drought2018 <- gpp_all %>%
  filter(doy >= 154 & doy <= 245) # 19th may (140), 2nd june (154) - 8th sep (252)(16 weeks; 3.5 months)

gpp_drought2018 <- gpp_all %>%
  filter(doy >= 210 & doy <= 252) # 28th july - 8th september (7 weeks)

gpp_drought2018 <- gpp_all %>%
  filter(doy >= 210 & doy <= 245) # 28th july - 1st september (6 weeks)

gpp_drought2018 <- gpp_all %>%
  filter(doy >= 189 & doy <= 245) # 1st july - 1st september (9 weeks)

gpp_drought1 <- gpp_all %>%
  filter(doy >= 217 & doy <= 252) # based on Wei et al. 2024

gpp_drought2 <- gpp_all %>%
  filter(doy >= 182 & doy <= 273) # based on Wei et al. 2024



gpp_2003 <- gpp_drought1 %>%
  filter(year >= 2000 & year <= 2005)
gpp_2018 <- gpp_drought2 %>%
  filter(year >= 2015 & year <= 2020)

gpp_2003$group <- as.factor(gpp_2003$group)

kruskal_test_2003 <- kruskal.test(mod_gpp ~ group, data = gpp_2003)
print(kruskal_test_2003)

kruskal_test_2018 <- kruskal.test(mod_gpp ~ year, data = gpp_2018)
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
  geom_text(aes(x = 2003, y = 3.5,
                label = paste("p-value = ", signif(kruskal_test_2003$p.value, digits = 1))), 
            vjust = -1, color = "black") +  
  scale_fill_manual(values = palette2003) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = 'none') +
  scale_x_continuous(breaks = c(2000, 2001, 2002, 2003, 2004, 2005)) +
  scale_y_continuous(limits = c(1.5, 15),
                     breaks = c(5, 10, 15))

plot(variability2003)

variability2018 <- ggplot(gpp_2018, aes(x = year, y = mod_gpp, group = year)) +
  geom_boxplot(aes(fill = group)) +
  labs(x = "Year",
       y = "GPP (gC/m²/day)") +
  geom_text(aes(x = 2018, y = 1.5,
                label = paste("p-value =", signif(kruskal_test_2018$p.value, digits = 1))), 
            vjust = -1, color = "black") + 
  scale_fill_manual(values = palette2018) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                 axis.title = element_text(size=11),
                 axis.text = element_text(size=9),
                 legend.title = element_text(size = 11, face = "bold"),
                 legend.text = element_text(size = 11),
                 legend.position = 'none') +
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020)) +
  scale_y_continuous(limits = c(1.5, 15),
                     breaks = c(5, 10, 15))

plot(variability2018)

# combine the inter-annual variability plots
combined_variability_plots <- grid.arrange(
  variability2003, variability2018,
  nrow = 1, 
  layout_matrix = rbind(c(1,2)), 
  heights = c(1))

ggsave("interannual_variability.png", path = "Plots", plot = combined_variability_plots, width = 10, height = 5, dpi = 500)

max(gpp_2003$mod_gpp)

# inter-annual variability for NEE
NEEvariability2003 <- ggplot(gpp_2003, aes(x = year, y = mod_nee, group = year)) +
  geom_boxplot(aes(fill = group)) +
  scale_fill_manual(values = palette2003) +
  labs(x = "Year",
       y = "NEE (gC/m²/day)",
       colour = "Year:") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = 'none') +
  scale_x_continuous(breaks = c(2000, 2001, 2002, 2003, 2004, 2005)) +
  scale_y_continuous(limits = c(-8, 0.5))

plot(NEEvariability2003)

NEEvariability2018 <- ggplot(gpp_2018, aes(x = year, y = mod_nee, group = year)) +
  geom_boxplot(aes(fill = group)) +
  labs(x = "Year",
       y = "NEE (gC/m²/day)",
       colour = "Year:") +
  scale_fill_manual(values = palette2018) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = 'none') +
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020)) +
  scale_y_continuous(limits = c(-8, 0.5))

plot(NEEvariability2018)

# combine the inter-annual variability plots
NEEcombined_variability_plots <- grid.arrange(
  NEEvariability2003, NEEvariability2018,
  nrow = 1, 
  layout_matrix = rbind(c(1,2)), 
  heights = c(1))

ggsave("NEE_interannual_variability.png", path = "Plots", plot = NEEcombined_variability_plots, width = 7, height = 4, dpi = 500)




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

# same for NEE
mean_nee <- aggregate(mod_nee ~ year, data = gpp_all, FUN = mean)
sd_nee <- aggregate(mod_nee ~ year, data = gpp_all, FUN = sd)
annual_nee <- gpp_all %>%
  group_by(year) %>%
  summarise(annual_nee = mean(mod_nee, na.rm = TRUE)*365)

annual_nee <- gpp_all %>%
  group_by(year) %>%
  summarise(
    annual_nee = mean(mod_nee, na.rm = TRUE) * 365,
    annual_nee_unc = sqrt(sum(mod_nee_unc95^2))
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


# a) plotting timeseries of modelled and obs GPP over time (5 years) ####

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
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.4) +
  geom_ribbon(aes(ymin = obs_nee - obs_nee_unc, ymax = obs_nee + obs_nee_unc, colour = "Obs unc"), fill = "#FF730085",  colour = "#FF73001F", alpha = 0.2) +
  geom_ribbon(aes(ymin = mod_nee - 0.2*abs(mod_nee_unc95), ymax = mod_nee + 0.2*abs(mod_nee_unc95), colour = "Mod unc"), fill = "#1000bfb5", colour = "#1704c436", alpha = 0.2) +
  geom_line(aes(y = mod_nee, colour = "Mod"), size = 0.6) +
  geom_point(aes(y = obs_nee, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "NEE (gC/m²/day)", colour = "Data:") +
  scale_color_manual(values = c("Mod" = "#1000bfee", "Mod unc" ="#1704c436", "Obs" = "#FF730085", "Obs unc" ="#FF73001F")) +
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
                     limits = c(-13,5))

needrought2018 <- ggplot(data2015, aes(x = day)) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.4) +
  geom_ribbon(aes(ymin = obs_nee - obs_nee_unc, ymax = obs_nee + obs_nee_unc, colour = "Obs unc"), fill = "#FF730085",  colour = "#FF73001F", alpha = 0.2) +
  geom_ribbon(aes(ymin = mod_nee - 0.2*abs(mod_nee_unc95), ymax = mod_nee + 0.2*abs(mod_nee_unc95), colour = "Mod unc"), fill = "#1000bfb5", colour = "#1704c436", alpha = 0.2) +
  geom_line(aes(y = mod_nee, colour = "Mod"), size = 0.6) +
  geom_point(aes(y = obs_nee, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "NEE (gC/m²/day)", colour = "Data:") +
  scale_color_manual(values = c("Mod" = "#1000bfee", "Mod unc" ="#1704c436", "Obs" = "#FF730085", "Obs unc" ="#FF73001F")) +
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
                     limits = c(-13,5))

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
       y = "GPP (gC/m²/day)",
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

combined_rq1_timeseries <- grid.arrange(
  gpp_variation_plot, nee_variation_plot,
  nrow = 1, 
  layout_matrix = rbind(c(1,2)), 
  heights = c(1))


# Save the plot as a PNG file to GitHub
ggsave("Combined_rq1_timeseries.png", path = "Plots", plot = combined_rq1_timeseries, width = 7, height = 5, dpi = 500)


# Same for NEE
nee_variation_plot <- ggplot(gpp_all, aes(x = doy, y = mod_nee, colour = group, group = year)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, size = 0.3, colour = "black") +
  labs(title = "",
       x = "Time (month)",
       y = "NEE (gC/m²/day)",
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
                     limits = c(-10,5))

plot(nee_variation_plot)

# combine the correlation plots
combined_rq1_nee <- grid.arrange(
  gpp_variation_plot, nee_variation_plot, 
  ncol = 2, 
  layout_matrix = rbind(c(1,2)), 
  heights = c(1))

# Save the plot as a PNG file to GitHub
ggsave("rq1_NEE&GPP.png", path = "Plots", plot = combined_rq1_nee, width = 10, height = 7, dpi = 500)

# now.. do i switch to investigating NEE instead???



### RQ2: Drivers vs response variables ####
drivers <- read.csv("rq2data.csv", header = TRUE)
boxplot(drivers$mod_gpp, main="Boxplot of mod_gpp") # visualise data

drivers <- drivers %>%
  mutate(condition = ifelse(year %in% c(2003, 2018), "drought", "normal"))

drivers$year_group <- as.factor(drivers$year_group)

drivers$year_group <- ifelse(drivers$year %in% c(2000, 2001, 2002, 2004, 2005, 2015, 2016, 2017, 2019, 2020), "normal",
                        # ifelse(drivers$year %in% c(2015, 2016, 2017, 2019, 2020), "2015-2020",
                               ifelse(drivers$year %in% c(2003), "2003", 
                                      ifelse(drivers$year %in% c(2018), "2018", NA)))

drought <- drought %>%
  group_by(year) %>%
  mutate(mean_maxT = maxT, mean_gpp = mod_gpp, mean_sm2 = sm2, mean_vpd = vpd, mean_swr = swr) %>%
  ungroup()

non_drought <- non_drought %>%
  group_by(doy) %>%
  mutate(mean_gpp = mean(mod_gpp, na.rm = TRUE),
         mean_maxT = mean(maxT, na.rm = TRUE),
         mean_sm2 = mean(sm2, na.rm = TRUE),
         mean_vpd = mean(vpd, na.rm = TRUE),
         mean_swr = mean(swr, na.rm = TRUE)) %>%
  ungroup()

new <- rbind(drought, non_drought)

palette_anomalies <- c("#D6D6D686", "#D6A400", "#B80422", "#7362BA")
palette_anomalies <- c("#29B071", "#D6A400", "darkgrey")

maxT_plot <- ggplot(new, aes(x = doy, y = mean_maxT, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Time (months)",
       y = "Max temperature (°C)",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  #scale_shape_manual() + 
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(126, 154, 182, 217, 252), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"))

plot(maxT_plot)


gpp_plot <- ggplot(new, aes(x = doy, y = mean_gpp, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Summer months",
       y = "GPP (gC/m²/day)",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  #scale_shape_manual() + 
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(126, 154, 182, 217, 252), 
        labels = c("May", "Jun", "Jul", "Aug", "Sep"))

plot(gpp_plot)




sm2_plot <- ggplot(new, aes(x = doy, y = mean_sm2, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Time (month)",
       y = "Soil moisture at depth 2 (X)",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  #scale_shape_manual() + 
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(126, 154, 182, 217, 252), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"))

plot(sm2_plot)


vpd_plot <- ggplot(new, aes(x = doy, y = mean_vpd, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Time (month)",
       y = "VPD (kPa)",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  #scale_shape_manual() + 
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(126, 154, 182, 217, 252), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"))
plot(vpd_plot)

swr_plot <- ggplot(new, aes(x = doy, y = mean_swr, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Time (months)",
       y = "SWR (MJ/m²/day)",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  #scale_shape_manual() + 
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(126, 154, 182, 217, 252), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"))
plot(swr_plot)


combined_rq2_plots <- grid.arrange(
  gpp_plot, maxT_plot, sm2_plot, 
  vpd_plot, swr_plot, 
  nrow = 2,
  layout_matrix = rbind(c(1,1,2), c(5,4,3)), 
  widths = c(1,1,1.5),
  heights = c(1, 1)
)

# Save the plot as a PNG file to GitHub
setwd("/exports/csce/datastore/geos/groups/gcel/for_Tegan/Diss_GitHub")
ggsave("rq2_plots.png", path = "Plots", plot = combined_rq2_plots, width = 10, height = 7, dpi = 500)





### Partial correlation test

install.packages("ppcor")
library(ppcor)

# Partial correlation between sm1 and gpp controlling for maxT, vpd, and swr

pcor_test_sm2 <- pcor.test(driver$mod_gpp, drivers2000$sm2, 
                            x = drivers2000[, c("maxT", "vpd", "swr")])

pcor_test_maxT <- pcor.test(drivers2000$mod_gpp, drivers2000$maxT, 
                            x = drivers2000[, c("sm2", "vpd", "swr")])

pcor_test_vpd <- pcor.test(drivers2000$mod_gpp, drivers2000$vpd, 
                           x = drivers2000[, c("sm2", "maxT", "swr")])

pcor_test_swr <- pcor.test(drivers2000$mod_gpp, drivers2000$swr, 
                           x = drivers2000[, c("sm2", "maxT", "vpd")])

cor_results <- cor(drivers2015[, c("mod_gpp", "maxT", "sm1", "sm2", "sm3", "vpd", "swr")])

# drought

pcor_test_sm2 <- pcor.test(drought$mod_gpp, drought$sm2, 
                           x = drought[, c("maxT", "vpd", "swr")])

pcor_test_maxT <- pcor.test(drought$mod_gpp, drought$maxT, 
                            x = drought[, c("sm2", "vpd", "swr")])

# Partial correlation between mod_gpp and vpd, controlling for sm1, sm2, sm3, maxT, swr
pcor_test_vpd <- pcor.test(drought$mod_gpp, drought$vpd, 
                           x = drought[, c("sm2", "maxT", "swr")])

# Partial correlation between mod_gpp and swr, controlling for sm1, sm2, sm3, maxT, vpd
pcor_test_swr <- pcor.test(drought$mod_gpp, drought$swr, 
                           x = drought[, c("sm2", "maxT", "vpd")])


# Results
pcor_resultsdrought <- data.frame(
  Variable = c("sm2", "maxT", "vpd", "swr"),
  Partial_Correlation = c(pcor_test_sm2$estimate, pcor_test_maxT$estimate, pcor_test_vpd$estimate, pcor_test_swr$estimate),
  P_Value = c(pcor_test_sm2$p.value, pcor_test_maxT$p.value, pcor_test_vpd$p.value, pcor_test_swr$p.value)
)

print(pcor_results2015)
print(pcor_results2000)
print(pcor_resultsdrought)





## Scale values to use glmer 

# Rescale the continuous variables
drivers$maxT.2 <- scale(drivers$maxT)
drivers$sm1.2 <- scale(drivers$sm1)
drivers$sm2.2 <- scale(drivers$sm2)
drivers$sm3.2 <- scale(drivers$sm3)
drivers$vpd.2 <- scale(drivers$vpd)
drivers$swr.2 <- scale(drivers$swr)

drivers2003 <- drivers %>%
  filter(year >= 2000 & year <= 2005)

drivers2018 <- drivers %>%
  filter(year >= 2015 & year <= 2020)


# Fit the model again

model_2015 <- glmer(mod_gpp ~  maxT.2 + sm2.2 + vpd.2 + swr.2 + (1|doy), data = drivers2015, family = Gamma(link = "log"))
summary(model_2015)

model_2000 <- glmer(mod_gpp ~  maxT.2 + sm2.2 + vpd.2 + swr.2 + (1|doy), data = drivers2000, family = Gamma(link = "log"))
summary(model_2000)

model_drought <- glmer(mod_gpp ~  maxT.2 + sm2.2 + vpd.2 + swr.2 + (1|doy), data = drought, family = Gamma(link = "log"))
summary(model_drought)



glm_drought <- glm(mod_gpp ~  maxT + sm2 + vpd + swr, data = drought)
summary(model_drought)


AIC(glm_drought, model_drought)


residuals_glm <- resid(model_2015)
shapiro.test(residuals_glm)
qqnorm(residuals_glm) 
qqline(residuals_glm)

residuals_glm <- resid(model_2000)
shapiro.test(residuals_glm)
qqnorm(residuals_glm) 
qqline(residuals_glm)

residuals <- residuals(model_drought)
plot_data <- data.frame(
  Fitted = fitted(model_drought),
  Residuals = residuals
)

ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residuals vs Fitted")

res_deviance <- residuals(model_drought, type = "deviance")

# Degrees of freedom
df <- df.residual(model_drought)

# Calculate overdispersion
overdispersion <- sum(res_deviance^2) / df

overdispersion
library(car)
vif(model_drought)




residuals_glm <- resid(glm_drought)
shapiro.test(residuals_glm)
qqnorm(residuals_glm) 
qqline(residuals_glm)





model_lm_drought <- lm(mod_gpp ~ airT + maxT + minT + sm1 + sm2 + sm3 + vpd + swr, data = drought)
summary(model_lm_drought)

model_lm_non <- lm(mod_gpp ~ maxT + sm1 + sm2 + sm3 + vpd + swr, data = non_drought)
summary(model_lm_non)


model_mixed <- lmer(mod_gpp ~ maxT + sm3 + vpd + swr + (1|group), data = drivers)
model_drought <- lmer(mod_gpp ~ maxT.2 + sm1.2 + sm2.2 + sm3.2 + vpd.2 + swr.2 + (1|doy),  data = drought)
model_non <-  lmer(mod_gpp ~ maxT.2 + sm1.2 + sm2.2 + sm3.2 + vpd.2 + swr.2 + (1|doy), data = non_drought)

summary(model_mixed)
summary(model_drought)
summary(model_non)

shapiro_test <- shapiro.test(resid(model_mixed))
print(shapiro_test)
qqnorm(resid(model_mixed))
qqline(resid(model_lm_drought))
plot(model_mixed)

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

install.packages("lmerTest")
library(lmerTest)
library(lmtest)

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


glm_drought <- glm(mod_gpp ~ maxT + sm3 + vpd + swr, data = drought, family = gaussian)
summary(glm_drought)

glm_2000 <- glm(mod_gpp ~ maxT + sm3 + vpd + swr, data = drivers2000, family = gaussian)
summary(glm_2000)

glm_2015 <- glm(mod_gpp ~ maxT + sm3 + vpd + swr, data = drivers2015, family = gaussian)
summary(glm_2015)

glm_2 <- glm(mod_gpp ~ maxT + sm3 + vpd + swr, data = drivers2000, family = gaussian)
summary(glm_2015)

residuals_glm <- resid(glm_drought)
shapiro.test(residuals_glm)
qqnorm(residuals_glm) 
qqline(residuals_glm) # passes qq test

# Partial residual plots
library(car)
crPlots(glm_drought)

sqrt_abs_standardized_residuals <- sqrt(abs(rstandard(glm_drought)))
plot(glm_drought$fitted.values, sqrt_abs_standardized_residuals, 
     xlab = "Fitted values", ylab = "√|Standardized residuals|",
     main = "Scale-Location", 
     col = "blue", pch = 16)
abline(h = 1, col = "red", lwd = 2)

vif_values <- vif(glm_drought)
print(vif_values)


# Plot residuals
ggplot(drought, aes(x = sm1)) +
  geom_point(aes(y = residuals_glm), colour = "blue", alpha = 0.7) +
  labs(y = "Residuals", colour = "Model") +
  theme_minimal()

model_2000 <- glmer(mod_gpp ~  maxT.2 + sm2.2 + vpd.2 + swr.2 + (1|doy), data = drivers2000, family = Gamma(link = "log"))
summary(model_2000)

model_2015 <- glmer(mod_gpp ~  maxT.2 + sm2.2 + vpd.2 + swr.2 + (1|doy), data = drivers2015, family = Gamma(link = "log"))
summary(model_2015)

model_2015 <- glm(mod_gpp ~  maxT + sm2 + vpd + swr, data = drivers2015)
summary(model_2015)

glm_drought <- glm(mod_gpp ~  maxT + sm2 + vpd + swr, data = drought)
summary(model_drought)

glmer_drought <- glmer(mod_gpp ~  maxT + sm2 + vpd + swr + (1|doy), data = drought, family = Gamma(link = "log"))
summary(model_drought)

residuals_glm <- resid(model_drought)
shapiro.test(residuals_glm)
qqnorm(residuals_glm) 
qqline(residuals_glm) # passes qq test



model_drought <- lm(mod_gpp ~  maxT + sm2 + vpd + swr, data = drought)
summary(model_drought)

model_2000 <- lmer(mod_gpp ~  maxT + sm2 + vpd + swr + (1|doy), data = drivers2000)
summary(model_2000)

model_2015 <- lmer(mod_gpp ~  maxT + sm2 + vpd + swr + (1|doy),data = drivers2015)
summary(model_2015)




residuals_glm <- resid(model_2015)
shapiro.test(residuals_glm)
qqnorm(residuals_glm) 
qqline(residuals_glm) # passes qq test


glm_null <- glm(mod_gpp ~ 1, data = drivers2015)

AIC(glm_null, glm_drought, glm_2000, glm_2015, glm_2)

model_sm2 <- lm(mod_gpp ~ sm2, data = drought)
summary(model_sm2)

model_lmer_drought <- lmer(mod_gpp ~  maxT + sm2 + vpd + swr + (1|doy), data = drought, control = lmerControl(optCtrl = list(maxfun = 10000, optimizer = "nloptwrap", calc.derivs = FALSE)))
summary(model_lmer_drought)

model_lmer_non <- lmer(mod_gpp ~  maxT + sm1 + sm3 + sm2 + vpd + swr + (1|doy), data = non_drought)
summary(model_lmer_non)

install.packages('lmtest')
library(lmtest)


model_lm_drought2 <- lm(mod_gpp ~  airT + sm1 + sm2 + sm3 + vpd + swr, data = drought)
summary(model_lm_drought2)

model_lm_drought4 <- lm(mod_gpp ~  maxT + sm1 + sm2 + sm3 + vpd + swr, data = drought)
summary(model_lm_drought4)

model_lm_drought3 <- lm(mod_gpp ~ maxT + sm1 * sm2 * sm3 + swr + vpd, data = drought)
summary(model_lm_drought3)

dev.off()
shapiro_test <- shapiro.test(resid(model_lmer_non))
print(shapiro_test)
qqnorm(resid(model_lmer_non))
qqline(resid(model_lmer_non))


install.packages("lmerTest")
library(lmerTest)

plot(model_lm_drought)
shapiro.test(resid(model_lm_drought))

plot(mod_gpp ~ maxT, data = drought)
abline(model_lm_drought3)

AIC(model_null, model_lmer_drought, model_lm_drought2, model_lm_drought3, model_lm_drought4)
autoplot((model_lm_drought3))

AIC_lmd <- AIC(model_lm_drought)
AIC_lmd2 <- AIC(model_lm_drought2)
AIC_lmd3 <- AIC(model_lm_drought3)
AIC_nulld <- AIC(model_nulldrought)

print(AIC_lmd) 
print(AIC_lmd2) 
print(AIC_lmd3)
print(AIC_nulld) 


maxT2000 <- lm(mod_gpp ~ maxT, data = drivers2000)
summary(maxT2000)
maxT2015 <- lm(mod_gpp ~ maxT, data = drivers2015)
summary(maxT2015)
maxTdrought <- lm(mod_gpp ~ maxT, data = drought)
summary(maxTdrought)

shapiro_test <- shapiro.test(resid())
print(shapiro_test)
qqnorm(resid(maxT2000))
qqline(resid(maxT2000))

model_lm2000 <- lm(mod_gpp ~ maxT + sm1 + sm2 + sm3 + vpd + swr, data = drivers2000)
summary(model_lm2000)

model_lm2015 <- lm(mod_gpp ~ maxT + sm1 + sm2 + sm3 + vpd + swr, data = drivers2015)
summary(model_lm2015)

library(lme4)

model_quad_drought <- lm(mod_gpp ~ sm3 + I(sm3^2), data = drought)
summary(model_glmer_drought)

model_glmer_drought <- glmer(mod_gpp ~ maxT.2 + sm1.2 + sm3.2 + sm2.2 + vpd.2 + swr.2 + (1|doy), 
                             data = drought,
                             family = Gamma(link = "log"), 
                             control = glmerControl(optCtrl = list(maxfun = 10000, optimizer = "nloptwrap")))
summary(model_glmer_drought)

model_glmer_2000 <- glmer(mod_gpp ~ maxT.2 + sm3.2 + vpd.2 + swr.2 + (1|doy), 
                             data = drivers2000, 
                             family = Gamma(link = "log"), 
                          control = glmerControl(optCtrl = list(maxfun = 10000, optimizer = "nloptwrap")))
summary(model_glmer_2000)

model_glmer_2015 <- glmer(mod_gpp ~ maxT.2 + sm1.2 + sm3.2 + sm2.2 + vpd.2 + swr.2 + (1|doy), 
                          data = drivers2015, 
                          family = Gamma(link = "log"), 
                          control = glmerControl(optCtrl = list(maxfun = 10000, optimizer = "nloptwrap")))
summary(model_glmer_2015)


model_mixed2000 <- lmer(mod_gpp ~ maxT.2 + sm1.2 + I(sm1.2^2) + sm2.2 + I(sm2.2^2) + sm3.2 + I(sm3.2^2) + swr.2 + vpd.2 + (1|doy), data = drivers2000)
model_mixed2000 <- lmer(mod_gpp ~ maxT + sm1 + sm2 + sm3 + swr + vpd + (1|doy),  data = drivers2000)
model_mixed2015 <- lmer(mod_gpp ~ maxT + sm1 + sm2 + sm3 + swr + vpd + (1|doy),  data = drivers2015)
summary(model_mixed2000)
summary(model_mixed2015)

model0 <- lm(mod_gpp ~ maxT + sm1 * sm2 * sm3 + swr + vpd, data = drivers2000)

model1 <- lmer(mod_gpp ~ maxT + sm1 + sm2 + sm3 + swr + vpd + (1|doy), data = drivers2000)
model2 <- lmer(mod_gpp ~ maxT + sm1 + sm2 + sm3 + swr + vpd + (1|doy), data = drivers2000)
model3 <- lmer(mod_gpp ~ maxT + sm1 + sm2 + sm3 + swr + vpd + (1|doy), data = drivers2000)

AIC(model_null, model0, model1, model_lm_drought3, model_lm_drought4)

shapiro_test <- shapiro.test(resid(model_glmer_drought))
print(shapiro_test)
qqnorm(resid(model_glmer_drought))
qqline(resid(model_glmer_drought))


shapiro_test <- shapiro.test(resid(model_lmer_drought))
print(shapiro_test)
qqnorm(resid(model_lmer_drought))
qqline(resid(model_lmer_drought))

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

library(mgcv)
model_gam_drought <- gam(mod_gpp ~ maxT + sm1 + sm2 + sm3 + swr + vpd, data = drought)
summary(model_gam_drought)

model_gam_drought <- gam(mod_gpp ~ maxT + sm1 + sm2 + sm3 + swr + vpd, data = drivers2015)

install.packages('lmtest')
library(lmtest)

model_lm <- lm(mod_gpp ~ bc_sm1, data = drivers2015)
bptest(model_lm)


residuals <- residuals(model_lm)
fitted_values <- fitted(model_lm)

ggplot() +
  geom_point(aes(x = fitted_values, y = residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted values",
       y = "Residuals")

plot(model_lm$fitted.values, model_lm$residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")
lines(lowess(model_lm$fitted.values, model_lm$residuals), col = "blue")

### END HERE !

model_gam <- gam(mod_gpp ~ maxT + sm1 + sm2 + sm3 + vpd + swr + (1|doy), data = drivers2015)


colnames(drivers2015)


install.packages("lmtest")
library(lmtest)

model_lm <- lm(mod_gpp ~ swr, data = drivers2015)
bptest(model_lm)





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






# using ggpredict
model2000 <- lmer(mod_gpp ~ maxT.2 * airT.2 * sm1.2 * sm2.2 * sm3.2 + swr.2 + vpd.2 + (1|doy), data = drivers2000)
model2015 <- lmer(mod_gpp ~ maxT.2 * airT.2 * sm1.2 * sm2.2 * sm3.2 + swr.2 + vpd.2 + (1|doy),  data = drivers2015)
modeldrought <- lm(mod_gpp ~ maxT * airT * sm1 * sm2 * sm3 + swr + vpd, data = drought )
summary(model2000)
summary(model2015)

# Generate partial dependence plots
install.packages("Hmisc")
install.packages("rms")
library(Hmisc)
library(rms)

# Compute the marginal effects
effects2000 <- allEffects(model2000)
effects2015 <- allEffects(model2015)
effectsdrought <- allEffects(modeldrought)

# Plot the marginal effects
plot(effects2000)
plot(effects2015)
plot(effects_drought)




# NEW PLOTS ####

# Predicted values using lm and lmer models
drivers$pred_gpp_drought <- predict(model_drought, newdata = drivers, re.form = NA)
drivers$pred_gpp_2000 <- predict(model_2000, newdata = drivers, re.form = NA)
drivers$pred_gpp_2015 <- predict(model_2015, newdata = drivers, re.form = NA)

drivers$group <- ifelse(drivers$year %in% c(2000, 2001, 2002, 2004, 2005), "2000-2005",
                        ifelse(drivers$year %in% c(2015, 2016, 2017, 2019, 2020), "2015-2020",
                               ifelse(drivers$year %in% c(2003, 2018), "2003 & 2018", NA)))

palette_drivers <- c("#96DB6B", "#FF8400E0", "#F2E857", "#FF8400E0", "#96DB6B", "#F2E857")

maxTplot <- ggplot(drivers, aes(x = maxT, y = mod_gpp, colour = group, group = group)) + 
  geom_point(aes(colour = group, shape = condition)) +
  # geom_smooth(aes(y = pred_gpp_drought, colour ="LM drought years"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2000, colour ="LMER 2000-2005"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  geom_smooth(se = FALSE, method = 'gam') +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "Max T(°C)", y = "GPP (gC/m²/day)")

plot(maxTplot)


swrplot <- ggplot(drivers, aes(x = swr, y = mod_gpp, colour = group)) + 
  geom_point(aes(colour = group, shape = condition)) +
  # geom_smooth(aes(y = pred_gpp_drought, colour ="LM drought years"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2000, colour ="LMER 2000-2005"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  geom_smooth(se = FALSE, method = 'gam') +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SWR (MJ/m²/day)", y = "GPP (gC/m²/day)")

plot(swrplot)

sm1plot <- ggplot(drivers, aes(x = sm1, y = mod_gpp, colour = group)) + 
  geom_point(aes(colour = group, shape = condition)) +
  geom_smooth(aes(y = pred_gpp_drought, colour ="LM drought years"), se = FALSE, method = "gam") +
  # geom_smooth(se = FALSE) +
  geom_smooth(aes(y = pred_gpp_2000, colour ="LMER 2000-2005"), se = FALSE, method = "gam") +
  geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "gam") +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +http://127.0.0.1:18153/graphics/plot_zoom_png?width=1088&height=672
  labs(x = "SM1 (?)", y = "GPP (gC/m²/day)")

sm2plot <- ggplot(drivers, aes(x = sm2, y = mod_gpp, colour = group)) + 
  geom_point(aes(colour = group, shape = condition)) +
  geom_smooth(se = FALSE, method = 'gam') +
  # geom_smooth(aes(y = pred_gpp_drought, colour ="LM drought years"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2000, colour ="LMER 2000-2005"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SM2 (?)", y = "GPP (gC/m²/day)")

plot(sm2plot)

sm3plot <- ggplot(drivers, aes(x = sm3, y = mod_gpp)) + 
  geom_point(aes(colour = group, shape = condition)) +
  # geom_smooth(se = FALSE, ) +
  # geom_smooth(se = FALSE, method = 'lm', formula = y ~ poly(x,2)) +
  geom_smooth(aes(y = pred_gpp_drought, colour ="LM drought years"), se = FALSE, method = "gam") +
  geom_smooth(aes(y = pred_gpp_2000, colour ="LMER 2000-2005"), se = FALSE, method = "gam") +
  geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "gam") +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SM3 (?)", y = "GPP (gC/m²/day)")

plot(sm3plot)

vpdplot <- ggplot(drivers, aes(x = vpd, y = mod_gpp, colour = group)) + 
  geom_point(aes(colour = group, shape = condition)) +
  geom_smooth(se = FALSE, method = 'gam') +
  # geom_smooth(aes(y = pred_gpp_drought, colour ="LM drought years"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2000, colour ="LMER 2000-2005"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
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
  maxTplot, sm2plot, vpdplot, swrplot, 
  nrow = 2, 
  layout_matrix = rbind(c(1,2), c(3, 4)), 
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



#### RQ2 NMDS ####

install.packages("vegan")
library(vegan)

drivers$group <- ifelse(drivers$year %in% c(2000, 2001, 2002, 2004, 2005), "2000-2005",
                        ifelse(drivers$year %in% c(2015, 2016, 2017, 2019, 2020), "2015-2020",
                               ifelse(drivers$year %in% c(2003), "2003",
                                      ifelse(drivers$year %in% c(2018), "2018", NA))))

drivers_new <- drivers %>%
  select(maxT, airT, sm1, sm2, sm3, vpd, swr, group)
  
drivers_new$group <- ifelse(drivers$year %in% c(2000, 2001, 2002, 2004, 2005), "2000-2005",
                        ifelse(drivers$year %in% c(2015, 2016, 2017, 2019, 2020), "2015-2020",
                               ifelse(drivers$year %in% c(2003), "2003",
                                      ifelse(drivers$year %in% c(2018), "2018", NA))))

# run NMDS
set.seed(123) # for reproducibility
nmds <- metaMDS(drivers_new, distance = "euclidean", k = 2)
nmds_coords <- as.data.frame(scores(nmds, "sites"))
nmds_coords$group <- drivers_new$group
# Plot NMDS
plot(nmds, type = "n") # create empty plot
points(nmds, col = as.factor(drivers_new), pch = 16) # add points colored by GPP
text(nmds, labels = rownames(drivers_new), cex = 0.8, pos = 3) # add sample labels

stressplot(nmds)
nmds$stress

plot(nmds$diss, nmds$dist)

diss_matrix <- vegdist(drivers_new, method = "euclidean")
anosim(diss_matrix, drivers_new$group, permutations = 9999)
# significance = 1e-04; R = 0.1565

en = envfit(nmds, drivers_new, permutations = 999, na.rm = TRUE)
en #this shows you the correlation of each variable with each NMDS
plot(nmds)
plot(en)

nmds_coords <- as.data.frame(scores(nmds, "sites"))
nmds_coords$group <- drivers_new$group

hull.data <- data.frame()
for (i in unique(nmds_coords$group)) {
  temp <- nmds_coords[nmds_coords$group == i, ][chull(nmds_coords[nmds_coords$group == i, c("NMDS1", "NMDS2")]), ]
  hull.data <- rbind(hull.data, temp)
}

en_coords = as.data.frame(scores(en, "vectors")) *ordiArrowMul(en)

ggplot(nmds_coords, aes(x = NMDS1, y = NMDS2)) +
  geom_polygon(data = hull.data, aes(x = NMDS1, y = NMDS2, group = group, fill = group)) +
  geom_point(data = nmds_coords, aes(x = NMDS1, y = NMDS2)) +
  geom_segment(data = en_coords, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               size = 1, alpha = 0.5, colour = "black") +
  geom_text(data = en_coords, aes(x = NMDS1, y = NMDS2), 
            colour = "black", fontface = "bold", label = row.names(en_coords))



### RQ3: Drivers vs response variables ####

sim <- subset(sim, select = c(date, mod_gpp_sim, mod_gpp_unc95_sim, mod_lai_sim, mod_lai_unc95_sim, mod_nee_sim))
calxsim <- merge(sim, data2015, by = c("date"))

calxsim2 <- subset(calxsim, select = c(date, day, mod_gpp_sim, mod_gpp_unc95_sim, mod_gpp, mod_gpp_unc95, obs_gpp, obs_gpp_unc))


# Plot
compared_simulation <- ggplot(calxsim2, aes(x = day)) +
  # geom_ribbon(aes(ymin = obs_gpp - obs_gpp_unc, ymax = obs_gpp + obs_gpp_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_gpp_sim - 1*abs(mod_gpp_unc95_sim), ymax = mod_gpp_sim + 1*abs(mod_gpp_unc95_sim)), fill = "#00AEC974", alpha = 0.3) +
  geom_line(aes(y = mod_gpp, colour = "Mod_cal"), linewidth = 0.5) +
  geom_line(aes(y = mod_gpp_sim, colour = "Mod_sim"), linewidth = 0.5) +
  geom_point(aes(y = obs_gpp, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "GPP (gC/m²/day)", colour = "Data:") +
  scale_color_manual(values = c("Mod_cal" = "#5D1CAD","Mod_sim" = "#00AEC974", "Obs" = "#FF730085", "Obs unc" ="#FF73001F")) +
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

plot(compared_simulation)

ggsave("simXcal_timeseries_plot.png", path = "Plots", plot = compared_simulation, width = 7, height = 5, dpi = 500)


# Validation (simulation)

gpprmsesim <- sqrt(mean((calxsim2$obs_gpp - calxsim2$mod_gpp_sim)^2, na.rm = TRUE))
gpprmsecal <- sqrt(mean((calxsim2$obs_gpp - calxsim2$mod_gpp)^2, na.rm = TRUE))

print(gpprmsesim)
print(gpprmsecal) # missing modelled lai and nee in dataset!


gppcorrelationsim <- cor(calxsim2$mod_gpp_sim, calxsim2$obs_gpp, use = "complete.obs")

gppr_squaredsim <- gppcorrelationsim^2

print(paste("R^2 value:", round(gppr_squaredsim, 3)))

gppcorrelationcal <- cor(calxsim2$mod_gpp, calxsim2$obs_gpp, use = "complete.obs")
gppr_squaredcal <- gppcorrelationcal^2
print(paste("R^2 value:", round(gppr_squaredcal, 3)))


# Print the values
print(gpprmsesim)
print(gpprmsecal) 
print(gppr_squaredsim)
print(gppr_squaredcal)

# Plot the relationships
gppcorrelationcalxsim <- ggplot(calxsim2, aes(y = obs_gpp)) +
  geom_point(aes(x=mod_gpp), colour = "#5D1CAD") +
  geom_point(aes(x=mod_gpp_sim), colour = "#00AEC974") +
  labs(x = "Modelled GPP (gC/m²/day)", y = "Observed GPP (gC/m²/day)") +
  geom_smooth(aes(x=mod_gpp), method = lm, se = FALSE, colour = "#5D1CAD") +
  geom_smooth(aes(x=mod_gpp_sim), method = lm, se = FALSE, colour = "#00AEC974") +
  # geom_abline(intercept = 0, slope = 1, color = "grey", linetype = "dashed", size = 0.6) +
  annotate("text", x = 0, y = 13.85, 
           label = substitute("RMSE" ~ "=" ~ value, list(value = round(gpprmsecal, 3))),
           hjust = 0, vjust = 1,
           size = 4, 
           fontface = "bold", 
           colour = "#5D1CAD") +
  annotate("text", x = 0, y = 12.85, 
           label = substitute("RMSE" ~ "=" ~ value, list(value = round(gpprmsesim, 3))),
           hjust = 0, vjust = 1,
           size = 4, 
           fontface = "bold", 
           colour = "#00AEC974") +
  annotate("text", x = 5, y = 14, 
           label = substitute("R"^2 ~ "=" ~ value, list(value = round(gppr_squaredcal, 3))),
           hjust = 0, vjust = 1,
           size = 4, 
           fontface = "bold", 
           colour = "#5D1CAD") +
  annotate("text", x = 5, y = 13, 
           label = substitute("R"^2 ~ "=" ~ value, list(value = round(gppr_squaredsim, 3))),
           hjust = 0, vjust = 1,
           size = 4, 
           fontface = "bold", 
           colour = "#00AEC974") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9)) +
  scale_x_continuous(limits = c(0,16)) +
  scale_y_continuous(limits = c(0,16))

plot(gppcorrelationcalxsim)

ggsave("simXcal_correlation_plot.png", path = "Plots", plot = gppcorrelationcalxsim, width = 5, height = 5, dpi = 500)



# Now just for 2018

compared_simulation2018 <- ggplot(calxsim2, aes(x = day)) +
  # geom_ribbon(aes(ymin = obs_gpp - obs_gpp_unc, ymax = obs_gpp + obs_gpp_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_gpp_sim - 1*abs(mod_gpp_unc95_sim), ymax = mod_gpp_sim + 1*abs(mod_gpp_unc95_sim)), fill = "#00AEC974", alpha = 0.3) +
  geom_line(aes(y = mod_gpp, colour = "Mod_cal"), linewidth = 0.5) +
  geom_line(aes(y = mod_gpp_sim, colour = "Mod_sim"), linewidth = 0.5) +
  geom_point(aes(y = obs_gpp, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "GPP (gC/m²/day)", colour = "Data:") +
  scale_color_manual(values = c("Mod_cal" = "#5D1CAD","Mod_sim" = "#00AEC974", "Obs" = "#FF730085", "Obs unc" ="#FF73001F")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_x_continuous(breaks = c(182, 553, 917, 1281, 1645, 2009), 
                     labels = c("2015", "2016", "2017", "2018", "2019", "2020"),
                     expand = c(0, 0),
                     limits = c(1099, 2184)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,17))

plot(compared_simulation2018)




