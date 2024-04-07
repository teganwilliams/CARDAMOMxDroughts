#### Calculating Z-scores for the full met dataset

# Load libraries
library(dplyr)
library(tidyverse)
library(ggplot2) 

# Load Meteorological Data
met_data <- read.csv("Data/DE-Hai-2000-2020-weekly_timeseries_metSM.csv", header = TRUE)
obs_data <- read.csv("Data/DE-Hai-2000-2020-weekly_timeseries_obs.csv", header = TRUE)

# Delete 2021 data from Met data set since only NA values (-9999)
rows_to_delete <- c(1093:1144)
met_data <- met_data[-rows_to_delete, ]


# Create new column for year, months and full dates 
rows_per_year <- 52
met_data$year <- rep(2000:2020, each = rows_per_year)
met_data$full_date <- as.Date(paste0(met_data$year, "-", met_data$doy), format = "%Y-%j")
met_data <- met_data %>%
  mutate(month = month(full_date, label = TRUE))


### Create weekly averages for sm1, sm2, sm3, maxT, vpd, swr and precip
  
weekly_average_data <- met_data %>%
  group_by(week = ceiling(as.numeric(doy)/7)) %>%
  summarise(meanT = mean(maxt_C, na.rm = TRUE),
            sdT = sd(maxt_C, na.rm = TRUE),
            meanSM1 = mean(SWC_1, na.rm = TRUE),
            sdSM1 = sd(SWC_1, na.rm = TRUE),
            meanSM2 = mean(SWC_2, na.rm = TRUE),
            sdSM2 = sd(SWC_2, na.rm = TRUE),
            meanSM3 = mean(SWC_3, na.rm = TRUE),
            sdSM3 = sd(SWC_3, na.rm = TRUE),
            meanVPD = mean(vpd_kPa, na.rm = TRUE),
            sdVPD = sd(vpd_kPa, na.rm = TRUE),
            meanSWR = mean(swrad_MJm2day, na.rm = TRUE),
            sdSWR = sd(swrad_MJm2day, na.rm = TRUE),
            meanPrecip = mean(precip_kgm2s, na.rm = TRUE),
            sdPrecip = sd(precip_kgm2s, na.rm = TRUE),
            )

fullmet_data1 <- cbind(weekly_average_data, met_data)

metfull <- fullmet_data1 %>%
  rename(date = full_date, maxT = maxt_C, sm1 = SWC_1, sm2 = SWC_2, sm3 = SWC_3, vpd = vpd_kPa, swr = swrad_MJm2day, precip = precip_kgm2s)


#### Calculating z-scores 

# sm_anomalies <- read.csv("/exports/csce/datastore/geos/groups/gcel/for_Tegan/sm_anomalies.csv")
# temp_anomalies <- read.csv("/exports/csce/datastore/geos/groups/gcel/for_Tegan/temp_anomalies.csv")

# Filter for summer months

metfull <- fullmet %>%
  arrange(year)

summermet <- fullmet %>%
  filter(week >= 18 & week <= 36)

# Z-scores 
hist(summermet$meanT)
shapiro.test(summermet$meanT) # non-normal distribution for full-dataset, but normal for summer filtering


# Calculate z-scores for temperature anomalies

# Calculate z-scores for temperature anomalies
temp_anomalies <- subset(metfull, select = c(year, week, maxT, meanT, sdT))
metfull <- metfull %>%
  arrange(year)
maxT <- metfull$maxT
meanT <- metfull$meanT
sdT <- metfull$sdT
temp_z_scores <- (maxT - meanT) / sdT
temp_z_scores <- as.data.frame(temp_z_scores)
temp_z_scores$order <- seq_len(nrow(temp_z_scores))
temp_anomalies$order <- seq_len(nrow(temp_anomalies))
merged_data_temp <- merge(temp_z_scores, temp_anomalies , by = (c("order")))
merged_data_temp <- merged_data_temp %>%
  arrange(year)


# Calculate z-scores for SM 1 anomalies
sm1_anomalies <- subset(metfull, select = c(year, week, sm1, meanSM1, sdSM1))
sm1_anomalies <- metfull %>%
  arrange(year)
sm1 <- metfull$sm1
meanSM1 <- metfull$meanSM1
sdSM1 <- metfull$sdSM1
sm1_z_scores <- (sm1 - meanSM1) / sdSM1
sm1_z_scores <- as.data.frame(sm1_z_scores)
sm1_anomalies$order <- seq_len(nrow(sm1_anomalies))
sm1_z_scores$order <- seq_len(nrow(sm1_z_scores))
merged_data_sm1 <- merge(sm1_z_scores, sm1_anomalies , by = (c("order")))
merged_data_sm1 <- merged_data_sm1 %>%
  arrange(year)

# sm1_z_scores2 <- merge(sm1_z_scores, sm1_anomalies, by = c("order"), all.x = TRUE)
# sm1_z_scores3 <- subset(sm1_z_scores2, select = c(year, week, sm1_z_scores))
# incomplete_template2 <- expand.grid(year = 2000, week = 1:9, sm1_z_scores = NA)
# merged_template <- rbind(incomplete_template, incomplete_template2)
# merged_data_sm1 <- rbind(incomplete_template2, sm1_z_scores3)
# merged_data_sm1 <- merged_data_sm1 %>%
  # arrange(year)


# SM2 
sm2_anomalies <- metfull %>%
  arrange(year)
sm2_anomalies <- subset(metfull, values = c(year, week, sm2, meanSM2, sdSM2))
sm2 <- metfull$sm2
meanSM2 <- metfull$meanSM2
sdSM2 <- metfull$sdSM2
sm2_z_scores <- (sm2 - meanSM2) / sdSM2
sm2_z_scores <- as.data.frame(sm2_z_scores)
sm2_anomalies$order <- seq_len(nrow(sm2_anomalies))
sm2_z_scores$order <- seq_len(nrow(sm2_z_scores))
merged_data_sm2 <- merge(sm2_z_scores, sm2_anomalies , by = (c("order")))
merged_data_sm2 <- merged_data_sm2 %>%
  arrange(year)

# SM3
sm3_anomalies <- metfull %>%
  arrange(year)
sm3_anomalies <- subset(metfull, values = c(year, week, sm3, meanSM3, sdSM3))
sm3 <- metfull$sm3
meanSM3 <- metfull$meanSM3
sdSM3 <- metfull$sdSM3
sm3_z_scores <- (sm3 - meanSM3) / sdSM3
sm3_z_scores <- as.data.frame(sm3_z_scores)
sm3_anomalies$order <- seq_len(nrow(sm3_anomalies))
sm3_z_scores$order <- seq_len(nrow(sm3_z_scores))
merged_data_sm3 <- merge(sm3_z_scores, sm3_anomalies , by = (c("order")))
merged_data_sm3 <- merged_data_sm3 %>%
  arrange(year)



# VPD
vpd_anomalies <- metfull %>%
  arrange(year)
vpd_anomalies <- subset(metfull, values = c(year, week, vpd, meanVPD, sdVPD))
vpd <- metfull$vpd
meanVPD <- metfull$meanVPD
sdVPD <- metfull$sdVPD
vpd_z_scores <- (vpd - meanVPD) / sdVPD
vpd_z_scores <- as.data.frame(vpd_z_scores)
vpd_anomalies$order <- seq_len(nrow(vpd_anomalies))
vpd_z_scores$order <- seq_len(nrow(vpd_z_scores))
merged_data_vpd <- merge(vpd_z_scores, vpd_anomalies , by = (c("order")))
merged_data_vpd <- merged_data_vpd %>%
  arrange(year)


# SWR
swr_anomalies <- metfull %>%
  arrange(year)
swr_anomalies <- subset(metfull, values = c(year, week, swr, meanSWR, sdSWR))
swr <- metfull$swr
meanSWR <- metfull$meanSWR
sdSWR <- metfull$sdSWR
swr_z_scores <- (swr - meanSWR) / sdSWR
swr_z_scores <- as.data.frame(swr_z_scores)
swr_anomalies$order <- seq_len(nrow(swr_anomalies))
swr_z_scores$order <- seq_len(nrow(swr_z_scores))
merged_data_swr <- merge(swr_z_scores, swr_anomalies, by = c("order"), all.x = TRUE)
merged_data_swr <- merged_data_swr %>%
  arrange(year)


# Precip
precip_anomalies <- metfull %>%
  arrange(year)
precip_anomalies <- subset(metfull, values = c(year, week, precip, meanPrecip, sdPrecip))
precip <- metfull$precip
meanPrecip <- metfull$meanPrecip
sdPrecip <- metfull$sdPrecip
precip_z_scores <- (precip - meanPrecip) / sdPrecip
precip_z_scores <- as.data.frame(precip_z_scores)
precip_anomalies$order <- seq_len(nrow(precip_anomalies))
precip_z_scores$order <- seq_len(nrow(precip_z_scores))
merged_data_precip <- merge(precip_z_scores, precip_anomalies, by = c("order"), all.x = TRUE)
merged_data_precip <- merged_data_precip %>%
  arrange(year)


# All combining anomalies
anomalies_combined <- cbind(merged_data_temp, merged_data_sm1, merged_data_sm2, merged_data_sm3, merged_data_vpd, merged_data_swr,merged_data_precip, by = (c("order")))

View(anomalies_combined)
anomalies1 <- subset(anomalies_combined, select = c(date, year, month, week, doy, temp_z_scores, sm1_z_scores, sm2_z_scores, sm3_z_scores, vpd_z_scores, swr_z_scores, precip_z_scores))

anomalies <- anomalies1 %>%
  filter(year >= 2000 & year <= 2005 | year >=2015 & year <= 2020)

setwd("/exports/csce/datastore/geos/groups/gcel/for_Tegan/")
write.csv(anomalies, "fullanomalies.csv")


# GPP anomaly

gpp_all <- datafull %>%
  filter(year >= 2000 & year <= 2005 | year >= 2015 & year <= 2020)

gpp_anomalies1 <- gpp_all %>%
  group_by(week = ceiling(as.numeric(doy)/7)) %>%
  summarise(meanGPP = mean(mod_gpp, na.rm = TRUE),
            sdGPP = sd(mod_gpp, na.rm = TRUE))

gpp_anomalies <- cbind(gpp_anomalies1, gpp_all)

gpp_anom <- subset(gpp_anomalies, select = c(date, year, week, doy, mod_gpp, meanGPP, sdGPP))

gpp_anom <- gpp_anom %>%
  arrange(year)

gpp <- gpp_anom$mod_gpp
meanGPP <- gpp_anom$meanGPP
sdGPP <- gpp_anom$sdGPP
gpp_z_scores <- (gpp - meanGPP) / sdGPP
gpp_z_scores <- as.data.frame(gpp_z_scores)
gpp_z_scores$order <- seq_len(nrow(gpp_z_scores))
gpp_anom$order <- seq_len(nrow(gpp_anom))
merged_data_gpp <- merge(gpp_z_scores, gpp_anom , by = (c("order")))
# temp_z_scores <- temp_z_scores %>%
# filter(week >= 18 & week <= 36) 
View(merged_data_gpp)


# adding gpp data
gppanomalies2015 <- merged_data_gpp %>%
  filter(year >= 2015 & year <= 2020)
merged_data2015 <- x %>% 
  filter(year >= 2015 & year <= 2020) %>%
  select(date, year, week, mod_gpp, mod_gpp_unc95)

merged_data_full <- cbind(merged_data_gpp, anomalies)
merged_data <- subset(merged_data_full, select = c(date, year, month, week, doy, gpp_z_scores, temp_z_scores, sm1_z_scores, sm2_z_scores, sm3_z_scores, vpd_z_scores, swr_z_scores))

setwd("/exports/csce/datastore/geos/groups/gcel/for_Tegan/")
write.csv(merged_data, "fullanomalies.csv")


fully_merged_summer <- merged_data %>%
  filter(week >= 22 & week <= 37)


# Visualisations ####

fully_merged <- read.csv("Data/fullanomalies.csv", header = TRUE)
fully_merged_summer <- fully_merged %>%
  filter(week >= 22 & week <= 40)

# Scatter plot of temperature anomalies vs soil moisture anomalies
ggplot(fully_merged_summer, aes(x = temp_z_scores, y = sm1_z_scores)) +
  geom_point() +
  labs(x = "Summer temperature Anomalies", y = "Summer Soil Moisture Anomalies") +
  ggtitle("Summer Temperature Anomalies vs Soil Moisture Anomalies")

ggplot(fully_merged_summer, aes(x = year, y = gpp_z_scores)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, colour = "red", se = FALSE) +
  labs(x = "Temperature Anomalies", y = "GPP") +
  ggtitle("Summer Temperature Anomalies vs GPP")

# Scatter plot of temperature anomalies vs GPP
ggplot(fully_merged_summer, aes(x = temp_z_scores, y = gpp_z_scores)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, colour = "red", se = FALSE) +
  labs(x = "Temperature Anomalies", y = "GPP") +
  ggtitle("Summer Temperature Anomalies vs GPP")

ggplot(fully_merged_summer, aes(x = sm3_z_scores, y = gpp_z_scores)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, colour = "red", se = FALSE) +
  labs(x = "SM Anomalies", y = "GPP") +
  ggtitle("Summer SM Anomalies vs GPP")

hist(fully_merged_summer$gpp_z_scores)
shapiro.test(fully_merged_summer$gpp_z_scores)



drivers <- fully_merged_summer %>%
  mutate(condition = ifelse(year %in% c(2003, 2018), "drought", "normal"))

drivers$year_group <- ifelse(drivers$year %in% c(2000, 2001, 2002, 2004, 2005, 2015, 2016, 2017, 2019, 2020), "normal",
                             # ifelse(drivers$year %in% c(2015, 2016, 2017, 2019, 2020), "2015-2020",
                             ifelse(drivers$year %in% c(2003), "2003", 
                                    ifelse(drivers$year %in% c(2018), "2018", NA)))

non_drought <- drivers %>%
  filter(!(year %in% c(2003, 2018)))

non_drought <- non_drought %>%
  group_by(doy) %>%
  mutate(mean_gpp = mean(gpp_z_scores, na.rm = TRUE),
         mean_maxT = mean(temp_z_scores, na.rm = TRUE),
         mean_sm1 = mean(sm1_z_scores, na.rm = TRUE),
         mean_sm2 = mean(sm2_z_scores, na.rm = TRUE),
         mean_sm3 = mean(sm3_z_scores, na.rm = TRUE),
         mean_vpd = mean(vpd_z_scores, na.rm = TRUE),
         mean_swr = mean(swr_z_scores, na.rm = TRUE)) %>%
  ungroup()

drought1 <- drivers %>%
  filter((year %in% c(2003, 2018)))

drought1 <- drought1 %>%
  group_by(year) %>%
  mutate(mean_maxT = temp_z_scores, mean_gpp = gpp_z_scores, mean_sm1 = sm1_z_scores, mean_sm2 = sm2_z_scores, mean_sm3 = sm3_z_scores, mean_vpd = vpd_z_scores, mean_swr = swr_z_scores) %>%
  ungroup()

new <- rbind(drought1, non_drought)

palette_anomalies <- c("#D6D6D686", "#D6A400", "#B80422", "#7362BA")
palette_anomalies <- c("#29B071", "#D6A400", "darkgrey")


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


maxT_plot <- ggplot(new, aes(x = doy, y = mean_maxT, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(linewidth = 0.8) +
  labs(title = "",
       x = "Time (months)",
       y = "Max temperature z-score",
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

sm1_plot <- ggplot(new, aes(x = doy, y = mean_sm2, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Time (month)",
       y = "Soil moisture 1 z-score",
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

plot(sm1_plot)


sm2_plot <- ggplot(new, aes(x = doy, y = mean_sm2, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Time (month)",
       y = "Soil moisture 2 z-score",
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
       y = "VPD z-score",
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
       y = "SWR z-score",
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





hist(non_drought$gpp_z_scores)
shapiro.test(non_drought$gpp_z_scores)

hist(drought$gpp_z_scores)
shapiro.test(drought$gpp_z_scores)

# plot z-scores ####
palette_anomalies <- c("#D6A400", "#B80422", "darkgrey")

gpp_plotz <- ggplot(new, aes(x = doy, y = mean_gpp, group = year_group, colour = year_group, linetype = condition)) +
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

plot(gpp_plotz)

maxT_plotz <- ggplot(new, aes(x = doy, y = mean_maxT, group = year_group, colour = year_group, linetype = condition)) +
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

plot(maxT_plotz)

sm1_plotz <- ggplot(new, aes(x = doy, y = mean_sm1, group = year_group, colour = year_group, linetype = condition)) +
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

plot(sm1_plotz)



sm2_plotz <- ggplot(new, aes(x = doy, y = mean_sm2, group = year_group, colour = year_group, linetype = condition)) +
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

plot(sm2_plotz)

sm3_plotz <- ggplot(new, aes(x = doy, y = mean_sm3, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Time (month)",
       y = "Soil moisture at depth 3 (X)",
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

plot(sm3_plotz)


vpd_plotz <- ggplot(new, aes(x = doy, y = mean_vpd, group = year_group, colour = year_group, linetype = condition)) +
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
plot(vpd_plotz)

swr_plotz <- ggplot(new, aes(x = doy, y = mean_swr, group = year_group, colour = year_group, linetype = condition)) +
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
plot(swr_plotz)


### Statistical tests ####
install.packages("ppcor")
library(ppcor)
library(car)

drought <- drought1 %>%
  filter((year %in% c(2003)))
drought <- drought1 %>%
  filter((year %in% c(2018)))
drought <- drought1 %>%
  filter((year %in% c(2003, 2018)))


drought <- drought %>%
  rename(mod_gpp = gpp_z_scores, maxT = temp_z_scores, sm1 = sm1_z_scores, sm2 = sm2_z_scores,
         sm3 = sm3_z_scores, vpd = vpd_z_scores, swr = swr_z_scores)


non_drought <- na.omit(non_drought)

pcor_sm3 <- pcor.test(non_drought$gpp_z_scores, non_drought$sm3_z_scores, 
                           x = non_drought[, c("temp_z_scores", "swr_z_scores", "vpd_z_scores", "sm1_z_scores", "sm2_z_scores")])
pcor_sm2 <- pcor.test(non_drought$gpp_z_scores, non_drought$sm2_z_scores, 
                      x = non_drought[, c("temp_z_scores", "swr_z_scores", "vpd_z_scores", "sm1_z_scores", "sm3_z_scores")])
pcor_sm1 <- pcor.test(non_drought$gpp_z_scores, non_drought$sm1_z_scores, 
                      x = non_drought[, c("temp_z_scores", "swr_z_scores", "vpd_z_scores", "sm3_z_scores", "sm2_z_scores")])
pcor_maxT <- pcor.test(non_drought$gpp_z_scores, non_drought$temp_z_scores, 
                      x = non_drought[, c("sm3_z_scores", "swr_z_scores", "vpd_z_scores", "sm1_z_scores", "sm2_z_scores")])
pcor_vpd <- pcor.test(non_drought$gpp_z_scores, non_drought$vpd_z_scores, 
                      x = non_drought[, c("temp_z_scores", "swr_z_scores", "sm3_z_scores", "sm1_z_scores", "sm2_z_scores")])
pcor_swr <- pcor.test(non_drought$gpp_z_scores, non_drought$swr_z_scores, 
                      x = non_drought[, c("temp_z_scores", "sm3_z_scores", "vpd_z_scores", "sm1_z_scores", "sm2_z_scores")])


print(pcor_sm1)
print(pcor_sm2)
print(pcor_sm3)
print(pcor_maxT)
print(pcor_vpd)
print(pcor_swr)



pcor_test_temp <- pcor.test(non_drought$gpp_z_scores, non_drought$temp_z_scores, 
                           x = non_drought[, c("sm3_z_scores", "swr_z_scores")])


pcor_test_swr <- pcor.test(non_drought$gpp_z_scores, non_drought$swr_z_scores, 
                            x = non_drought[, c("sm3_z_scores", "temp_z_scores")])


pcor_test_vpd <- pcor.test(non_drought$mod_gpp, non_drought$precip, 
                           x = non_drought[, c("sm3", "maxT", "swr", "precip")])



cor_results <- pcor(non_drought[, c("mod_gpp", "maxT","sm2", "swr")])
print(pcor_test_sm3)

# Drought

# Compute partial correlations
pcor_results <- pcor(drought[, c("mod_gpp", "maxT", "sm2", "vpd", "swr")])

# Print results
print(pcor_results)

cor(drought$vpd, drought$maxT)

pcor_test_sm3 <- pcor.test(drought$mod_gpp, drought$sm3, 
                           x = drought[, c("maxT", "swr")])

print(pcor_test_sm3)
# Partial correlation test for sm3
pcor_test_sm3 <- pcor.test(drought$mod_gpp, drought$sm3, 
                           x = drought[, c("maxT", "vpd", "swr", "sm2", "sm1")])

print(pcor_test_sm3)

pcor_test_maxT <- pcor.test(drought$mod_gpp, drought$maxT, 
                           x = drought[, c("sm3", "vpd", "swr", "sm2", "sm1")])

print(pcor_test_maxT)

pcor_test_sm1 <- pcor.test(drought$mod_gpp, drought$sm1, 
                            x = drought[, c("sm3", "vpd", "swr", "sm2", "maxT")])
print(pcor_test_sm1)

pcor_test_sm2 <- pcor.test(drought$mod_gpp, drought$sm2, 
                           x = drought[, c("sm3", "vpd", "swr", "sm1", "maxT")])
print(pcor_test_sm2)

pcor_test_vpd <- pcor.test(drought$mod_gpp, drought$vpd, 
                           x = drought[, c("sm3", "sm2", "swr", "sm1", "maxT")])
print(pcor_test_vpd)

pcor_test_swr <- pcor.test(drought$mod_gpp, drought$swr, 
                           x = drought[, c("sm3", "sm2", "vpd", "sm1", "maxT")])
print(pcor_test_swr)


# for 2003 drought, strong correlation for SM3 (0.91); the rest are non-significant
# for 2018 drought, medium correlation for SM3 (0.50) and maxT (0.50); no correlation for the rest.
# by combining both droughts, strong cor for SM3 (0.74); medium for maxT (0.50); strong for vpd (-0.83) and swr (-0.83)

pcor_test_maxT <- pcor.test(drought$mod_gpp, drought$maxT, 
                            x = drought[, c("sm3", "sm2")])

print(pcor_test_maxT)

# Partial correlation between mod_gpp and vpd, controlling for sm1, sm2, sm3, maxT, swr
pcor_test_vpd <- pcor.test(drought$mod_gpp, drought$vpd, 
                           x = drought[, c("sm3", "maxT", "swr")])

print(pcor_test_vpd)

# Partial correlation between mod_gpp and swr, controlling for sm1, sm2, sm3, maxT, vpd
pcor_test_swr <- pcor.test(drought$mod_gpp, drought$swr, 
                           x = drought[, c("sm3","vpd","maxT")])




print(pcor_test_swr)

# Results
pcor_results_2003 <- data.frame(
  Variable = c("sm3", "maxT", "swr"),
  Partial_Correlation = c(pcor_test_sm3$estimate, pcor_test_maxT$estimate, pcor_test_swr$estimate),
  P_Value = c(pcor_test_sm3$p.value, pcor_test_maxT$p.value, pcor_test_swr$p.value)
)

print(pcor_results_nondrought)
print(pcor_results_drought)

print(pcor_results2015)
print(pcor_results2000)

print(pcor_results_2003)
print(pcor_results_2018)

print(pcor_resultsdrought)
print(pcor_results_non_drought)

partial_corr_values <- pcor_results_non_drought$Partial_Correlation[3:5]
print(partial_corr_values)

pcor_results <- pcor(as.matrix(non_drought[, c("mod_gpp", "sm3", "vpd", "maxT", "swr", "precip")]), method = "pearson")

print(pcor_results)







### test out the data #####

library(corrplot)

# Calculate correlation coefficients
correlation_matrix <- cor(fully_merged_summer[c("temp_z_scores", "sm_z_scores", "mod_gpp")])

# Print correlation coefficients
print(correlation_matrix)

# Visualize correlation matrix
corrplot(correlation_matrix, method = "circle", type = "upper", tl.cex = 0.8)

# Visualize scatter plots
# Scatter plot of temperature anomalies vs soil moisture anomalies
ggplot(fully_merged_summer, aes(x = temp_z_scores, y = sm1_z_scores)) +
  geom_point() +
  labs(x = "Summer temperature Anomalies", y = "Summer Soil Moisture Anomalies") +
  ggtitle("Summer Temperature Anomalies vs Soil Moisture Anomalies")

# Scatter plot of temperature anomalies vs GPP
ggplot(fully_merged_summer, aes(x = temp_z_scores, y = gpp_z_scores)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, colour = "red", se = FALSE) +
  labs(x = "Temperature Anomalies", y = "GPP") +
  ggtitle("Summer Temperature Anomalies vs GPP")

drought2 <- drought %>%
  filter(doy >= 154 & doy <= 259)

ggplot(drought2, aes(x = swr_z_scores, y = gpp_z_scores)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, colour = "red", se = FALSE) +
  labs(x = "SM Anomalies", y = "GPP") +
  ggtitle("Summer SM Anomalies vs GPP")

## Regression analysis ####

# linear regression

model_all <- lm(gpp_z_scores ~ temp_z_scores + sm3_z_scores + sm2_z_scores + vpd_z_scores + swr_z_scores , data = fully_merged_summer)
summary(model_all)
plot(model_all)


residuals_lmer <- resid(model_all)
shapiro.test(residuals_lmer)
qqnorm(residuals_lmer) 
qqline(residuals_lmer)


# Multiple linear regression

mod_gpp <- fully_merged_summer$mod_gpp
temp_z <- fully_merged_summer$temp_z_scores
sm_z <- fully_merged_summer$sm_z_scores

model_all <- lm(gpp_z_scores ~ temp_z_scores + sm3_z_scores + sm2_z_scores + vpd_z_scores + swr_z_scores , data = fully_merged_summer)
summary(model_all)
plot(model_all)


residuals_lmer <- resid(model_non_drought)
shapiro.test(residuals_lmer)
qqnorm(residuals_lmer) 
qqline(residuals_lmer)

model_interaction <- lm(mod_gpp ~ temp_z_scores*sm_z_scores, data = fully_merged_summer)
summary(model_interaction)
plot(model_both)


# Model for the effect of temperature on GPP
model_temp <- lm(mod_gpp ~ temp_z_scores, data = fully_merged_summer)
summary(model_temp)
plot(model_temp)

# Model for the effect of soil moisture on GPP
model_sm <- lm(mod_gpp ~ sm_z_scores, data = fully_merged_summer)
summary(model_sm)

model_met <- lm(sm_z_scores ~ temp_z_scores, data = fully_merged_summer)
summary(model_met)

# Mixed effect with random effect of time as year
library(lme4)

mixed_model <- lmer(mod_gpp ~ sm_z_scores + temp_z_scores + (1 | year), data = fully_merged_summer)
summary(mixed_model)
mixed_modelSM <- lmer(mod_gpp ~ sm_z_scores + (1 | year), data = fully_merged_summer)
summary(mixed_modelSM)

##### Plots of trends by drought vs non drought (linear)
fully_merged_summer <- fully_merged_summer %>%
  mutate(condition = ifelse(year %in% c(2003, 2018), "drought", "normal"))

drivers$year_group <- as.factor(drivers$year_group)

drivers$year_group <- ifelse(drivers$year %in% c(2000, 2001, 2002, 2004, 2005, 2015, 2016, 2017, 2019, 2020), "normal",
                             # ifelse(drivers$year %in% c(2015, 2016, 2017, 2019, 2020), "2015-2020",
                             ifelse(drivers$year %in% c(2003), "2003", 
                                    ifelse(drivers$year %in% c(2018), "2018", NA)))



#### Anomalie z-score timeseries plots #####
temp_drought_years <- anomalies_combined %>%
  filter(year %in% c(2003,2010,2018,2019,2020)) %>%
  pull(year) %>%
  unique()

combined <- anomalies_combined

combined['status'] = '2000-2020'
combined$status[combined$year %in% temp_drought_years] <- as.character(combined$year[combined$year %in% temp_drought_years])
combined$status <- as.factor(combined$status)

palette_anomalies <- c("#D6D6D686", "#D6A400","#0b9bd4", "#B80422", "#7362BA", "#3EA85A")

temp_anomaly_plot <- ggplot(combined, aes(x = week, y = temp_z_scores, colour = status, group = year)) +
  geom_line(size = 0.8) +
  geom_hline(yintercept = 0, size = 0.4, colour = "black") +
 #  geom_hline(yintercept = 4.69, linetype = "dashed", size = 0.7, colour = "#FC6C19BE") +
  # geom_text(aes(x = 35.2, y = -0.4, label = "Norm"), colour = "black") + 
  # geom_text(aes(x = 34, y = 5.2, label = "95th percentile"), colour = "darkorange", size = 3) + 
  labs(title = "",
       x = "Summer months",
       y = "Air temperature anomaly (z-score)",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(18, 22, 27, 32, 36), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"),
                     expand = c(0, 0),
                     limits = c(18, 36)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-2,3))

plot(temp_anomaly_plot)

sm_anomaly_plot <- ggplot(combined, aes(x = week, y = sm_z_scores, group = year, colour = status)) +
  geom_line(size = 0.8) +
  geom_hline(yintercept = 0, size = 0.4, colour = "black") +
  # geom_hline(yintercept = -8.06, linetype = "dashed", size = 0.7, colour = "#FC6C19BE") +
  # geom_text(aes(x = 35.1, y = 1, label = "Norm"), colour = "black") + 
  # geom_text(aes(x = 34, y = -7, label = "80th percentile"), colour = "darkorange", size = 3) +  
  labs(title = "",
       x = "Summer months",
       y = "Deep soil moisture anomalie (z-score)",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(18, 22, 27, 32, 36), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"),
                     expand = c(0, 0),
                     limits = c(18, 36)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-4,2))

plot(sm_anomaly_plot)

# plot them as a panel
library(gridExtra)

(combined_anomaly_plot <- grid.arrange(temp_anomaly_plot, sm_anomaly_plot, nrow = 2, layout_matrix = rbind(c(1, 2)), heights = c(1, 1)))

# Display the combined plot
print(combined_anomaly_plot)

save(combined_anomaly_plot, file = "combined_anomaly_plot.png")





### Models ####
# Fit the model again
library(lme4)
install.packages("glmmTMB")
library(glmmTMB)
library(dplyr)

non2000 <- non_drought %>%
  filter(year >= 2000 & year <= 2005)

non2015 <- non_drought %>%
  filter(year >= 2015 & year <= 2020)

drought <- fully_merged_summer %>%
  filter((year %in% c(2003,2018)))

drought <- drought1 %>%
  filter((year %in% c(2003)))
drought <- drought1 %>%
  filter((year %in% c(2018)))
drought <- drought1 %>%
  filter((year %in% c(2003,2018)))


# check linearity between gpp and met drivers

ggplot(x = non_drought$temp_z_scores, y = non_drought$gpp_z_scores) + 
  geom_scatter()


drought <- drought %>%
  rename(mod_gpp = gpp_z_scores, maxT = temp_z_scores, sm1 = sm1_z_scores, sm2 = sm2_z_scores,
         sm3 = sm3_z_scores, vpd = vpd_z_scores, swr = swr_z_scores)

model_drought <- glmmTMB(gpp_z_scores ~ temp_z_scores + sm2_z_scores + sm3_z_scores + swr_z_scores, 
                             data = drought, 
                             family = tweedie(link = "log"))
summary(model_drought)
residuals_lmer <- resid(model_drought)
shapiro.test(residuals_lmer)
qqnorm(residuals_lmer) 
qqline(residuals_lmer)

model_non_drought <- glmmTMB(gpp_z_scores ~ temp_z_scores + sm2_z_scores + sm3_z_scores + swr_z_scores, 
                             data = non_drought, 
                             family = tweedie(link = "log"))
summary(model_non_drought)

model_non_drought2000 <- glmmTMB(gpp_z_scores ~ temp_z_scores + sm2_z_scores + sm3_z_scores + swr_z_scores + (1|year), 
                                 data = non2000, 
                                 family = tweedie(link = "log"))


model_non_drought <- glm(gpp_z_scores ~  temp_z_scores * sm2_z_scores * sm3_z_scores * swr_z_scores, data = non_drought, family = gaussian(link = "identity"))
summary(model_non_drought)

residuals_lmer <- resid(model_non_drought)
shapiro.test(residuals_lmer)
qqnorm(residuals_lmer) 
qqline(residuals_lmer)


model_drought <- glm(mod_gpp ~  maxT * sm2 * sm3 * swr, data = drought, family = gaussian(link = "identity"))
summary(model_drought)

residuals_lmer <- resid(model_drought)
shapiro.test(residuals_lmer)
qqnorm(residuals_lmer) 
qqline(residuals_lmer)




