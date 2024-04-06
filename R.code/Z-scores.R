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

fullmet <- fullmet_data1 %>%
  rename(date = full_date, maxT = maxt_C, sm1 = SWC_1, sm2 = SWC_2, sm3 = SWC_3, vpd = vpd_kPa, swr = swrad_MJm2day, precip = precip_kgm2s)


#### Calculating z-scores 

# sm_anomalies <- read.csv("/exports/csce/datastore/geos/groups/gcel/for_Tegan/sm_anomalies.csv")
# temp_anomalies <- read.csv("/exports/csce/datastore/geos/groups/gcel/for_Tegan/temp_anomalies.csv")

# Filter for summer months

fullmet <- fullmet %>%
  arrange(year)

summer_met <- fullmet %>%
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


# All combining anomalies
anomalies_combined <- cbind(merged_data_temp, merged_data_sm1, merged_data_sm2, merged_data_sm3, merged_data_vpd, merged_data_swr, by = (c("order")))

View(anomalies_combined)
anomalies1 <- subset(anomalies_combined, select = c(date, year, month, week, doy, temp_z_scores, sm1_z_scores, sm2_z_scores, sm3_z_scores, vpd_z_scores, swr_z_scores))

anomalies <- anomalies1 %>%
  filter(year >= 2000 & year <= 2005 | year >=2015 & year <= 2020)

setwd("/exports/csce/datastore/geos/groups/gcel/for_Tegan/")
write.csv(anomalies, "fullanomalies.csv")


# GPP anomaly
gpp_anomalies <- obs %>%
  mutate(group_by = week, mean(mod_gpp)) %>% 
  select(year, week, mod_gpp, meanGPP, sdGPP)

gpp <- gpp_anomalies$mod_gpp
meanGPP <- gpp_anomalies$meanGPP
sdGPP <- gpp_anomalies$sdGPP
gpp_z_scores <- (gpp - meanGPP) / sdGPP
gpp_z_scores <- as.data.frame(gpp_z_scores)
gpp_z_scores$order <- seq_len(nrow(gpp_z_scores))
gpp_anomalies$order <- seq_len(nrow(gpp_anomalies))
gpp <- merge(gpp_z_scores, gpp_anomalies , by = (c("order")))
# temp_z_scores <- temp_z_scores %>%
 # filter(week >= 18 & week <= 36) 
View(gpp_z_scores)








# adding gpp data
anomalies2015 <- anomalies_combined %>%
  filter(year >= 2015 & year <= 2020)
merged_data2015 <- merged_data2015 %>%
  filter(year >= 2015 & year <= 2020) %>%
  select(date, year, week, mod_gpp, mod_gpp_unc95)
merged_gpp2015x <- merge(anomalies2015, merged_data2015, by = c("year", "week"))
merged_gpp2015 <- merged_gpp2015x %>%
  arrange(date)

anomalies2000 <- anomalies_combined %>%
  filter(year >= 2000 & year <= 2005)
merged_data2000 <- merged_data2000 %>%
  filter(year >= 2000 & year <= 2005) %>%
  select(date, year, week, mod_gpp, mod_gpp_unc95)
merged_gpp2000x <- merge(anomalies2000, merged_data2000, by = c("year", "week"))
merged_gpp2000 <- merged_gpp2000x %>%
  arrange(date)

fully_merged <- rbind(merged_gpp2000, merged_gpp2015)
fully_merged_summer <- fully_merged %>%
  filter(week >= 18 & week <= 36) 



### test out the data

library(corrplot)

# Calculate correlation coefficients
correlation_matrix <- cor(fully_merged_summer[c("temp_z_scores", "sm_z_scores", "mod_gpp")])

# Print correlation coefficients
print(correlation_matrix)

# Visualize correlation matrix
corrplot(correlation_matrix, method = "circle", type = "upper", tl.cex = 0.8)

# Visualize scatter plots
# Scatter plot of temperature anomalies vs soil moisture anomalies
ggplot(fully_merged_summer, aes(x = temp_z_scores, y = sm_z_scores)) +
  geom_point() +
  labs(x = "Summer temperature Anomalies", y = "Summer Soil Moisture Anomalies") +
  ggtitle("Summer Temperature Anomalies vs Soil Moisture Anomalies")

# Scatter plot of temperature anomalies vs GPP
ggplot(fully_merged_summer, aes(x = temp_z_scores, y = mod_gpp)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, colour = "red", se = FALSE) +
  labs(x = "Temperature Anomalies", y = "GPP") +
  ggtitle("Summer Temperature Anomalies vs GPP")

ggplot(fully_merged_summer, aes(x = sm_z_scores, y = mod_gpp)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, colour = "red", se = FALSE) +
  labs(x = "SM Anomalies", y = "GPP") +
  ggtitle("Summer SM Anomalies vs GPP")

## Regression analysis

# Multiple linear regression

mod_gpp <- fully_merged_summer$mod_gpp
temp_z <- fully_merged_summer$temp_z_scores
sm_z <- fully_merged_summer$sm_z_scores

model_both <- lm(mod_gpp ~ temp_z_scores + sm_z_scores, data = fully_merged_summer)
summary(model_both)
plot(model_both)

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


# Mediation test (for indirect effect of t on sm) ???


#### Anomalie z-score timeseries plots
temp_drought_years <- anomalies_combined%>%
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


