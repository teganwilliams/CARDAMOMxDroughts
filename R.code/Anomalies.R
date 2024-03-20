####### Anomalies plotted #######
### Amended/final code script ###
### Tegan Williams March 2024 ###

#### Data loading & wrangling ####

# Libraries (required packages for data wrangling)
install.packages('tidyverse')
install.packages('tidyr')
install.packages('RColorBrewer')

library(dplyr)
library(tidyverse)
library(ggplot2) 

# Load Meteorological Data
met_data <- read.csv("Data/DE-Hai-2000-2020-weekly_timeseries_metSM.csv", header = TRUE)
obs_data <- read.csv("Data/DE-Hai-2000-2020-weekly_timeseries_obs.csv", header = TRUE)
sm_data <- read.csv("Data/DE-Hai-2000-2020-weekly_timeseries_metSM.csv", header= TRUE)
climate_data <- read.csv("Data/DE-Hai_FLUXNET2015_DD_1989-2020_met.csv")

# Delete 2021 data from Met data set since only NA values (-9999)
rows_to_delete <- c(1093:1144)
met_data <- met_data[-rows_to_delete, ]

# Create new column for year, months and full dates 
rows_per_year <- 52
met_data$year <- rep(2000:2020, each = rows_per_year)
met_data$full_date <- as.Date(paste0(met_data$year, "-", met_data$doy), format = "%Y-%j")
met_data <- met_data %>%
  mutate(month = month(full_date, label = TRUE))

# Now filter datasets for fewer years
met_data$year <- as.numeric(met_data$year)
met2000to2005 <- met_data[met_data$year >= 2000 & met_data$year <= 2005, ]
met2015to2020 <- met_data[met_data$year >= 2015 & met_data$year <= 2020, ]

met2003 <- met_data[met_data$year >= 2003 & met_data$year <= 2003, ]
met2018 <- met_data[met_data$year >= 2018 & met_data$year <= 2018, ]

#### Plots of raw Meteorological data time series ####

# Air Temperature over Time for 2000 to 2005
ggplot(met2000to2005, aes(x = full_date, y = airt_C)) +
  geom_line(colour="red2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="Temperature trends 2000-2005") + 
  theme(plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  xlab("Time [year]") + 
  ylab("Air Temperature [°C]")

# Max Air Temperature over Time for 2000-2005 --> EXACTLY THE SAME TRENDS
ggplot(met2000to2005, aes(x = full_date, y = maxt_C)) +
  geom_line(colour="red2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="Maximum Temperature trends 2000-2005") + 
  theme(plot.title=element_text(size=13, hjust=0.5)) + 
  xlab("Time [year]") + 
  ylab("Max Temperature [°C]")



# plotting T over time for 5 consecutive years e.g., 2000-2005, to show the difference during drought years 
# plus add the 30-year mean temperature as well!
avg_monthly_temp <- met_data %>%
  group_by(year, month) %>%
  summarise(monthlyT = mean(airt_C, na.rm = TRUE))

avg_monthly_temp$year <- as.numeric(avg_monthly_temp$year)
avg_monthly_temp_05 <- avg_monthly_temp[avg_monthly_temp$year >= 2000 & avg_monthly_temp$year <= 2005, ]
avg_monthly_temp_20 <- avg_monthly_temp[avg_monthly_temp$year >= 2015 & avg_monthly_temp$year <= 2020, ]

met_data_with_avg_temp_05 <- left_join(met2000to2005, avg_monthly_temp_05, by = c("year", "month"))

ggplot(met_data_with_avg_temp_05, aes(x = month, y = monthlyT, colour = as.factor(year), group = year)) +
  geom_line(size = 0.7) +
  geom_smooth(method = 'loess', aes(group=1), colour = "black", size = 0.7) +
  scale_color_manual(values = c("#FAEF17", "#FFD700","#FFA500","red", "#BA8659", "#EDC279")) +
  labs(x = "Month", y = "Air Temperature (°C)", title = "Air temperature over a year with drought years highlighted", colour = "Year") +
  scale_x_discrete(limits = c("May", "Jun", "Jul", "Aug", "Sep")) +
  scale_y_continuous(limits = c(9, 22)) +
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Using weekly values without the mean monthly 
ggplot(met_data_with_avg_temp_05, aes(x = doy, y = airt_C, colour = as.factor(year), group = year)) +
  geom_line(size = 0.7) +
  geom_smooth(method = 'loess', aes(group=1), colour = "black", size = 0.7) +
  scale_color_manual(values = c("#FAEF17", "#FFD700","#FFA500","red", "#BA8659", "#EDC279")) +
  labs(x = "Month", y = "Air Temperature (°C)", title = "Air temperature over a year with drought years highlighted", colour = "Year") +
  scale_x_continuous(breaks = c(121, 154, 182, 213, 242), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"),
                     expand = c(0, 0),
                     limits = c(119, 245)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(5,30)) +
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"))


# Precipitation over Time
ggplot(met2000to2005, aes(x = full_date, y = precip_kgm2s)) +
  geom_line(colour="blue2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="Precipitation trends 2000-2005") + 
  xlab("Time [year]") + 
  ylab("Precipitation [kg/m^2/s]")


#### Temperature Anomalies ####

# Reference period 1989-2020
climate_data <- climate_data %>%
  mutate(TIMESTAMP = as.character(TIMESTAMP),
         year = year(as.Date(TIMESTAMP, format = "%Y%m%d")),
         month = month(as.Date(TIMESTAMP, format = "%Y%m%d")),
         doy = yday(as.Date(TIMESTAMP, format = "%Y%m%d")))

# finding average summer mean max day temperatures

max_temperatures <- aggregate(TA_ERA_DAY ~ year, data = climate_data, FUN = max)
max_temperatures$TA_ERA_DAY <- as.numeric(max_temperatures$TA_ERA_DAY)
mean(max_temperatures$TA_ERA_DAY)

summer_temp_data <- climate_data[climate_data$month %in% c("6", "7", "8"), ]
summer_temp_data <- summer_temp_data %>%
  select("year", "month", "TA_ERA_DAY")
mean_summer_temp <- aggregate(TA_ERA_DAY ~ year, data = summer_temp_data, FUN = mean)

mean(mean_summer_temp$TA_ERA_DAY)



summer_rainfall_data <- climate_data[climate_data$month %in% c("7", "8"), ]
summer_rainfall_data <- summer_rainfall_data %>%
  select("year", "month", "Precip")

# Aggregate the rainfall data by year and calculate the mean rainfall for each year
mean_summer_rainfall <- aggregate(Precip ~ year, data = summer_rainfall_data, FUN = sum)

mean(mean_summer_rainfall$Precip)


# Plotting weekly temperature anomalies over Time

weekly_average_temp_data <- climate_data %>%
  group_by(week = ceiling(as.numeric(doy)/7)) %>%
  summarise(AverageWeeklyMeanT = mean(MeanT, na.rm = TRUE),
            sd = sd(MeanT, na.rm = TRUE))

weekly_temp_data <- climate_data %>%
  group_by(year, week = ceiling(as.numeric(doy)/7)) %>%
  summarise(WeeklyMeanT = mean(MeanT, na.rm = TRUE)) 

  
View(weekly_temp_data)


# find the anomaly values
weekly_temp_merged_data <- merge(weekly_average_temp_data, weekly_temp_data, by = "week", all.x = TRUE)
weekly_temp_merged_data$tempAnomaly <- weekly_temp_merged_data$WeeklyMeanT - weekly_temp_merged_data$AverageWeeklyMeanT
weekly_temp_percentile_90 <- quantile(weekly_temp_merged_data$tempAnomaly, 0.9, na.rm = TRUE)
weekly_temp_percentile_0 <- quantile(weekly_temp_merged_data$tempAnomaly, 0, na.rm = TRUE)
weekly_temp_percentile_95 <- quantile(weekly_temp_merged_data$tempAnomaly, 0.95, na.rm = TRUE)

weekly_temp_anomaly_data <- weekly_temp_merged_data %>%
  filter(tempAnomaly > weekly_temp_percentile_0)
weekly_temp_anomaly_data90 <- weekly_temp_merged_data %>%
  filter(tempAnomaly > weekly_temp_percentile_90)
weekly_temp_anomaly_data95 <- weekly_temp_merged_data %>%
  filter(tempAnomaly > weekly_temp_percentile_95)

min(weekly_temp_anomaly_data95$tempAnomaly)
max(weekly_temp_anomaly_data95$tempAnomaly)

weekly_temp_anomaly_data$year <- as.factor(weekly_temp_anomaly_data$year)
weekly_temp_anomaly_data$week <- as.numeric(weekly_temp_anomaly_data$week)

weekly_temp_merged_data$full_date <- ymd(paste0(weekly_temp_merged_data$year, "-01-07")) + weeks(weekly_temp_merged_data$week - 1)


anomalies1 <- merge(weekly_temp_merged_data, sm_anomaly_data_filtered, by = "full_date")

View(weekly_temp_merged_data)
View(sm_anomaly_data_filtered)

anomalies <- anomalies1 %>%
  select(full_date, week.x, year.x, tempAnomaly, smAnomaly) %>%
  rename(date = full_date, week = week.x, year = year.x) %>%
  filter(year >= 2000 & year <=2005 | year >= 2015 & year <= 2020)
  
anomalies_full <- anomalies1 %>%
  select(full_date, week.x, year.x, tempAnomaly, smAnomaly) %>%
  rename(date = full_date, week = week.x, year = year.x) 

# creating a new column to group into drought vs non-drought years 
# based on their max value (e.g., >6.5 temp anomaly)

# Create new column 'drought_status' and initialise it with 'non-drought'

temp_anomaly_data_summer <- weekly_temp_anomaly_data[weekly_temp_anomaly_data$week >= 18 & weekly_temp_anomaly_data$week <= 36, ]
drought_years <- temp_anomaly_data_summer %>%
  filter(tempAnomaly > 5.2) %>%
  pull(year) %>%
  unique()

temp_drought_years <- temp_anomaly_data_summer%>%
  filter(year %in% c(2003,2010,2018,2019)) %>%
  pull(year) %>%
  unique()

hotyears <- as.data.frame(drought_years)

temp_anomaly_data_summer['Drought_Status'] = '1989-2020'
temp_anomaly_data_summer$Drought_Status[temp_anomaly_data_summer$year %in% temp_drought_years] <- as.character(temp_anomaly_data_summer$year[temp_anomaly_data_summer$year %in% temp_drought_years])
temp_anomaly_data_summer$Drought_Status <- as.factor(temp_anomaly_data_summer$Drought_Status)

min(temp_anomaly_data_summer$tempAnomaly)

palette_anomalies1 <- c("#D6D6D686", "#29B071", "#2C78DB", "#C93402")
palette_anomalies2 <-c("#D6D6D686", "#DB2C95", "#11A0D9", "#8A3FBF")
palette_anomalies3 <- c("#D6D6D686", "#990AFF", "#126DFF", "#FF19AB")
palette_anomalies <- c("#D6D6D686", "#D6A400", "#7362BA", "#B80422", "#3EA85A")

colourblind_palette <- c("#D4D4D4C4","#329FD6", "#C20502", "#9E21C4")

temp_anomaly_plot <- ggplot(temp_anomaly_data_summer, aes(x = week, y = tempAnomaly, colour = Drought_Status, group = year)) +
  geom_line(size = 0.8) +
  geom_hline(yintercept = 0, size = 0.4, colour = "black") +
  geom_hline(yintercept = 4.69, linetype = "dashed", size = 0.7, colour = "#FC6C19BE") +
  # geom_text(aes(x = 35.2, y = -0.4, label = "Norm"), colour = "black") + 
  # geom_text(aes(x = 34, y = 5.2, label = "95th percentile"), colour = "darkorange", size = 3) + 
  labs(title = "",
       x = "Summer months",
       y = "Air temperature anomaly (°C)",
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
                     limits = c(-6,8))

plot(temp_anomaly_plot)

dev.off()

# Save the plot as a PNG file to GitHub
ggsave("temp_anomaly_plot.png", path = "Plots/Anomalies", plot = temp_anomaly_plot, width = 7, height = 5, dpi = 500)


#### Deep Soil Moisture Anomalies ####

# Reference period 2000-2020
sm_reference_period <- met_data %>%
  filter(year >= 2000 & year <= 2020) %>%
  group_by(doy) %>%
  summarize(smAverage = mean(SWC_1, na.rm = TRUE),
            sd = sd(mean(SWC_1, na.rm = TRUE)))

# Merge with the main data
sm_merged_data <- merge(met_data, sm_reference_period, by = "doy", all.x = TRUE)

# Calculate soil moisture anomalies
sm_merged_data$smAnomaly <- sm_merged_data$SWC_1 - sm_merged_data$smAverage
sm_merged_data$weeklysm <- sm_merged_data$SWC_1

# Calculate the 90th percentile of sm anomalies
sm_percentile_95 <- quantile(sm_merged_data$smAnomaly, 0.95, na.rm = TRUE)
sm_percentile_90 <- quantile(sm_merged_data$smAnomaly, 0.9, na.rm = TRUE)
sm_percentile_0 <- quantile(sm_merged_data$smAnomaly, 0, na.rm = TRUE)
sm_percentile_10 <- quantile(sm_merged_data$smAnomaly, 0.1, na.rm = TRUE)

# Filter only values in the upper 90th percentile
sm_anomaly_data <- sm_merged_data %>%
  filter(smAnomaly > sm_percentile_0)

sm_anomaly_data90 <- sm_merged_data %>%
  filter(smAnomaly > sm_percentile_90)

sm_anomaly_data95 <- sm_merged_data %>%
  filter(smAnomaly > sm_percentile_95)

sm_anomaly_data10 <- sm_merged_data %>%
  filter(smAnomaly > sm_percentile_10)

min(sm_anomaly_data95$smAnomaly)

sm_anomaly_data$year <- as.factor(sm_anomaly_data$year)
sm_anomaly_data$doy <- as.factor(sm_anomaly_data$doy)

sm_anomaly_data$week <- ceiling((sm_anomaly_data$doy - 6) / 7)
View(sm_anomaly_data)

sm_anomaly_data_filtered <- sm_anomaly_data[c("year", "week", "full_date", "weeklysm", "sd", "smAverage", "smAnomaly")]
sm_anomaly_data_filtered$year <- as.factor(sm_anomaly_data_filtered$year)
sm_anomaly_data_filtered$doy <- as.numeric(sm_anomaly_data_filtered$doy)



# Create new column 'drought_status' and initialise it with 'non-drought'
sm_anomaly_data_summer <- sm_anomaly_data_filtered %>%
  filter(doy %in% c(18:36))

sm_drought_years <- sm_anomaly_data_summer%>%
  filter(year %in% c(2003,2010,2018,2019)) %>%
  pull(year) %>%
  unique()

sm_anomaly_data_summer['Drought_Status'] = '2000-2020'

sm_anomaly_data_summer <- sm_anomaly_data_summer %>%
  mutate(Drought_Status = ifelse(year %in% sm_drought_years, as.character(year), Drought_Status))

sm_anomaly_data_summer$Drought_Status <- as.factor(sm_anomaly_data_summer$Drought_Status)
sm_anomaly_data_summer$year <- as.factor(sm_anomaly_data_summer$year)
sm_anomaly_data_summer$doy <- as.numeric(sm_anomaly_data_summer$doy)
sm_anomaly_data_summer$smAnomaly <- as.numeric(sm_anomaly_data_summer$smAnomaly)

sm_anomaly_data_filtered10 <- sm_anomaly_data_summer %>%
  filter(smAnomaly > sm_percentile_10)

deficit_data <- sm_anomaly_data_summer %>%
  filter(smAnomaly < 0)
drought_threshold <- quantile(deficit_data$smAnomaly, 0.05)
minimum_deficit <- deficit_data %>%
  mutate(Drought_Status = ifelse(smAnomaly < drought_threshold, "Drought", "No Drought"))

drought_threshold95 <- minimum_deficit %>%
  filter(Drought_Status == "Drought")

max(drought_threshold95$smAnomaly)

# threshold for 80th percentile is -8.06
# threshold for 90th percentile is -9.43
# threshold for 95th percentile is -9.93

palette_before <- c("#D4D4D4C4", "#178A86", "#0F5596", "#B51717")

# Plotting soil moisture anomalies
sm_anomaly_plot <- ggplot(sm_anomaly_data_summer, aes(x = doy, y = smAnomaly, group = year, colour = Drought_Status)) +
  geom_line(size = 0.8) +
  geom_hline(yintercept = 0, size = 0.4, colour = "black") +
  geom_hline(yintercept = -8.06, linetype = "dashed", size = 0.7, colour = "#FC6C19BE") +
  # geom_text(aes(x = 35.1, y = 1, label = "Norm"), colour = "black") + 
  # geom_text(aes(x = 34, y = -7, label = "80th percentile"), colour = "darkorange", size = 3) +  
  labs(title = "",
       x = "Summer months",
       y = "Deep soil moisture anomaly (mm)",
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
                     limits = c(-15,15))

plot(sm_anomaly_plot)

ggsave("sm_anomaly_plot.png", path = "Plots/Anomalies", plot = sm_anomaly_plot, width = 7, height = 5, dpi = 500)


#### Creating normalised anomaly data for comparison of their effects on gpp


View(sm_merged_data)
View(sm_anomaly_data_filtered)


# save data tables for anomalies
write.csv(temp_anomaly_data_summer, "temp_anomaly_data.csv")
write.csv(sm_anomaly_data_summer, "sm_anomaly_data.csv")

write.csv(anomalies, "anomalies.csv")
write.csv(anomalies_full, "anomalies_full.csv")
write.csv(weekly_temp_merged_data, "temp_anomalies.csv")
write.csv(sm_anomaly_data_filtered, "sm_anomalies.csv")

#### Combined plots ####

library(gridExtra)

(combined_anomaly_plot <- grid.arrange(temp_anomaly_plot, sm_anomaly_plot, nrow = 2, layout_matrix = rbind(c(1, 2)), heights = c(1, 1)))

# Display the combined plot
print(combined_anomaly_plot)

# Save plot

ggsave("combined_anomaly_plot.png", path = "Plots/Anomalies", plot = combined_anomaly_plot, width = 10, height = 8)



