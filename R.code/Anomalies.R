####### Anomalies plotted #######
### Amended/final code script ###
### Tegan Williams March 2024 ###

#### Data loading & wrangling ####

# Libraries (required packages for data wrangling)
library(dplyr)
library(tidyverse)
library(ggplot2) 

# Load Meteorological Data
met_data <- read.csv("Data/DE-Hai-2000-2020-weekly_timeseries_met.csv", header = TRUE)
obs_data <- read.csv("Data/DE-Hai-2000-2020-weekly_timeseries_obs.csv", header = TRUE)
sm_data <- read.csv("Data/DE-Hai-2000-2020-weekly_timeseries_metSM.csv", header= TRUE)

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

#### Plots of raw met data timeseries ####

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









