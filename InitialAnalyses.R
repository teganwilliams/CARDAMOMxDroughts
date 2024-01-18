### Initial Analysis
### 18/01/2024-present
### Tegan Williams

#### Data Wrangling Pre-Analysis ####

# Libraries (allow access to required packages for data wrangling)
library(dplyr)
library(tidyverse)
library(ggplot2) 

# Load FLUXNET Data
met_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_met.csv", header = TRUE)
obs_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_obs.csv", header = TRUE)
sm_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_metSM.csv", header= TRUE)

# Delete 2021 data from Met data set since only NA values (-9999)
rows_to_delete <- c(1093:1144)
met_data <- met_data[-rows_to_delete, ]

# Create new column for year, months and full dates 
rows_per_year <- 52
met_data$year <- rep(2000:2020, each = rows_per_year)
met_data$full_date <- as.Date(paste0(met_data$year, "-", met_data$doy), format = "%Y-%j")
met_data <- met_data %>%
  mutate(month = month(full_date, label = TRUE))

#### Climate trends and anomalies ####

# Creating seasonal climate averages
met_data$full_date <- as.Date(met_data$full_date)
met_data$month <- month(met_data$full_date, label = TRUE)
seasonal_data <- met_data %>%
  group_by(year, season = case_when(
    month %in% c('Dec', 'Jan', 'Feb') ~ 'Winter',
    month %in% c('Mar', 'Apr', 'May') ~ 'Spring',
    month %in% c('Jun', 'Jul', 'Aug') ~ 'Summer',
    month %in% c('Sep', 'Oct', 'Nov') ~ 'Autumn')) %>%
  summarise(avg_temp = mean(airt_C))

ggplot(seasonal_data, aes(x=year, y = avg_temp, colour = season)) +
  geom_line() +
  geom_point() +
  labs(x = 'Year', y ='Average Temperature', title = 'Seasonal Climate Averages')+
  scale_color_manual(values = c('Winter'='blue', 'Spring'='green', 'Summer'='red', 'Autumn'='orange'))+
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Create dataframe for climate averages (doy, month, temp, precip, sm)

climate_averages <- cbind(mean_month_temp, mean_month_precip)
column_index_to_delete <- 3
climate_averages <- climate_averages[,-column_index_to_delete]
avg_monthly_MaxT <- met_data %>%
  group_by(year, month) %>%
  summarise(monthlyMaxT = mean(maxt_C, na.rm = TRUE))
mean_month_MaxT <- avg_monthly_MaxT %>%
  group_by(month) %>%
  summarise(monthlyMaxT = mean(monthlyMaxT, na.rm = TRUE))  
climate_averages <- cbind(climate_averages, mean_month_maxT)
column_index_to_delete2 <- 4
climate_averages <- climate_averages[,-column_index_to_delete2]
climate_data_long <- gather(climate_averages, key = "temperature_type", value = "temperature", -month, -monthlyPrecip)


monthly_norms <- 

# Plot climate norms against anomalies 

T_anomaly_plot <- ggplot() +
  geom_line(data = met_data_with_avg_temp, aes(x = month, y = monthlyT, group = 1), colour = "red", alpha = 0.5) +
  geom_line(data = climate_averages, aes(x = month, y = monthlyT), colour = "black", size = 1) 
              


#### Analysis of temperature extremes vs flux data ####

