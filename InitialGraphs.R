### Initial Graphs
### 06/07/2023-present
### Tegan Williams

# Libraries (allow access to required packages for data wrangling)
library(dplyr)
library(tidyverse)
library(ggplot2) 

# Load FLUXNET Data
met_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_met.csv", header = TRUE)
obs_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_obs.csv", header = TRUE)
# or when using a uni desktop:
met_data <- read_csv("DE-Hai-2000-2020-weekly_timeseries_met.csv")
obs_data <- read_csv("DE-Hai-2000-2020-weekly_timeseries_obs.csv")

# Explore data
head(met_data) # Gives first few variables (6 first rows) e.g. to check data imported OK, get familiar with the variables 
summary(met_data) # Gives an overall summary of our dataset 
summary(met_data$doy) # Gives length of the "day of year" column and variable type 
str(met_data) # Compactly displays the structure of the dataset (type of variable e.g., character, logistic, numeric etc) 
glimpse(met_data) # Similar to str() but provides all columns
  
# Delete 2021 data from Met data set since only NA values (-9999)
rows_to_delete <- c(1093:1144)
met_data <- met_data[-rows_to_delete, ]
  
# Create new column for year, months and full dates 
rows_per_year <- 52
met_data$year <- rep(2000:2020, each = rows_per_year)
met_data$full_date <- as.Date(paste0(met_data$year, "-", met_data$doy), format = "%Y-%j")
met_data <- met_data %>%
  mutate(month = month(full_date, label = TRUE))


# Plot Air Temperature over Time
ggplot(met_data, aes(x = full_date, y = airt_C)) +
      geom_line(colour="red2") +
      theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
      labs(title="Temperature trends 2000-2020") + 
      xlab("Time [year]") + 
      ylab("Air Temperature [°C]")

# Filter data for the years 2010 to 2020
met2010to2020 <- met_data[met_data$year >= 2010 & met_data$year <= 2020, ]

ggplot(met2010to2020, aes(x = full_date, y = airt_C)) +
  geom_line(colour="red2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="Temperature trends 2010-2020") + 
  theme(plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  xlab("Time [year]") + 
  ylab("Air Temperature [°C]")

# Now filter for even less years
met_data$year <- as.numeric(met_data$year)
met2015to2020 <- met_data[met_data$year >= 2015 & met_data$year <= 2020, ]
met2018to2020 <- met_data[met_data$year >= 2018 & met_data$year <= 2020, ]
met2019 <- met_data[met_data$year >= 2019 & met_data$year <= 2019, ]

ggplot(met2018to2020, aes(x = full_date, y = airt_C)) +
  geom_line(colour="red2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="Temperature trends 2018-2020") + 
  theme(plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  xlab("Time [year]") + 
  ylab("Air Temperature [°C]")

ggplot(met2019, aes(x = full_date, y = airt_C)) +
  geom_line(colour="red2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="Temperature trends 2019") + 
  theme(plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  xlab("Time [year]") + 
  ylab("Air Temperature [°C]")

# Let's look at max temperature instead since droughts are characterised by these
ggplot(met_data, aes(x = full_date, y = maxt_C)) +
  geom_line(colour="red2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="Maximum Temperature trends 2000-2020") + 
  theme(plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  xlab("Time [year]") + 
  ylab("Max Temperature [°C]")

ggplot(met2010to2020, aes(x = full_date, y = maxt_C)) +
  geom_line(colour="red2") +
  geom_smooth(method = lm, colour = "darkred") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="Maximum Temperature trends 2000-2020") + 
  theme(plot.title=element_text(size=13, hjust=0.5)) + 
  xlab("Time [year]") + 
  ylab("Max Temperature [°C]")

ggplot(met2015to2020, aes(x = full_date, y = maxt_C)) +
  geom_line(colour="red2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(title="Maximum Temperature trends 2015-2020") + 
  xlab("Time [year]") + 
  scale_y_continuous(name = "Max Temperature [°C]", 
                     # limits=c(15, 30)
                     )


# Now let's look at Precipitation over Time
ggplot(met_data, aes(x = full_date, y = precip_kgm2s)) +
  geom_line(colour="blue2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="Precipitation trends 2000-2020") + 
  xlab("Time [year]") + 
  ylab("Precipitation [kg/m^2/s]")

# Now the same but at different timescales
ggplot(met2010to2020, aes(x = full_date, y = precip_kgm2s)) +
  geom_line(colour="blue2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="Precipitation trends 2010-2020") + 
  xlab("Time [year]") + 
  ylab("Precipitation [kg/m^2/s]")

ggplot(met2018to2020, aes(x = full_date, y = precip_kgm2s)) +
  geom_line(colour="blue2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="Precipitation trends 2018-2020") + 
  xlab("Time [year]") + 
  ylab("Precipitation [kg/m^2/s]")

ggplot(met2019, aes(x = full_date, y = precip_kgm2s)) +
  geom_line(colour="blue2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="Precipitation trends 2019") + 
  xlab("Time [year]") + 
  ylab("Precipitation [kg/m^2/s]")


# Now lets look at the observation data
head(obs_data) 
summary(obs_data) 
summary(obs_data$doy) 
str(obs_data)
glimpse(obs_data)

# Create a date column for this table too
rows_per_year <- 52
obs_data$year <- rep(2000:2020, each = rows_per_year)
obs_data$full_date <- as.Date(paste0(obs_data$year, "-", obs_data$doy), format = "%Y-%j")

# Replace -9999 with NA in the GPP column
obs_data$GPP_gCm2day[obs_data$GPP_gCm2day == -9.999000e+03] <- NA
obs_data$NEE_gCm2day[obs_data$NEE_gCm2day == -9.999000e+03] <- NA
obs_data$Reco_gCm2day[obs_data$Reco_gCm2day == -9.999000e+03] <- NA

# Plot GPP over time
ggplot(obs_data, aes(x = full_date, y = GPP_gCm2day)) +
  geom_line(colour="green3") +
  # geom_smooth(method = lm, colour = "darkgreen") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="GPP trends 2000-2020") + 
  xlab("Time [year]") + 
  ylab("GPP [gC/m^2/day]")


# Plot GPP against Temperature
# first combine the datasets
all_data <- cbind(met_data, obs_data$GPP_gCm2day, obs_data$Reco_gCm2day, obs_data$NEE_gCm2day)
View(all_data)

y_limits <- c(0, 16)

ggplot(all_data, aes(x = airt_C, y = obs_data$GPP_gCm2day)) +
  geom_point(colour="green3") +
  geom_smooth(method = lm, colour = "darkgreen") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  coord_cartesian(ylim = y_limits) + 
  labs(title="GPP against Air Temperature") + 
  xlab("Temperature [°C]") + 
  ylab("GPP [gC/m^2/day]")

# GPP against Precipitation
ggplot(all_data, aes(x = precip_kgm2s, y = obs_data$GPP_gCm2day)) +
  geom_point(colour="green3") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="GPP against precipitation") + 
  xlab("Precipitation [kg/m^2/s]") + 
  ylab("GPP [gC/m^2/day]")

# Now Reco against Time, Temp, and Precip
ggplot(obs_data, aes(x = full_date, y = Reco_gCm2day)) +
  geom_line(colour="orange") +
  geom_smooth(method = lm, colour = "red4") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="Reco trends 2000-2020") + 
  xlab("Time [year]") + 
  ylab("Reco [gC/m^2/day]")

ggplot(all_data, aes(x = airt_C, y = obs_data$Reco_gCm2day)) +
  geom_point(colour="orange") +
  geom_smooth(method = lm, colour = "red4") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="Reco against Air Temperature") + 
  xlab("Temperature [°C]") + 
  ylab("Reco [gC/m^2/day]")


ggplot(all_data, aes(x = precip_kgm2s, y = obs_data$Reco_gCm2day)) +
  geom_point(colour="orange") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="Reco against Precipitation") + 
  xlab("Precipitation [kg/m2/s]") + 
  ylab("Reco [gC/m^2/day]")



# Let's try overlapping the years to get a better idea of the variation
met_data$year <- as.factor(met_data$year)
met_data$doy <- as.factor(met_data$doy)

ggplot(met2010to2020, aes(x = doy, y = maxt_C, colour = year)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Day of Year", y = "Max Temperature",
       title = "Overlapping Temperature Data",
       colour = "Year") +
  theme_minimal() 
# + facet_wrap(~ year, ncol = 2)

ggplot(met_data, aes(x = doy, y = maxt_C)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Day of Year", y = "Max Temperature",
       title = "Overlapping Temperature Data") +
  scale_x_continuous(limits = c(0,400), breaks = seq(0,400, by = 50)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(met2019, aes(x = doy, y = maxt_C)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Day of Year", y = "Max Temperature",
       title = "Max Temperature Data for 2019",
       colour = "Year") +
  theme_minimal() 



# Creating seasonal climate averages

met_data$full_date <- as.Date(met_data$full_date)
met_data$month <- month(met_data$full_date, label = TRUE)

seasonal_data <- met_data %>%
  group_by(year, season = case_when(
    month %in% c(12, 1, 2) ~ 'Winter',
    month %in% c(3, 4, 5) ~ 'Spring',
    month %in% c(6, 7, 8) ~ 'Summer',
    month %in% c(9, 10, 11) ~ 'Autumn')) %>%
  summarise(avg_temp = mean(airt_C))

ggplot(seasonal_data, aes(x=, y = avg_temp, colour = season)) +
  geom_line() +
  geom_point() +
  labs(x = 'Year', y ='Average Temperature', title = 'Seasonal Climate Averages')+
  scale_color_manual(values = c('Winter'='blue', 'Spring'='green', 'Summer'='red', 'Autumn'='orange'))

highlight_years <- c(2003, 2006, 2010, 2015, 2018)
filtered_data <- avg_monthly_temp %>%
  filter(year %in% highlight_years)

avg_monthly_temp <- met_data %>%
  group_by(year, month) %>%
  summarise(monthlyT = mean(airt_C, na.rm = TRUE))

met_data_with_avg_temp <- left_join(met_data, avg_monthly_temp, by = c("year", "month"))

ggplot(met_data_with_avg_temp, aes(x = month, y = monthlyT, group = year, color = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c("grey", "grey","grey", "blue","grey","grey", "red","grey","grey","grey", "green","grey","grey","grey","grey", "purple","grey","grey","grey","grey", "orange")) +
  labs(x = "Month", y = "Air Temperature (°C)",
       title = "Air temperature over a year with drought years highlighted",
       color = "Year") +
  theme(legend.position = "right")

# create table with average value calculated for all the same doy across 20 years

mean_month_temp <- avg_monthly_temp %>%
  group_by(month) %>%
  summarise(avgmonthlyT = mean(monthlyT, na.rm = TRUE))

# ok now use these mean values alongside the highlighted years

ggplot(mean_month_temp, aes(x = month, y = avgmonthlyT)) +
  geom_line() +
  labs(x = "Month", y = "Air Temperature (°C)",
       title = "Monthly air temperature averages for 2000-2020 period") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(mean_month_temp, aes(x = month, y = avgmonthlyT, group = year, colour = as.factor(year))) +
  geom_line() +
  labs(x = "Month", y = "Air Temperature (°C)",
       title = "Monthly air temperature averages for 2000-2020 period", colour = "Year") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot() +
  geom_line(data = mean_month_temp, aes(x = month, y = avgmonthlyT)) +
  geom_line(data = filtered_data, aes(x = month, y = monthlyT, group = year, colour = as.factor(year))) +
  labs(x = "Month", y = "Average Air Temperature",
       title = "Monthly Temperature Across Years",
       colour = "Year") +
  scale_color_manual(values = c("grey", "blue", "red", "green", "purple", "orange")) +
  theme_minimal() +
  theme(legend.position = "left", panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  