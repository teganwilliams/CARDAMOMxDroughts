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
sm_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_metSM.csv", header= TRUE)

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
  scale_y_continuous(limits = c(5, 23)) +
  xlab("Time [year]") + 
  ylab("Air Temperature [°C]")

# Now filter for even less years
met_data$year <- as.numeric(met_data$year)
met2015to2020 <- met_data[met_data$year >= 2015 & met_data$year <= 2020, ]
met2018to2020 <- met_data[met_data$year >= 2018 & met_data$year <= 2020, ]
met2019 <- met_data[met_data$year >= 2019 & met_data$year <= 2019, ]
met2013 <- met_data[met_data$year >= 2013 & met_data$year <= 2013, ]

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

highlight_years <- c(2003, 2006, 2010, 2015, 2018, 2019, 2020)
filtered_data <- avg_monthly_temp %>%
  filter(year %in% highlight_years)

avg_monthly_temp <- met_data %>%
  group_by(year, month) %>%
  summarise(monthlyT = mean(airt_C, na.rm = TRUE))

met_data_with_avg_temp <- left_join(met_data, avg_monthly_temp, by = c("year", "month"))

ggplot(met_data_with_avg_temp, aes(x = month, y = monthlyT, colour = as.factor(year), group = year)) +
  geom_line(size = 0.7, 
            #aes(group = interaction(highlight_years, year, lex.order = TRUE))
            ) +
  geom_smooth(method = 'loess', aes(group=1), colour = "black", size = 0.7) +
  scale_color_manual(values = c("grey", "pink","grey", "#FAEF17","grey","grey", "#FFD700","grey","grey","grey","#FFA500" ,"grey","grey","grey","grey","#FF8C00","grey","grey","#D9534F","#FF4640","#FF001A")) +
  labs(x = "Month", y = "Air Temperature (°C)", title = "Air temperature over a year with drought years highlighted", colour = "Year") +
  scale_x_discrete(limits = c("May", "Jun", "Jul", "Aug", "Sep")) +
  scale_y_continuous(limits = c(9, 22)) +
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"))

# create table with average value calculated for all the same doy across 20 years
mean_month_temp <- avg_monthly_temp %>%
  group_by(month) %>%
  summarise(monthlyT = mean(monthlyT, na.rm = TRUE))
filtered_data2 <- rbind(filtered_data, mean_month_temp)
filtered_data2$year <- as.character(filtered_data2$year)
filtered_data2 <- filtered_data2 %>%
  mutate(year = replace_na(year, '2020'))

# ok now use these mean values alongside the highlighted years

ggplot(data = filtered_data2, aes(x = month, y = monthlyT, group = year, colour = as.factor(year))) +
  geom_line() +
  scale_color_manual(values = c("#FAEF17", "#FFD700", "#FFA500", "#FF8C00", "#D9534F","red", "black")) +
  labs(x = "Month", y = "Air Temperature (°C)",
       title = "Air temperature over a year with drought years highlighted", colour = "Year") +
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"))

# let's do the same thing for precipitation trends 

highlight_years <- c(2003, 2006, 2010, 2015, 2018, 2019, 2020)

avg_monthly_precip <- met_data %>%
  group_by(year, month) %>%
  summarise(monthlyPrecip = mean(precip_kgm2s, na.rm = TRUE))

precip_filtered_data <- avg_monthly_precip %>%
  filter(year %in% highlight_years)

met_data_with_avg_precip <- left_join(met_data, avg_monthly_precip, by = c("year", "month"))

ggplot(met_data_with_avg_precip, aes(x = month, y = monthlyPrecip, colour = as.factor(year), group = year)) +
  geom_line(size = 0.7) +
  geom_smooth(method = 'loess', aes(group=1), colour = "black", size = 0.7) +
  scale_color_manual(values = c("grey", "pink","grey", "#84E1EB","grey","grey", "#40BDEB","grey","grey","grey","#3E91D1" ,"grey","grey","grey","grey","#1979FF","grey","grey","#2B34E3","#6853E0","darkblue")) +
  labs(x = "Month", y = "Precipitation (kg/m^2/s)", title = "Precipitation during drought years compared to average", colour = "Year") +
  scale_x_discrete(limits = c("May", "Jun", "Jul", "Aug", "Sep")) +
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"))

mean_month_precip <- avg_monthly_precip %>%
  group_by(month) %>%
  summarise(monthlyPrecip = mean(monthlyPrecip, na.rm = TRUE)) 

precip_filtered_data2 <- rbind(precip_filtered_data, mean_month_precip)
precip_filtered_data2$year <- as.character(precip_filtered_data2$year)
precip_filtered_data2 <- precip_filtered_data2 %>%
  mutate(year = replace_na(year, '2000'))

ggplot(data = precip_filtered_data2, aes(x = month, y = monthlyPrecip, group = year, colour = as.factor(year))) +
  geom_line(size = 0.7) +
  scale_color_manual(values = c("black", "#84E1EB", "#40BDEB", "#3E91D1","#1979FF", "#2B34E3","#6853E0","darkblue")) +
  labs(x = "Month", y = "Precipitation (kg/m^2/s)",
       title = "Precipitation during drought years compared to average", colour = "Year") +
  scale_x_discrete(limits = c("May", "Jun", "Jul", "Aug", "Sep")) +
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"))


# interesting comparison: 2003 vs 2018 droughts

met2003vs2018 <- c(2003, 2018)
precipdata2003vs2018 <- avg_monthly_precip %>%
  filter(year %in% met2003vs2018) 

tempdata2003vs2018 <- avg_monthly_temp %>%
  filter(year %in% met2003vs2018)

data2003vs2018 <- cbind(precipdata2003vs2018, tempdata2003vs2018$monthlyT)
data2003vs2018 <- data2003vs2018 %>%
  rename(monthlyT = ...4)

ggplot(data = data2003vs2018, aes(x = month, y = monthlyPrecip, group = year, colour = as.factor(year))) +
  geom_line(size = 0.7) +
  scale_color_manual(values = c("#1979FF", "darkblue")) +
  labs(x = "Month", y = "Precipitation (kg/m^2/s)",
       title = "Precipitation: 2003 vs 2018", colour = "Year") +
  scale_x_discrete(limits = c("May", "Jun", "Jul", "Aug", "Sep")) +
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data = data2003vs2018, aes(x = month, y = monthlyT, group = year, colour = as.factor(year))) +
  geom_line(size = 0.7) +
  scale_color_manual(values = c("orange", "red3")) +
  labs(x = "Month", y = "Air Temperature (°C)",
       title = "Air Temperature: 2003 vs 2018", colour = "Year") +
  scale_x_discrete(limits = c("May", "Jun", "Jul", "Aug", "Sep")) +
  scale_y_continuous(limits = c(10,22)) +
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"))

# now what about comparing their GPP values?

obs_data$full_date <- as.Date(met_data$full_date)
obs_data$month <- month(obs_data$full_date, label = TRUE)

obs_data$month <- as.character(obs_data$month)

avg_monthly_GPP <- obs_data %>%
  group_by(year, month) %>%
  summarise(monthlyGPP = mean(GPP_gCm2day, na.rm = TRUE))

flux2003vs2018 <- avg_monthly_GPP %>%
  filter(year %in% met2003vs2018)

doyflux2003vs2018 <- obs_data %>%
  filter(year %in% met2003vs2018)

ggplot(flux2003vs2018, aes(x = month, y = monthlyGPP, colour = as.factor(year))) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE, aes(group = year)) +
  scale_colour_manual(values = c("green3", "blue"), name = "Year") + 
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="GPP 2003 vs 2018") + 
  xlab("Month") + 
  ylab("GPP [gC/m^2/day]")

ggplot(doyflux2003vs2018, aes(x = doy, y = GPP_gCm2day, colour = as.factor(year))) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE, aes(group = year)) +
  scale_colour_manual(values = c("green3", "blue"), name = "Year") + 
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="GPP 2003 vs 2018") + 
  xlab("DOY") + 
  ylab("GPP [gC/m^2/day]")

# plot consecutive years of GPP data around 2003 and 2018

obs2002to2005 <- obs_data[obs_data$year >= 2002 & obs_data$year <= 2005, ]
obs2017to2020 <- obs_data[obs_data$year >= 2017 & obs_data$year <= 2020, ]
merged <- rbind(obs2002to2005, obs2017to2020)
merged$yeargroup <- ifelse(rownames(merged) %in% rownames(obs2002to2005), "2002to2005", "2017to2020")


ggplot(obs2002to2005, aes(x = full_date, y = GPP_gCm2day)) +
  geom_line(colour="green3") +
  # geom_smooth(method = lm, colour = "darkgreen") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="GPP trends 2002-2005") + 
  xlab("Time [year]") + 
  ylab("GPP [gC/m^2/day]")

ggplot(obs2017to2020, aes(x = full_date, y = GPP_gCm2day)) +
  geom_line(colour="green3") +
  # geom_smooth(method = lm, colour = "darkgreen") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="GPP trends 2017-2020") + 
  xlab("Time [year]") + 
  ylab("GPP [gC/m^2/day]")


# Overlaying these two trendlines

ggplot(merged, aes(x = doy, y = GPP_gCm2day, group = yeargroup)) +
  geom_line() +
  scale_color_manual(values = c("pink","grey")) +
  # geom_smooth(method = lm, colour = "darkgreen") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="GPP trends 2017-2020") + 
  xlab("Time [year]") + 
  ylab("GPP [gC/m^2/day]")

ggplot(merged, aes(x = month, y = GPP_gCm2day, group = yeargroup)) +
  geom_line(size = 0.7) +
  geom_smooth(method = 'loess', aes(group=1), colour = "black", size = 0.7) +
  scale_color_manual(values = c("pink","grey")) +
  labs(x = "Month", y = "Precipitation (kg/m^2/s)", title = "Precipitation during drought years compared to average", colour = "Year") +
  scale_x_discrete(limits = c("May", "Jun", "Jul", "Aug", "Sep")) +
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"))





# Creating the climate averages dataframe
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

ggplot(climate_data_long, aes(x = month, y = temperature, colour = temperature_type)) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE) +
  scale_color_manual(values = c("orange", "red3")) +
  labs(x = "Month", y = "Temperature (°C)",
       title = "Air Temperature vs Max Temperature averages", colour = "Year") +
  # scale_x_discrete(limits = c("May", "Jun", "Jul", "Aug", "Sep")) +
  # scale_y_continuous(limits = c(10,22)) +
  theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black"))


### Plotting Soil Moisture at different Depths over Time  

rows_per_year <- 52
sm_data$year <- rep(2000:2020, each = rows_per_year)
sm_data$full_date <- as.Date(paste0(sm_data$year, "-", sm_data$doy), format = "%Y-%j")
sm_data <- sm_data %>%
  mutate(month = month(full_date, label = TRUE))

ggplot(sm_data, aes(x = full_date, y = SWC_1)) +
  geom_line(colour="#1D32ED") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="Soil Moisture Depth 1 Trends 2000-2020") + 
  xlab("Time [year]") + 
  ylab("Soil Water Content [Pa]")

ggplot(sm_data, aes(x = full_date, y = SWC_2)) +
  geom_line(colour="#0D66FF") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="Soil Moisture Depth 2 Trends 2000-2020") + 
  xlab("Time [year]") + 
  ylab("Soil Water Content [Pa]") 

ggplot(sm_data, aes(x = full_date, y = SWC_3)) +
  geom_line(colour="#6899FC") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="Soil Moisture Depth 3 Trends 2000-2020") + 
  xlab("Time [year]") + 
  ylab("Soil Water Content [Pa]")

SM_data_long <- gather(sm_data, key = "SMdepth", value = "SWC", -mint_C, -maxt_C, -airt_C, -co2_ppm, -swrad_MJm2day, -vpd_kPa, -precip_kgm2s, -wind_spd_ms, -SWC_1_unc, -SWC_2_unc, -SWC_3_unc, -month)


# Plotting SM anomalies

# Reference period 2000-2020
sm_reference_period <- sm_data %>%
  filter(year >= 1989 & year <= 2020) %>%
  group_by(doy) %>%
  summarize(smAverage = mean(SWC_1, na.rm = TRUE))

# Merge with the main data
sm_merged_data <- merge(sm_data, sm_reference_period, by = "doy", all.x = TRUE)

# Calculate temperature anomalies
sm_merged_data$smAnomaly <- sm_merged_data$SWC_1 - sm_merged_data$smAverage

# Calculate the 90th percentile of temperature anomalies
sm_percentile_90 <- quantile(sm_merged_data$smAnomaly, 0.9, na.rm = TRUE)
sm_percentile_0 <- quantile(sm_merged_data$smAnomaly, 0, na.rm = TRUE)


# Filter only values in the upper 90th percentile
sm_anomaly_data <- sm_merged_data %>%
  filter(smAnomaly > sm_percentile_0)

sm_anomaly_data90 <- sm_merged_data %>%
  filter(smAnomaly > sm_percentile_90)

min(sm_anomaly_data90$smAnomaly)

sm_anomaly_data$year <- as.factor(sm_anomaly_data$year)
sm_anomaly_data$doy <- as.factor(sm_anomaly_data$doy)

sm_anomaly_data_filtered <- sm_anomaly_data[c("year", "doy", "smAverage", "smAnomaly")]
sm_anomaly_data_filtered$year <- as.factor(sm_anomaly_data_filtered$year)
sm_anomaly_data_filtered$doy <- as.numeric(sm_anomaly_data_filtered$doy)

# Plotting SM anomalies over time

ggplot(sm_anomaly_data_filtered, aes(x = doy, y = smAnomaly, colour = year)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_text(aes(x = 242, y = 1, label = "Norm"), colour = "black") + 
  labs(title = "Summer SM Anomalies compared to 20 year average",
       x = "",
       y = "Weekly SM Anomaly (mm)") +
  scale_colour_manual(values = c("grey","grey","grey","blue","grey", "grey","green","grey","grey","grey","cyan" ,"grey","grey","grey","grey", "darkblue","grey","grey", "#337AFF", "#0098C2", "#4FD6FF")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) +
  scale_x_continuous(breaks = c(154, 182, 213, 242), 
                     labels = c("Jun", "Jul", "Aug", "Sep"),
                     expand = c(0, 0),
                     limits = c(152, 245)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-15,15))

ggplot(sm_anomaly_data_filtered, aes(x = doy, y = smAnomaly, colour = year)) +
  # geom_rect(aes(xmin = 119, xmax = 245, 
      #          ymin = -15, ymax = -6.98), 
        #    fill = "orange", alpha = 0.5) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_hline(yintercept = -6.98, linetype = "dashed", colour = "darkorange") +
  geom_text(aes(x = 242, y = 1, label = "Norm"), colour = "black") + 
  geom_text(aes(x = 235, y = -11, label = "90th Percentile"), colour = "darkorange") + 
  labs(title = "Summer Deep SM Anomalies compared to 20 year average",
       x = "",
       y = "Weekly SM Anomaly (mm)") +
  scale_colour_manual(values = c("grey","grey","grey","deeppink","grey", "grey","blue","grey","grey","grey","cyan3" ,"grey","grey","grey","grey", "darkblue","grey","grey","red2", "#337AFF", "#4FD6FF")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) +
  scale_x_continuous(breaks = c(121, 154, 182, 213, 242), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"),
                     expand = c(0, 0),
                     limits = c(119, 245)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-15,15))
  

  
