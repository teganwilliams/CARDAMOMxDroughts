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

# Explore data
head(met_data) # Gives first few variables (6 first rows) e.g. to check data imported OK, get familiar with the variables 
summary(met_data) # Gives an overall summary of our dataset 
summary(met_data$doy) # Gives length of the "day of year" column and variable type 
str(met_data) # Compactly displays the structure of the dataset (type of variable e.g., character, logistic, numeric etc) 
glimpse(met_data) # Similar to str() but provides all columns
  
# Delete 2021 data from Met data set since only NA values (-9999?) -> ask david about this
rows_to_delete <- c(3, 7, 10)
my_data <- my_data[-rows_to_delete, ]
  
# Create new column for full dates 
rows_per_year <- 52
met_data$year <- rep(2000:2020, each = rows_per_year)
met_data$full_date <- as.Date(paste0(met_data$year, "-", met_data$doy), format = "%Y-%j")

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
  labs(title="Temperature trends 2000-2020") + 
  xlab("Time [year]") + 
  ylab("Precipitation [kg/m^2/s]")

# Now the same but at different timescales
ggplot(met2010to2020, aes(x = full_date, y = precip_kgm2s)) +
  geom_line(colour="blue2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="Temperature trends 2010-2020") + 
  xlab("Time [year]") + 
  ylab("Precipitation [kg/m^2/s]")

ggplot(met2018to2020, aes(x = full_date, y = precip_kgm2s)) +
  geom_line(colour="blue2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="Temperature trends 2018-2020") + 
  xlab("Time [year]") + 
  ylab("Precipitation [kg/m^2/s]")

ggplot(met2019, aes(x = full_date, y = precip_kgm2s)) +
  geom_line(colour="blue2") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title=element_text(size=13, hjust=0.5)) + # Title size and position
  labs(title="Temperature trends 2019") + 
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
dev.off()
# + facet_wrap(~ year, ncol = 2)

ggplot(met2019, aes(x = doy, y = maxt_C)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Day of Year", y = "Max Temperature",
       title = "Max Temperature Data for 2019",
       colour = "Year") +
  theme_minimal() 








  
  