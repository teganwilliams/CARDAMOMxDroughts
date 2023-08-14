### Initial Graphs
### 06/07/2023
### Tegan Williams

# First check GitHub connection works
# Yay it does so now lets attempt to upload our FLUXNETdataset

# Libraries (allow access to required packages for data wrangling)
library(dplyr)
library(tidyverse)
library(ggplot2) 

# Load FLUXNET Data
wwFLUX_data <- read.csv("~/Desktop/Dissertation/Data/FLX_DE-Hai_FLUXNET2015_FULLSET_2000-2020_beta-3/weekly_FLX_DE-Hai_FLUXNET2015_FULLSET_WW_2000-2020_beta-3.csv", header=FALSE)

met_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_met.csv", header = TRUE)
obs_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_obs.csv", header = TRUE)

# Explore data
  head(met_data) # Gives first few variables (6 first rows here) e.g. to check data imported OK, get familiar with the variables 
  summary(met_data) # Gives an overall summary of our dataset 
  summary(met_data$doy) # Gives length of the "day of year" column and variable type 
  str(met_data) # Compactly displays the structure of the dataset (type of variable e.g., character, logistic, numeric etc) 
  glimpse(met_data) # Similar to str() but provides all columns
  
# Delete 2021 data since weird values (-9999?) -> ask david about this
rows_to_delete <- c(3, 7, 10)
my_data <- my_data[-rows_to_delete, ]
  
# Create new collumn for full dates 
rows_per_year <- 52
met_data$year <- rep(2000:2020, each = rows_per_year)
met_data$full_date <- as.Date(paste0(met_data$year, "-", met_data$doy), format = "%Y-%j")


# Plot Air Temperature over Time

my_time_series <- ts(met_data$airt_C, frequency = 52) 

dev.off()

ggplot(data = met_data, aes(x = full_date, y = airt_C)) +
  geom_line() +
  labs(x = "Date", y = "Air Temperature [C]", title = "Temperature over time")

ggplot(met_data, aes(x = full_date, y = airt_C)) +
      geom_line(colour="green3") +
      #geom_smooth(colour = "green3") +
      theme(legend.position = "bottom") + # Legend position 
      labs(title="Temperature trends") + 
      theme(plot.title=element_text(size=15, hjust=0.5)) + # Title size and position
      xlab("Time (day of year)") + 
      ylab("Air Temperature [degrees C]")

# Filter data for the years 2010 to 2020
filtered_data <- met_data[met_data$year >= 2010 & met_data$year <= 2020, ]
my_time_series <- ts(filtered_data$airt_C, frequency = 52)

# Plot the time series
ggplot(data = filtered_data, aes(x = full_date, y = airt_C)) +
  geom_line() +
  labs(x = "Date", y = "Value", title = "Time Series Plot (2010-2020)")
  
  