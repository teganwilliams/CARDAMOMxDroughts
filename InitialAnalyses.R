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

# Create dataframe for climate averages 



#### Analysis of temperature extremes vs flux data ####

