### Initial Graphs
### 16/08/2023-present
### Tegan Williams

# Libraries (allow access to required packages for data wrangling)
library(dplyr)
library(tidyverse)
library(ggplot2) 

# Load FLUXNET Data
met_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_met.csv", header = TRUE)
obs_data <- read.csv("~/Desktop/Dissertation/Data/DE-Hai-2000-2020-weekly_timeseries_obs.csv", header = TRUE)
