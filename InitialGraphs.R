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
  wwFLUX_data <- read.csv("~/Desktop/Y4 Dissertation/Data/FLX_DE-Hai_FLUXNET2015_FULLSET_2000-2020_beta-3/weekly_FLX_DE-Hai_FLUXNET2015_FULLSET_WW_2000-2020_beta-3.csv", header=FALSE)

# Explore data
  head(wwFLUX_data) # Gives first few variables (6 first rows here) e.g. to check data imported OK, get familiar with the variables 
  summary(wwFLUX_data) # Gives an overall summary of our dataset 
  summary(wwFLUX_data$V1) # Gives length of the "TIMESTAMP_START" column and variable type 
  str(wwFLUX_data) # Compactly displays the structure of the dataset (type of variable e.g., character, logistic, numeric etc) 
  glimpse(wwFLUX_data) # Similar to str() but provides all columns
  
  # Rename collums


  
# Plot Temperature over time
  (f1 <- ggplot(wwFLUX_data, aes(x=, y=v11)) +
      geom_line(colour="green3") +
      geom_point(colour="green3") +
      theme(legend.position = "bottom") + # Positioning the legend
      labs(title="Temperature trends") + # Add plot title
      theme(plot.title=element_text(size=15, hjust=0.5)) + # Change title size and position
      xlab("Time (year)") + # Change x-axis title (add units)
      ylab("Air Temperature [degrees C]")) # Change y-axis title 
  
  