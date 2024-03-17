### Analysis
### 18/01/2024-present
### Tegan Williams

#### Data Wrangling ####

# Libraries
library(dplyr)
library(ggplot2)

# Start by openning CARDAMOM processed results file into R environment!

# Determine the total number of timesteps
finish = dim(states_all$gpp_gCm2day)[2]

# Estimate the ensemble of GPPs
mod_gpp_timeseries = apply(states_all$gpp_gCm2day, 2, median) 
mod_gpp_unc = quantile(states_all$gpp_gCm2day, probs =c(0.5, 0.95))
mod_gpp_unc2 = apply(states_all$gpp_gCm2day, 2, quantile, probs = c(0.5, 0.95))
mod_gpp_annual = apply(states_all$gpp_gCm2day, 1, median)
View(mod_gpp_unc2)
print(mod_gpp_timeseries)

# Create Dataframe
mod_gpp_data <- data.frame(mod_gpp = mod_gpp_timeseries)

# very simple plot to view data distribution
plot(mod_gpp_timeseries, type = 'l', col = 'red')


#### For 2015 to 2020 ####

# Import Datasets
obs <- read.csv("/exports/csce/datastore/geos/groups/gcel/for_Tegan/CARDAMOM/DE-Hai/2015-2020/DE-Hai_timeseries_obs.csv", header = TRUE)
met <- read.csv("/exports/csce/datastore/geos/groups/gcel/for_Tegan/CARDAMOM/DE-Hai/2015-2017/DE-Hai_timeseries_met.csv", header = TRUE)

# Filter and rename columns in the obs dataset
filtered_obs <- obs %>%
  select(doy, year, obs_gpp = GPP_gCm2day, obs_gpp_unc = GPP_unc_gCm2day)%>%
  mutate(obs_gpp = replace(obs_gpp, obs_gpp == -9999, NA))%>%
  mutate(day = seq(7, by = 7, length.out = nrow(filtered_obs)))

mod_gpp_data <- mod_gpp_data %>%
  mutate(day = seq(7, by = 7, length.out = nrow(mod_gpp_data)))

# Merge the filtered obs dataset with mod_gpp_data
merged_data <- merge(mod_gpp_data, filtered_obs, by = c("day"))

start_date <- as.Date("2015-01-01")
merged_data$date <- start_date + merged_data$day - 1

ggplot(merged_data, aes(x = date)) +
  geom_line(aes(y = mod_gpp, color = "Model")) +
  geom_point(aes(y = obs_gpp, color = "Observation")) +
  labs(x = "Date", y = "GPP (gC/m^2/day)", color = "Dataset") +
  scale_color_manual(values = c("Model" = "blue", "Observation" = "red")) +
  theme_minimal()

merged_data$day <- as.numeric(merged_data$day)

ggplot(merged_data, aes(x = day)) +
  geom_line(aes(y = mod_gpp, color = "Mod")) +
  geom_point(aes(y = obs_gpp, color = "Obs")) +
  labs(x = "Time (year)", y = "GPP (gC/m^2/day)", colour = "Data:") +
  scale_color_manual(values = c("Mod" = "darkgreen", "Obs" = "orchid")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_x_continuous(breaks = c(7, 371, 735, 1099, 1463, 1827), 
                     labels = c("01/2015", "01/2016", "01/2017", "01/2018", "01/2019", "01/2020"),
                     expand = c(0, 0),
                     limits = c(0,2184) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,20)))
# add uncertainty bounds! 

# Now look at linear model of this
ggplot(merged_data, aes(x = mod_gpp, y = obs_gpp)) +
  geom_point(colour = "grey") +
  labs(x = "Modelled GPP (gC/m^2/day)", y = "Observed GPP (gC/m^2/day)") +
  geom_abline(intercept = 0, slope = 1, color = "black", size = 0.5) +
  geom_abline(intercept = 0, slope = max(merged_data$obs_gpp, na.rm = TRUE) / max(merged_data$mod_gpp, na.rm = TRUE), linetype = "dotted", color = "red") +
  theme_minimal()


#### For 2000 to 2005 ####

# Import Datasets
obs2000 <- read.csv("/exports/csce/datastore/geos/groups/gcel/for_Tegan/CARDAMOM/DE-Hai/2000-2005/DE-Hai_timeseries_obs.csv", header = TRUE)
met2000 <- read.csv("/exports/csce/datastore/geos/groups/gcel/for_Tegan/CARDAMOM/DE-Hai/2000-2005/DE-Hai_timeseries_met.csv", header = TRUE)

# Filter and rename columns in the obs dataset
filtered_obs2000 <- obs2000 %>%
  select(doy, year, obs_gpp = GPP_gCm2day, obs_gpp_unc = GPP_unc_gCm2day)%>%
  mutate(obs_gpp = replace(obs_gpp, obs_gpp == -9999, NA))%>%
  mutate(day = seq(7, by = 7, length.out = nrow(filtered_obs)))

mod_gpp_data2000 <- mod_gpp_data2000 %>%
  mutate(day = seq(7, by = 7, length.out = nrow(mod_gpp_data)))

# Merge the filtered obs dataset with mod_gpp_data
merged_data <- merge(mod_gpp_data, filtered_obs, by = c("day"))

start_date <- as.Date("2000-01-01")
merged_data$date <- start_date + merged_data$day - 1

ggplot(merged_data, aes(x = date)) +
  geom_line(aes(y = mod_gpp, color = "Model")) +
  geom_point(aes(y = obs_gpp, color = "Observation")) +
  labs(x = "Date", y = "GPP (gC/m^2/day)", color = "Dataset") +
  scale_color_manual(values = c("Model" = "blue", "Observation" = "red")) +
  theme_minimal()

merged_data$day <- as.numeric(merged_data$day)

ggplot(merged_data, aes(x = day)) +
  geom_line(aes(y = mod_gpp, color = "Mod")) +
  geom_point(aes(y = obs_gpp, color = "Obs")) +
  labs(x = "Time (year)", y = "GPP (gC/m^2/day)", colour = "Data:") +
  scale_color_manual(values = c("Mod" = "darkgreen", "Obs" = "orchid")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_x_continuous(breaks = c(182, 553, 917, 1281, 1645, 2009), 
                     labels = c("2000", "2001", "2002", "2003", "2004", "2005"),
                     expand = c(0, 0),
                     limits = c(0,2184) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,20)))
# add uncertainty bounds! 

# Linear model (R squared; correlation coefficient)
# 1) Calculate correlation coefficient
correlation2000 <- cor(merged_data2000$mod_gpp, merged_data2000$obs_gpp, use = "complete.obs")

# Square the correlation coefficient to get R^2
r_squared2000 <- correlation2000^2

# Print the R^2 value
print(paste("R^2 value:", round(r_squared2000, 3)))

# PLot this
ggplot(merged_data2000, aes(x = mod_gpp, y = obs_gpp)) +
  geom_point(colour = "orchid") +
  labs(x = "Modelled GPP (gC/m²/day)", y = "Observed GPP (gC/m²/day)") +
  geom_abline(intercept = 0, slope = 1, color = "purple", size = 0.6) +
  geom_abline(intercept = 0, slope = max(merged_data2000$obs_gpp, na.rm = TRUE) / max(merged_data2000$mod_gpp, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 11, 
                y = 6), 
            label = paste("R² =", round(r_squared2000, 3)), 
            hjust = 0, vjust = 1,
            size = 5, 
            fontface = "bold", 
            colour = "purple") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9))



# Parameters overlapping histograms ???
# ask david for the code used to plot the histogram figures?

plot(hist())

#### Parameters ####
parameter_data <- data.frame(x = parameters)
parameter_data <- t(parameter_data)
colnames(parameter_data) <- c("SOM", "GPP fraction auto", "Fraction to Foliage", 
                              "Fraction to Roots", "Leaf Lifespan", "TOR Wood", 
                              "TOR Roots", "Litter Turnover", "SOM Turnover", "Temp factor",
                              "11", "Max bud burst day", "Fraction to Clab", "Clab release period", "Max leaf fall day",
                              "Leaf fall period", "LMA", "C labile", "C foliar", "C roots", "C wood",
                              "C litter", "C SOM", "Initial soil water",
                              "Fraction Cwood coarse root", "26","27", "Resilience factor","29",
                              "30", "31", "32", "Likelihood score")

parameter_data2000 <- data.frame(parameter_data)
parameter_data2015 <- data.frame(parameter_data)

# Plot the first histogram
par(mfrow = c(2, 2))

hist(parameter_data2000$SOM, col = "pink", main = "Histogram of SOM", xlab = "SOM", ylab = "Frequency", breaks = 20, xlim = range(c(parameter_data2000$SOM, parameter_data2015$SOM)))
hist(parameter_data2015$SOM, col = "lightblue", add = TRUE, breaks = 20)
legend("topright", legend = c("SOM 2003", "SOM 2018"), fill = c("pink", "lightblue"))

hist(parameter_data2000$GPP.fraction.auto, col = "pink", main = "Histogram of SOM", xlab = "SOM", ylab = "Frequency", breaks = 20, xlim = range(c(parameter_data2000$GPP.fraction.auto, parameter_data2015$GPP.fraction.auto)))
hist(parameter_data2015$GPP.fraction.auto, col = "lightblue", add = TRUE, breaks = 20)
legend("topright", legend = c("gpp auto 2003", "gpp auto 2018"), fill = c("pink", "lightblue"))

hist(parameter_data2000$Temp.factor, col = "pink", main = "Histogram of SOM", xlab = "SOM", ylab = "Frequency", breaks = 20, xlim = range(c(parameter_data2000$Temp.factor, parameter_data2015$Temp.factor)))
hist(parameter_data2015$Temp.factor, col = "lightblue", add = TRUE, breaks = 20)
legend("topright", legend = c("Temp factor 2003", "Temp factor 2018"), fill = c("pink", "lightblue"))

hist(parameter_data2000$C.labile, col = "pink", main = "Histogram of SOM", xlab = "SOM", ylab = "Frequency", breaks = 20, xlim = range(c(parameter_data2000$C.labile, parameter_data2015$C.labile)))
hist(parameter_data2015$C.labile, col = "lightblue", add = TRUE, breaks = 20)
legend("topright", legend = c("C labile 2003", "C labile 2018"), fill = c("pink", "lightblue"))


plot(hist(parameter_data2015$GPP.fraction.auto))
plot(hist(parameter_data2015$Temp.factor))
plot(hist(parameter_data2015$C.labile))
plot(hist(parameter_data2015$Likelihood.score))

plot(hist(combined_parameters$SOM))
plot(hist(combined_parameters$GPP.fraction.auto))
plot(hist(combined_parameters$Temp.factor))
plot(hist(combined_parameters$C.labile))
plot(hist(combined_parameters$Likelihood.score))


##### RQ2: Anomalies x fluxes ####

# Datasets

obs <- read.csv("/exports/csce/datastore/geos/groups/gcel/for_Tegan/CARDAMOM/DE-Hai/2015-2020/DE-Hai_timeseries_obs.csv", header = TRUE)
met <- read.csv("/exports/csce/datastore/geos/groups/gcel/for_Tegan/CARDAMOM/DE-Hai/2015-2017/DE-Hai_timeseries_met.csv", header = TRUE)
sm <- read.csv("Data/DE-Hai-2000-2020-weekly_timeseries_metSM.csv", header= TRUE)
climate_data <- read_csv("Data/DE-Hai_FLUXNET2015_DD_1989-2020_met.csv")




# EXTRAS ####
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

# Plot climate norms against anomalies 
T_anomaly_plot <- ggplot() +
  geom_line(data = met_data_with_avg_temp, aes(x = month, y = monthlyT, group = 1), colour = "red", alpha = 0.5) +
  geom_line(data = climate_averages, aes(x = month, y = monthlyT), colour = "black", size = 1) 
            

