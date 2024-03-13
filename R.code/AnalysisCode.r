### Graphing processed results ###
####### by Tegan Williams ########
######### February 2024 ##########

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Datasets ####

obs <- read.csv("Data/DE-Hai-2000-2020-weekly_timeseries_obs.csv", header = TRUE)
met <- read.csv("Data/DE-Hai-2000-2020-weekly_timeseries_metSM.csv", header = TRUE)
climate <- read_csv("Data/DE-Hai_FLUXNET2015_DD_1989-2020_met.csv")



names(drivers)
drivers$obs
drivers$met
view(drivers$met)
view(obs)
view(drivers$obs)
 
# Data wrangling

colnames(drivers$met) <- c("day", "x", "y", "z", ...) 
drivers$obs$day <- seq_len(nrow(drivers$obs))

merged_data <- merge(drivers$met[, c("day", "temperature")], 
                     drivers$obs[, c("day", "desired_column1", "desired_column2", ...)], 
                     by = "day")

view(states_all$gpp_gCm2day)



# calculate annual values of GPP to quantify the differences between years
obs <- subset(obs, GPP_gCm2day != -9999)
annual_gpp <- aggregate(GPP_gCm2day ~ year, data = obs, FUN = sum)
annual_GPP <- data.frame(year = annual_gpp$year, annual_gpp = annual_gpp$GPP_gCm2day)
print(annual_GPP)

obs$GPP_gCm2day[obs$GPP_gCm2day== -9999] <- NA

yearly_std_dev <- tapply(obs$GPP_gCm2day, obs$year, sd, na.rm = TRUE)
print(yearly_std_dev)

# overall standard deviation
gpp_std_dev <- sd(obs$GPP_gCm2day, na.rm = TRUE)
print(gpp_std_dev)

annual_GPP <- data.frame(
  year = unique(obs$year),
  annual_gpp = tapply(obs$GPP_gCm2day, obs$year, sum, na.rm = TRUE),
  std_dev = yearly_std_dev)
print(annual_GPP)

### Create datasets for first drought: 2000-2005

#### Met Data ####
# delete 2021 data from Met data set since only NA values (-9999)
rows_to_delete <- c(1093:1144)
met <- met[-rows_to_delete, ]

# create new column for year, months and full dates 
rows_per_year <- 52
met$year <- rep(2000:2020, each = rows_per_year)
met$full_date <- as.Date(paste0(met$year, "-", met$doy), format = "%Y-%j")
met <- met %>%
  mutate(month = month(full_date, label = TRUE))

# filter for 2000-2005
met$year <- as.numeric(met$year)
met2000to2005 <- met[met$year >= 2000 & met$year <= 2005, ]
met2015to2020 <- met[met$year >= 2015 & met$year <= 2020, ]

# since flux obs data is weekly should i also change the met data to weekly? 


#### Temperature Anomalies ####

# reference period 1989-2020
climate_data <- climate_data %>%
  mutate(TIMESTAMP = as.character(TIMESTAMP),
         year = year(as.Date(TIMESTAMP, format = "%Y%m%d")),
         month = month(as.Date(TIMESTAMP, format = "%Y%m%d")),
         doy = yday(as.Date(TIMESTAMP, format = "%Y%m%d")))

# weekly temperature anomalies 
weekly_average_temp_data <- climate_data %>%
  group_by(week = ceiling(as.numeric(doy)/7)) %>%
  summarise(AverageWeeklyMeanT = mean(MeanT, na.rm = TRUE))

weekly_temp_data <- climate_data %>%
  group_by(year, week = ceiling(as.numeric(doy)/7)) %>%
  summarise(WeeklyMeanT = mean(MeanT, na.rm = TRUE))

# anomaly values
weekly_temp_merged_data <- merge(weekly_average_temp_data, weekly_temp_data, by = "week", all.x = TRUE)
weekly_temp_merged_data$tempAnomaly <- weekly_temp_merged_data$WeeklyMeanT - weekly_temp_merged_data$AverageWeeklyMeanT
weekly_temp_percentile_0 <- quantile(weekly_temp_merged_data$tempAnomaly, 0, na.rm = TRUE)
temp_anomalies <- weekly_temp_merged_data %>%
  filter(tempAnomaly > weekly_temp_percentile_0)

temp_anomalies$year <- as.factor(temp_anomalies$year)
temp_anomalies$doy <- as.factor(temp_anomalies$doy)
temp_anomalies$year <- as.factor(temp_anomalies$year)
temp_anomalies$week <- as.numeric(temp_anomalies$week)

# creating a new column to group into drought vs non-drought years 
# based on their max value (e.g., >6.5 temp anomaly aka 95th percentile)

temp_anomalies_summer <- temp_anomalies[temp_anomalies$week >= 18 & temp_anomalies$week <= 36, ]
drought_years <- temp_anomalies_summer %>%
  filter(tempAnomaly > 6.5) %>%
  pull(year) %>%
  unique()

temp_anomalies_summer['Drought_Status'] = '1989-2020'
temp_anomalies_summer$Drought_Status[temp_anomalies_summer$year %in% drought_years] <- as.character(temp_anomalies_summer$year[temp_anomalies_summer$year %in% drought_years])
temp_anomalies_summer$Drought_Status <- as.factor(temp_anomalies_summer$Drought_Status)


#### Deep SM Anomalies ####

# Reference period 2000-2020
sm_reference <- met %>%
  filter(year >= 2000 & year <= 2020) %>%
  group_by(doy) %>%
  summarize(smAverage = mean(SWC_1, na.rm = TRUE))

# Merge with the main data
sm_merged_data <- merge(met, sm_reference, by = "doy", all.x = TRUE)

# Calculate soil moisture anomalies
sm_merged_data$smAnomaly <- sm_merged_data$SWC_1 - sm_merged_data$smAverage

# Calculate the 90th percentile of sm anomalies
sm_percentile_95 <- quantile(sm_merged_data$smAnomaly, 0.95, na.rm = TRUE)
sm_percentile_90 <- quantile(sm_merged_data$smAnomaly, 0.9, na.rm = TRUE)
sm_percentile_0 <- quantile(sm_merged_data$smAnomaly, 0, na.rm = TRUE)
sm_percentile_10 <- quantile(sm_merged_data$smAnomaly, 0.1, na.rm = TRUE)

sm_anomalies <- sm_merged_data %>%
  filter(smAnomaly > sm_percentile_0)

sm_anomalies$year <- as.factor(sm_anomalies$year)
sm_anomalies$doy <- as.factor(sm_anomalies$doy)

sm_anomalies_filtered <- sm_anomalies[c("year", "doy", "smAverage", "smAnomaly")]
sm_anomalies_filtered$year <- as.factor(sm_anomalies_filtered$year)
sm_anomalies_filtered$doy <- as.numeric(sm_anomalies_filtered$doy)

# Create new column 'drought_status' and initialise it with 'non-drought'
sm_anomalies_summer <- sm_anomalies_filtered %>%
  filter(doy %in% c(18:36))

sm_drought_years <- sm_anomalies_summer%>%
  filter(year %in% c(2003,2010,2018)) %>%
  pull(year) %>%
  unique()

sm_anomalies_summer['Drought_Status'] = '2000-2020'

sm_anomalies_summer <- sm_anomalies_summer %>%
  mutate(Drought_Status = ifelse(year %in% sm_drought_years, as.character(year), Drought_Status))

sm_anomalies_summer$Drought_Status <- as.factor(sm_anomalies_summer$Drought_Status)
sm_anomalies_summer$year <- as.factor(sm_anomalies_summer$year)
sm_anomalies_summer$doy <- as.numeric(sm_anomalies_summer$doy)
sm_anomalies_summer$smAnomaly <- as.numeric(sm_anomalies_summer$smAnomaly)

sm_anomaly_data_filtered10 <- sm_anomalies_summer %>%
  filter(smAnomaly > sm_percentile_10)

deficit_data <- sm_anomalies_summer %>%
  filter(smAnomaly < 0)
drought_threshold <- quantile(deficit_data$smAnomaly, 0.05)
minimum_deficit <- deficit_data %>%
  mutate(Drought_Status = ifelse(smAnomaly < drought_threshold, "Drought", "No Drought"))

drought_threshold95 <- minimum_deficit %>%
  filter(Drought_Status == "Drought")


#### Flux Observations ####

# assimilated fluxes should be within 'drivers' -> names(drivers) 


flux_data2003 <- 

flux_data2018 <- 


# datasets with obs and mod GPP in SEPARATE columns (called obs and mod) 
gpp_data2003 <- 
  
gpp_data2018 <- 

#### Merge Anomalies with Flux Observation data ####

data2000_2005 <- xxx

#### RQ1: GPP (obs vs modelled) ####
#### a) plot over time (6 years) with uncertainties 
#### b) linear models of obs vs modelled 

# Plotting a) for 2000-2005 (fully assimilated)

palette_GPP <- c('blue','pink')

gpp_drought2003 <- ggplot(data2000_2005, aes(x = day, y = GPP, group = type)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Time (year)",
       y = "GPP (gCm2day)",
       colour = "Type:") +
  scale_colour_manual(values = palette_GPP) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(1, 366, 731, 1096, 1461), 
                     labels = c("2000", "2001", "2002", "2003", "2004", "2005")) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,20))


# b) analysis
#### linear regressions


linear2003 <- lm(obs ~ mod, data = gpp_data2003)

linear2018 <- lm(obs ~ mod, data = gpp_data2018)

summary(linear2003)
summary(linear2018)

# visualisation of this fit:

ggplot(gpp_data2003, aes(x = mod, y = obs)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Modeled GPP", y = "Observed GPP", title = "Observed vs. Modeled GPP")


# what about uncertainties???


#### RQ2: Fluxes against T and SM anomalies  ####

#### RQ3: Simulated drought response -> accuracy #####
  

# Previous code ####

# Met drivers against GPP

drivers$met
drivers$obs

# plotting ecosystem respiration against GPP
plot(states_all$reco_gCm2day, states_all$gpp_gCm2day, col = 'grey', 
     xlab = 'GPP (gCm2day)', ylab = 'Reco (gCm2day)', frame = FALSE)

a <- states_all$reco_gCm2day
b <- states_all$gpp_gCm2day
lines(lowess(a,b), col='darkgreen', lwd = 2)

# plotting npp  against autotrophic respiration
plot(states_all$npp_gCm2day, states_all$reco_gCm2day, col = 'grey', 
     xlab = 'NPP (gCm2day)', ylab = 'Rauto (gCm2day)', frame = FALSE)
x <- states_all$npp_gCm2day
y <- states_all$reco_gCm2day
lines(lowess(x,y), col='darkgreen', lwd = 2)

# plotting gpp against variours factors
plot(apply(states_all$gpp_gCm2day,2,median) ~ drivers$met[,6],
     xlab = 'precipitation?', ylab = 'GPP (gC/m2/day)', frame = FALSE) # find where each met variable is in one of the main CARDAMOM files!!! + code on plotting these

# GPP over time 
plot(apply(states_all$gpp_gCm2day,2,median), type='l',
     xlab = 'Time (weeks)', ylab = 'GPP (gC/m2/day)', frame = FALSE)
points(drivers$obs[,5])


plot(apply(states_all$lai_m2m2,2,median), type='l', xlab = 'Time?', ylab = 'LAI (m2/m2)', frame = FALSE)

# plotting modelled vs observed NEE where obs are points
plot(apply(states_all$nee_gCm2day,2,median), type='l', xlab = 'Time?', ylab = 'NEE (gCm2/day)', frame = FALSE)
points(drivers$obs[,5])

### GPP over time (doesnt work)

plot(states_all$doy, states_all$gpp_gCm2day, col = 'grey', 
     xlab = 'NPP (gCm2day)', ylab = 'Rauto (gCm2day)', frame = FALSE)

### Plot template for LAI over Time

timestep = 1
# if (PROJECT$model$timestep == "monthly") {timestep = mean(PROJECT$model$timestep_days)}
if (PROJECT$model$timestep == "weekly") {timestep = mean(PROJECT$model$timestep_days)}
time_vector = 1:dim(states_all$gpp_gCm2day)[2]

year_vector = time_vector/(365.25/timestep)
year_vector = year_vector+as.numeric(PROJECT$start_year)

interval = floor(length(year_vector)/10)

var = t(states_all$lai_m2m2)
obs = drivers$obs[,3]
obs_unc <- drivers$obs[,4]
# filter -9999 to NA
filter = which(obs == -9999) ; obs[filter] = NA ; obs_unc[filter] = NA

# par(mfrow=c(1,1), mar=c(5,5,3,1))
plot(obs, pch=16, xaxt="n", ylim=c(0,max(max(obs, na.rm = TRUE), quantile(as.vector(var), prob=c(0.999), na.rm=TRUE))),
     cex=0.8, ylab = "LAI (m2/m2)", xlab = "Time (Year)",
     # main=paste(PROJECT$sites[n]," - ",PROJECT$name, sep="")
)

axis(1, at=time_vector[seq(1,length(time_vector),interval)],
     labels=round(year_vector[seq(1,length(time_vector),interval)], digits=0),tck=-0.02, padj=+0.15, cex.axis=1.9)
axis(1, at=time_vector[seq(1,length(time_vector),interval)],
     labels=round(year_vector[seq(1,length(time_vector),interval)], digits=0),tck=-0.02)

# add the confidence intervals
plotconfidence(var)
# calculate and draw the median values, could be mean instead or other
lines(apply(var[1:(dim(var)[1]-1),],1,median,na.rm=TRUE), pch=1, col="red")
# add the data on top
if (length(which(is.na(obs))) != length(obs) ) {
  points(obs, pch=16, cex=0.8)
  plotCI(obs,gap=0,uiw=obs_unc, col="black", add=TRUE, cex=1,lwd=2,sfrac=0.01,lty=1,pch=16)
}
dev.off()

plot(drivers$met[1:192,6], states_all$gpp_gCm2day[1:192,1])

plot(states_all$gpp_gCm2day[1:192,1], drivers$met[1:192,7], type='h', xlab='GPP (gCm2day)', ylab = 'Max Precipitation (kgH2O/m2/s)')

plot(drivers$met[1:192,6],states_all$gpp_gCm2day[1:192,7],type = 'h', ylab='GPP (gCm2day)', xlab = 'doy')

plot(states_all$gpp_gCm2day[1:192,1], drivers$met[1:192,7], type='h', xlab='GPP (gCm2day)', ylab = 'Max Precipitation (kgH2O/m2/s)')

plot(drivers$met[1:192,1],states_all$gpp_gCm2day[1:192,192],type = 'p', ylab='GPP (gCm2day)', xlab = 'Run Day')

plot(drivers$met[,3], drivers$met[,1], ylab = 'Run day', xlab='Max T')


### Statistics
# Function to determine rmse
rmse <- function(obs, pred) sqrt(mean((obs-pred)^2, na.rm=TRUE))

# rmse for LAI
pred_lai = states_all$lai_m2m2
obs_lai = drivers$obs[,3]
rmse_lai <- sqrt(mean((obs_lai-pred_lai)^2, na.rm=TRUE))
rmse_lai
# = 6391.353

# rmse for NEE
pred_nee = states_all$nee_gCm2day

obs_nee = drivers$obs[,5] 
rmse_nee <- sqrt(mean((obs_nee-pred_nee)^2, na.rm=TRUE))
rmse_nee
# = 2401.354

# rmse for GPP 
