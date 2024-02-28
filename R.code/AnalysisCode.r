### Graphing processed results ###
####### by Tegan Williams ########
######### February 2024 ##########

# Set working directory in which the CARDAMOM code base can be found
setwd("/exports/csce/datastore/geos/groups/gcel/for_Tegan/CARDAMOM")


## Load needed libraries and internal functions
source("./R_functions/load_all_cardamom_functions.r")

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

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
