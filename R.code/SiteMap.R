###########################
##   Site map creation   ##
##      10/03/2024       ##
##     Tegan Williams    ##
###########################

# WD
setwd("Path ~") #sets a new one
getwd() #use this command to check that it's worked

# Libraries
library(ggmap)
library(gridExtra)
library(tidyverse)

# API
ggmap::register_google(key = "AIzaSyAKiJu7sfxgQrb-hkmyj8k4pm-KFxJFNqg", write = TRUE) #register Google API Key 
