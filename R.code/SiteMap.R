###########################
##   Site map creation   ##
##      10/03/2024       ##
##     Tegan Williams    ##
###########################

# Libraries

install.packages('ggspatial')
library(ggmap)
library(gridExtra)
library(tidyverse)
library(ggspatial)

# API
ggmap::register_google(key = "AIzaSyAcx1zc4XmT2crJvCMtRch6fGdZCDZOk7g", write = TRUE) #register Google API Key 


# Setting the location
(hainich1 <- map <- get_googlemap("hainich", zoom = 16))


hainich <- c(left = 10.362269, bottom = 51.057892, right = 10.546850, top = 51.110676) 

dev.off()

hainich_map_terrain <- get_map(hainich, maptype='terrain', source="google", zoom=12) 
(terrain_map <- ggmap(hainich_map_terrain) +
    xlab("Longitude") +
    ylab("Latitude"))

tower <- data.frame(lat = 51.079267,
           lon = 10.452000)

hainich_map_satellite <- get_map(hainich, maptype='satellite', source="google", zoom=12) 
(satellite_map <- ggmap(hainich_map_satellite) +
    xlab("Longitude") +
    ylab("Latitude") +
    geom_point(data = tower, aes(lon, lat, colour = 'red')) +
    annotation_north_arrow(location = "tr", which_north = "true"))





hainich_map_hybrid <- get_map(hainich, maptype='hybrid', source="google", zoom=12)
(hybrid_map <- ggmap(hainich_map_hybrid) +
    xlab("Longitude") +
    ylab("Latitude"))


