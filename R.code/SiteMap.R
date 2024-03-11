###########################
##   Site map creation   ##
##      10/03/2024       ##
##     Tegan Williams    ##
###########################

# Libraries
library(ggmap)
library(gridExtra)
library(tidyverse)
library(ggspatial)

# Google API registration
ggmap::register_google(key = "AIzaSyAcx1zc4XmT2crJvCMtRch6fGdZCDZOk7g", write = TRUE) 


# Setting the location
(hainich1 <- map <- get_googlemap("hainich", zoom = 16))

hainich <- c(left = 10.238392, bottom = 51.009592, right = 10.617650, top = 51.154815) 

# Create point for the flux tower location 
tower <- data.frame(lon = 10.452000, lat = 51.079267)

hainich_map_satellite <- get_map(hainich, maptype='satellite', source="google", zoom=12) 

(satellite_map <- ggmap(hainich_map_satellite) +
    xlab("Longitude") +
    ylab("Latitude") +
    geom_point(data = tower, shape = 25, colour = "darkorange", aes(lon, lat, size = 1)) +
    annotation_north_arrow(location = "tr", which_north = "true", 
                           style = north_arrow_fancy_orienteering (text_col = 'floralwhite',
                                                                   line_col = 'floralwhite',
                                                                   fill = 'floralwhite')) +
    theme(legend.position = "none",
          aspect.ratio = 1,  
          panel.background = element_rect(fill = "white"),
          axis.title = element_text(size=14),
          axis.text = element_text(size=12)))

satellite_map

ggsave("site_map.png", plot = satellite_map, path = 'Plots', width = 6, height = 4)


# Hybrid style map (adds place names and roads to satellite image)

hainich_map_hybrid <- get_map(hainich, maptype='hybrid', source="google", zoom=12)
(hybrid_map <- ggmap(hainich_map_hybrid) +
    xlab("Longitude") +
    ylab("Latitude"))


