#### Final code using wrangled datasheets 
#### for data analysis and visualisation

### Libraries
library(dplyr)
library(tidyr)
library(ggplot2) 

### Load datafiles

gpp2000 <- read.csv("Data/gpp2000.csv", header = TRUE)
gpp2015 <- read.csv("Data/gpp2015.csv", header = TRUE)

# RQ1: 
# a) plotting timeseries of modelled and obs GPP over time (5 years) 
# ALSO need to include my calculations of annual GPP here!

drought2003 <- ggplot(gpp2000, aes(x = day)) +
  geom_ribbon(aes(ymin = obs_gpp - obs_gpp_unc, ymax = obs_gpp + obs_gpp_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_gpp - mod_gpp_unc95, ymax = mod_gpp + mod_gpp_unc95 ), fill = "#5D1CAD", alpha = 0.3) +
  geom_line(aes(y = mod_gpp, colour = "Mod"), linewidth = 0.6) +
  geom_point(aes(y = obs_gpp, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "GPP (gC/m²/day)", colour = "Data:") +
  scale_color_manual(values = c("Mod" = "#5D1CAD", "Obs" = "#FF730085", "Obs unc" ="#FF73001F")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_x_continuous(breaks = c(182, 553, 917, 1281, 1645, 2009), 
                     labels = c("2000", "2001", "2002", "2003", "2004", "2005"),
                     expand = c(0, 0),
                     limits = c(0,2184)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,17))

drought2018 <- ggplot(gpp2015, aes(x = day)) +
  geom_ribbon(aes(ymin = obs_gpp - obs_gpp_unc, ymax = obs_gpp + obs_gpp_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_gpp - mod_gpp_unc95, ymax = mod_gpp + mod_gpp_unc95 ), fill = "#5D1CAD", alpha = 0.3) +
  geom_line(aes(y = mod_gpp, colour = "Mod"), linewidth = 0.6) +
  geom_point(aes(y = obs_gpp, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "GPP (gC/m²/day)", colour = "Data:") +
  scale_color_manual(values = c("Mod" = "#5D1CAD", "Obs" = "#FF730085", "Obs unc" ="#FF73001F")) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_x_continuous(breaks = c(182, 553, 917, 1281, 1645, 2009), 
                     labels = c("2015", "2016", "2017", "2018", "2019", "2020"),
                     expand = c(0, 0),
                     limits = c(0,2184)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,17))

plot(drought2003)
plot(drought2018)

ggsave("gpp2015_timeseries.png", path = "Plots", plot = drought2018, width = 7, height = 5, dpi = 500)

ggsave()

# b) RMSE // R squared calculations and plotting correlation! -> appendix?

correlation2000 <- cor(merged_data2000$mod_gpp, merged_data2000$obs_gpp, use = "complete.obs")
obs_gpp2000 <- merged_data2000$obs_gpp[!is.na(merged_data2000$obs_gpp)]

# Square the correlation coefficient to get R^2
r_squared2000 <- correlation2000^2
rmse2000 <- sqrt(mean((obs_gpp2000 - merged_data2000$mod_gpp)^2, na.rm = TRUE))
relative_rmse2000 <- rmse2000 / (max(obs_gpp) - min(obs_gpp))

# Print the values
print(paste("R^2 value:", round(r_squared2000, 3)))

# Plot the relationship
ggplot(merged_data2000, aes(x = mod_gpp, y = obs_gpp)) +
  geom_point(colour = "#5A00C75E") +
  labs(x = "Modelled GPP (gC/m²/day)", y = "Observed GPP (gC/m²/day)") +
  geom_abline(intercept = 0, slope = 1, color = "#5D1CAD", size = 0.6) +
  geom_abline(intercept = 0, slope = max(merged_data2000$obs_gpp, na.rm = TRUE) / max(merged_data2000$mod_gpp, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 11, 
                y = 6), 
            label = paste("R² =", round(r_squared2000, 3)), 
            hjust = 0, vjust = 1,
            size = 5, 
            fontface = "bold", 
            colour = "#5D1CAD") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9))

## Same for 2015-2020
# Linear model (R squared; correlation test)
obs_gpp <- merged_data$obs_gpp[!is.na(merged_data$obs_gpp)]
# Calculate correlation coefficient
correlation2020 <- cor(merged_data$mod_gpp, merged_data$obs_gpp, use = "complete.obs")
# Square the correlation coefficient to get R^2
r_squared2020 <- correlation2020^2
rmse2020 <- sqrt(mean((merged_data$obs_gpp - merged_data$mod_gpp)^2, na.rm = TRUE))
relative_rmse2020 <- 1.337 / (max(obs_gpp) - min(obs_gpp))
# Print the values
print(paste("R^2 value:", round(r_squared2020, 3)))
print(rmse2020)
print(relative_rmse2020)

# Plot the relationship
ggplot(merged_data, aes(x = mod_gpp, y = obs_gpp)) +
  geom_point(colour = "#5A00C75E") +
  labs(x = "Modelled GPP (gC/m²/day)", y = "Observed GPP (gC/m²/day)") +
  geom_abline(intercept = 0, slope = 1, color = "#5D1CAD", size = 0.6) +
  geom_abline(intercept = 0, slope = max(merged_data$obs_gpp, na.rm = TRUE) / max(merged_data$mod_gpp, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 11, 
                y = 6), 
            label = paste("R² =", round(r_squared2020, 3)), 
            hjust = 0, vjust = 1,
            size = 5, 
            fontface = "bold", 
            colour = "#5D1CAD") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9))

### RQ2: Drivers vs response variables 
# should i maybe include more drivers, such as VPD; also respiration??







