#### Final code using wrangled datasheets 
#### for data analysis and visualisation

### Libraries
library(dplyr)
library(tidyr)
library(ggplot2) 
library(gridExtra)

### Load datafiles

data2000 <- read.csv("Data/data2000-2005.csv", header = TRUE)
data2015 <- read.csv("Data/data2015-2020.csv", header = TRUE)

# RQ1: Modelling ecosystem productivity response to 2 major drought events ####
# a) plotting timeseries of modelled and obs GPP over time (5 years) 
# ALSO need to include my calculations of annual GPP here!

gppdrought2003 <- ggplot(data2000, aes(x = day)) +
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

gppdrought2018 <- ggplot(data2015, aes(x = day)) +
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

laidrought2003 <- ggplot(data2000, aes(x = day)) +
  geom_ribbon(aes(ymin = obs_lai - obs_lai_unc, ymax = obs_lai + obs_lai_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_lai - mod_lai_unc95, ymax = mod_lai + mod_lai_unc95 ), fill = "#5D1CAD", alpha = 0.3) +
  geom_line(aes(y = mod_lai, colour = "Mod"), size = 0.6) +
  geom_point(aes(y = obs_lai, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "LAI (m² / m²)", colour = "Data:") +
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
                     limits = c(0,8))

laidrought2018 <- ggplot(data2015, aes(x = day)) +
  geom_ribbon(aes(ymin = obs_lai - obs_lai_unc, ymax = obs_lai + obs_lai_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_lai - mod_lai_unc95, ymax = mod_lai + mod_lai_unc95 ), fill = "#5D1CAD", alpha = 0.3) +
  geom_line(aes(y = mod_lai, colour = "Mod"), size = 0.6) +
  geom_point(aes(y = obs_lai, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "LAI (m² / m²)", colour = "Data:") +
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
                     limits = c(0,8))

recodrought2003 <- ggplot(data2000, aes(x = day)) +
  geom_ribbon(aes(ymin = obs_reco - obs_reco_unc, ymax = obs_reco + obs_reco_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_reco - mod_reco_unc95, ymax = mod_reco + mod_reco_unc95 ), fill = "#5D1CAD", alpha = 0.3) +
  geom_line(aes(y = mod_reco, colour = "Mod"), size = 0.6) +
  geom_point(aes(y = obs_reco, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "Reco (gC/m²/day)", colour = "Data:") +
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
                     limits = c(0,8))

recodrought2018 <- ggplot(data2015, aes(x = day)) +
  geom_ribbon(aes(ymin = obs_reco - obs_reco_unc, ymax = obs_reco + obs_reco_unc, colour = "Obs unc"), fill = "#FF730085", alpha = 0.3) +
  geom_ribbon(aes(ymin = mod_reco - mod_reco_unc95, ymax = mod_reco + mod_reco_unc95 ), fill = "#5D1CAD", alpha = 0.3) +
  geom_line(aes(y = mod_reco, colour = "Mod"), size = 0.6) +
  geom_point(aes(y = obs_reco, colour = "Obs"), size = 1.2) +
  labs(x = "Time (year)", y = "Reco (gC/m²/day)", colour = "Data:") +
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
                     limits = c(0,8))

plot(gppdrought2003)
plot(gppdrought2018)
plot(laidrought2003)
plot(laidrought2018)
plot(recodrought2003)
plot(recodrought2018)

ggsave("gpp2000_timeseries.png", path = "Plots", plot = gppdrought2003, width = 7, height = 5, dpi = 500)
ggsave("gpp2015_timeseries.png", path = "Plots", plot = gppdrought2018, width = 7, height = 5, dpi = 500)
ggsave("lai2000_timeseries.png", path = "Plots", plot = laidrought2003, width = 7, height = 5, dpi = 500)
ggsave("lai2015_timeseries.png", path = "Plots", plot = laidrought2018, width = 7, height = 5, dpi = 500)
ggsave("reco2000_timeseries.png", path = "Plots", plot = recodrought2003, width = 7, height = 5, dpi = 500)
ggsave("reco2015_timeseries.png", path = "Plots", plot = recodrought2018, width = 7, height = 5, dpi = 500)

# b) RMSE // R squared calculations and plotting correlation! -> appendix?

gppcorrelation2000 <- cor(data2000$mod_gpp, data2000$obs_gpp, use = "complete.obs")
obs_gpp2000 <- data2000$obs_gpp[!is.na(data2000$obs_gpp)]

# Square the correlation coefficient to get R^2
gppr_squared2000 <- gppcorrelation2000^2

gpprmse2000 <- sqrt(mean((data2000$obs_gpp - data2000$mod_gpp)^2, na.rm = TRUE))
lairmse2000 <- sqrt(mean((data2000$obs_lai - data2000$mod_lai)^2, na.rm = TRUE))
recormse2000 <- sqrt(mean((data2000$obs_reco - data2000$mod_reco)^2, na.rm = TRUE))

gpprelative_rmse2000 <- gpprmse2000 / (max(obs_gpp2000) - min(obs_gpp2000))

# Print the values
print(paste("R^2 value:", round(gppr_squared2000, 3)))
print(gpprmse2000)
print(gpprelative_rmse2000)

# Plot the relationships
gppcorrelation2000 <- ggplot(data2000, aes(x = mod_gpp, y = obs_gpp)) +
  geom_point(colour = "#5A00C75E") +
  labs(x = "Modelled GPP₂₀₀₃ (gC/m²/day)", y = "Observed GPP₂₀₀₃ (gC/m²/day)") +
  geom_abline(intercept = 0, slope = 1, color = "#5D1CAD", size = 0.6) +
  # geom_abline(intercept = 0, slope = max(data2000$obs_gpp, na.rm = TRUE) / max(data2000$mod_gpp, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 0, 
                y = 13), 
            label = paste("RMSE =", round(gpprmse2000, 2)), 
            hjust = 0, vjust = 1,
            size = 4, 
            fontface = "bold", 
            colour = "#5D1CAD") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9)) +
  scale_x_continuous(limits = c(0,16)) +
  scale_y_continuous(limits = c(0,16))

plot(gppcorrelation2000)

laicorrelation2000 <- ggplot(data2000, aes(x = mod_lai, y = obs_lai)) +
  geom_point(colour = "#059C0093") +
  labs(x = "Modelled LAI₂₀₀₃ (m² / m²)", y = "Observed LAI₂₀₀₃ (m² / m²)") +
  geom_abline(intercept = 0, slope = 1, color = "darkgreen", size = 0.6) +
  # geom_abline(intercept = 0, slope = max(data2000$obs_lai, na.rm = TRUE) / max(data2000$mod_lai, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 0, 
                y = 6), 
            label = paste("RMSE =", round(lairmse2000, 2)), 
            hjust = 0, vjust = 1,
            size = 4, 
            fontface = "bold", 
            colour = "darkgreen") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9)) +
  scale_x_continuous(limits = c(0,6)) +
  scale_y_continuous(limits = c(0,7))

plot(laicorrelation2000)

recocorrelation2000 <- ggplot(data2000, aes(x = mod_reco, y = obs_reco)) +
  geom_point(colour = "#eb1d129b") +
  labs(x = "Modelled Reco₂₀₀₃ (gC/m²/day)", y = "Observed Reco₂₀₀₃ (gC/m²/day)") +
  geom_abline(intercept = 0, slope = 1, color = "#ba0d04e5", size = 0.6) +
  # geom_abline(intercept = 0, slope = max(data2000$obs_reco, na.rm = TRUE) / max(data2000$mod_reco, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 0, 
                y = 6.5), 
            label = paste("RMSE =", round(recormse2000, 2)), 
            hjust = 0, vjust = 1,
            size = 4, 
            fontface = "bold", 
            colour = "#ba0d04e5") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9)) +
  scale_x_continuous(limits = c(0,6)) +
  scale_y_continuous(limits = c(0,8))

plot(recocorrelation2000)


## Same for 2015-2020
# Linear model (R squared; correlation test)
obs_gpp2015 <- data2015$obs_gpp[!is.na(data2015$obs_gpp)]
# Calculate correlation coefficient
gppcorrelation2015 <- cor(data2015$mod_gpp, data2015$obs_gpp, use = "complete.obs")
# Square the correlation coefficient to get R^2
gppr_squared2015 <- gppcorrelation2015^2

gpprmse2015 <- sqrt(mean((data2015$obs_gpp - data2015$mod_gpp)^2, na.rm = TRUE))
lairmse2015 <- sqrt(mean((data2015$obs_lai - data2015$mod_lai)^2, na.rm = TRUE))
recormse2015 <- sqrt(mean((data2015$obs_reco - data2015$mod_reco)^2, na.rm = TRUE))

gpprelative_rmse2015 <- 1.338 / (max(obs_gpp2015) - min(obs_gpp2015))

# Print the values
print(paste("R^2 value:", round(gppr_squared2015, 3)))
print(gpprmse2015)
print(gpprelative_rmse2015)

# Plot the relationships
gppcorrelation2015 <- ggplot(data2015, aes(x = mod_gpp, y = obs_gpp)) +
  geom_point(colour = "#5A00C75E") +
  labs(x = "Modelled GPP₂₀₁₈ (gC/m²/day)", y = "Observed GPP₂₀₁₈ (gC/m²/day)") +
  geom_abline(intercept = 0, slope = 1, color = "#5D1CAD", size = 0.6) +
  # geom_abline(intercept = 0, slope = max(data2015$obs_gpp, na.rm = TRUE) / max(data2015$mod_gpp, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 0, 
                y = 13), 
            label = paste("RMSE =", round(gpprmse2015, 2)), 
            hjust = 0, vjust = 1,
            size = 4, 
            fontface = "bold", 
            colour = "#5D1CAD") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9)) +
  scale_x_continuous(limits = c(0,16)) +
  scale_y_continuous(limits = c(0,16))

plot(gppcorrelation2015)

laicorrelation2015 <- ggplot(data2015, aes(x = mod_lai, y = obs_lai)) +
  geom_point(colour = "#059C0093") +
  labs(x = "Modelled LAI₂₀₁₈ (m² / m²)", y = "Observed LAI₂₀₁₈ (m² / m²)") +
  geom_abline(intercept = 0, slope = 1, color = "darkgreen", size = 0.6) +
  # geom_abline(intercept = 0, slope = max(data2015$obs_lai, na.rm = TRUE) / max(data2015$mod_lai, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 0, 
                y = 6), 
            label = paste("RMSE =", round(lairmse2015, 2)), 
            hjust = 0, vjust = 1,
            size = 4, 
            fontface = "bold", 
            colour = "darkgreen") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9))+
  scale_x_continuous(limits = c(0,6)) +
  scale_y_continuous(limits = c(0,7))

plot(laicorrelation2015)

recocorrelation2015 <- ggplot(data2015, aes(x = mod_reco, y = obs_reco)) +
  geom_point(colour = "#eb1d129b") +
  labs(x = "Modelled Reco₂₀₁₈ (gC/m²/day)", y = "Observed Reco₂₀₁₈ (gC/m²/day)") +
  geom_abline(intercept = 0, slope = 1, color = "#ba0d04e5", size = 0.6) +
  # geom_abline(intercept = 0, slope = max(data2015$obs_reco, na.rm = TRUE) / max(data2015$mod_reco, na.rm = TRUE), linetype = "dashed", color = "black") +
  geom_text(aes(x = 0, 
                y = 6.5), 
            label = paste("RMSE =", round(recormse2015, 2)), 
            hjust = 0, vjust = 1,
            size = 4, 
            fontface = "bold", 
            colour = "#ba0d04e5") +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=11),
        axis.text = element_text(size=9)) +
  scale_x_continuous(limits = c(0,6)) +
  scale_y_continuous(limits = c(0,8))

plot(recocorrelation2015)

# combine the correlation plots
combined_cor_plots <- grid.arrange(
  gppcorrelation2000, recocorrelation2000, laicorrelation2000, 
  gppcorrelation2015, recocorrelation2015, laicorrelation2015, 
  nrow = 2, 
  layout_matrix = rbind(c(1,2,3), c(4,5,6)), 
  heights = c(1,1))


ggsave("correlation_plots.png", path = "Plots", plot = combined_cor_plots, width = 10, height = 7, dpi = 500)
ggsave("gpp2015_correlation.png", path = "Plots", plot = correlation2015, width = 5, height = 5, dpi = 500)

# Statistical test to assess whether gpp is significantly different by year:

lm_annualgpp2000 <- lm(mod_gpp ~ mod_lai, data = data2000)
lm_annualgpp2015 <- lm(mod_gpp ~ obs_gpp, data = data2015)
lm_annualgppboth <- lm(mod_gpp ~ year, data = combinedyears)

summary(lm_annualgpp2015)


### RQ2: Drivers vs response variables ####
# should i maybe include more drivers, such as VPD; also respiration??







