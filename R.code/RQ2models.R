#### R script for MODELS in RQ2 ####

# Load libraries
library(dplyr)
library(tidyverse)
library(ggplot2) 

# Load Data
zscores_full <- read.csv("fullanomalies.csv", header = TRUE)

# Create subset dataframes for drought vs non drought

zscores_full$year_group <- ifelse(zscores_full$year %in% c(2000, 2001, 2002, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2019, 2020), "Non-drought",
                             ifelse(zscores_full$year %in% c(2003), "2003", 
                                    ifelse(zscores_full$year %in% c(2018), "2018", NA)))

zscores_full <- zscores_full %>%
  mutate(condition = ifelse(year %in% c(2003, 2018), "Drought", "Non-drought"))

zscores <- zscores_full %>%
  filter(year >= 2000 & year <= 2005 | year >= 2015 & year <= 2020)

summer_zscores <- zscores %>%
  filter(week >= 20 & week <= 39)

neg_gpp <- zscores %>%
  filter(gpp_z_scores < 0) # 305 out of 624 are negative


zscores$year_group <- ifelse(zscores$year %in% c(2000, 2001, 2002, 2004, 2005, 2015, 2016, 2017, 2019, 2020), "normal",
                             # ifelse(drivers$year %in% c(2015, 2016, 2017, 2019, 2020), "2015-2020",
                             ifelse(zscores$year %in% c(2003), "2003", 
                                    ifelse(zscores$year %in% c(2018), "2018", NA)))

nondrought <- zscores %>%
  filter(!(year %in% c(2003, 2018)))

drought <- zscores %>%
  filter(year %in% c(2002, 2003, 2004, 2017, 2018, 2019)) %>%
  filter(week >= 20 & week <= 39)

nondrought <- zscores %>%
  filter(year %in% c(2002, 2004, 2017, 2019)) %>%
  filter(week >= 20 & week <= 39)


drought2003 <- zscores %>%
  filter(year %in% c(2002, 2003, 2004)) %>%
  filter(week >= 20 & week <= 39)

drought2003 <- zscores %>%
  filter(year %in% c(2003, 2005)) %>%
  filter(week >= 18 & week <= 43)


# Models ####

library(lme4)
library(lmerTest)

cor(drought$temp_z_scores, drought$sm2_z_scores)
cor(drought$sm2_z_scores, drought$sm3_z_scores)
cor(drought$temp_z_scores, drought$sm3_z_scores)
cor(drought$temp_z_scores, drought$swr_z_scores)
cor(drought$sm3_z_scores, drought$swr_z_scores)

drought <- zscores %>%
  filter(year %in% c(2002, 2003, 2004, 2017, 2018, 2019)) %>%
  filter(week >= 20 & week <= 39)

model <- lmer(gpp_z_scores ~  temp_z_scores + swr_z_scores + sm2_z_scores + sm3_z_scores + (1|condition), data = drought)
summary(model)

residuals <- resid(model)
shapiro.test(residuals)
qqnorm(residuals) 
qqline(residuals)
vif_values <- vif(model)
print(vif_values)


library(MuMIn)

# Compute marginal and conditional R-squared
r2_values <- r.squaredGLMM(model)
print(r2_values)

anova_drought <- anova(model)
print(anova_drought)


nondrought <- zscores %>%
  filter(year %in% c(2002, 2004, 2017, 2019)) %>%
  filter(week >= 20 & week <= 39)

model2 <- lmer(gpp_z_scores ~  temp_z_scores + swr_z_scores + sm2_z_scores + sm3_z_scores + (1|year), data = nondrought)
summary(model2)

residuals <- resid(model2)
shapiro.test(residuals)
qqnorm(residuals) 
qqline(residuals)
vif_values <- vif(model2)
print(vif_values)
plot(model2, which = 1)

r2_values <- r.squaredGLMM(model2)
print(r2_values)

anova_nondrought <- anova(model2)
print(anova_nondrought)


# Correlation test of using new groupings #####
nondrought <- zscores %>%
  filter(year %in% c(2000, 2001, 2002, 2004, 2005, 2015, 2016, 2017, 2019, 2020)) %>%
  filter(week >= 20 & week <= 39)


nondrought <- na.omit(nondrought)

pcor_sm3 <- pcor.test(nondrought$gpp_z_scores, nondrought$sm3_z_scores, 
                      x = nondrought[, c("temp_z_scores", "swr_z_scores")])
pcor_sm2 <- pcor.test(nondrought$gpp_z_scores, nondrought$sm2_z_scores, 
                      x = nondrought[, c("temp_z_scores", "swr_z_scores")])
pcor_sm1 <- pcor.test(nondrought$gpp_z_scores, nondrought$sm1_z_scores, 
                      x = nondrought[, c("temp_z_scores", "swr_z_scores")])
pcor_maxT <- pcor.test(nondrought$gpp_z_scores, nondrought$temp_z_scores, 
                       x = nondrought[, c("sm3_z_scores", "swr_z_scores", "sm2_z_scores")])
pcor_swr <- pcor.test(nondrought$gpp_z_scores, nondrought$swr_z_scores, 
                      x = nondrought[, c("temp_z_scores", "sm3_z_scores","sm2_z_scores")])


print(pcor_sm1)
print(pcor_sm2)
print(pcor_sm3)
print(pcor_maxT)
print(pcor_swr)


drought <- zscores %>%
  filter(week >= 20 & week <= 39)

drought <- na.omit(drought)

pcor_sm3 <- pcor.test(drought$gpp_z_scores, drought$sm3_z_scores, 
                      x = drought[, c("temp_z_scores", "swr_z_scores")])
pcor_sm2 <- pcor.test(drought$gpp_z_scores, drought$sm2_z_scores, 
                      x = drought[, c("temp_z_scores", "swr_z_scores")])
pcor_sm1 <- pcor.test(drought$gpp_z_scores, drought$sm1_z_scores, 
                      x = drought[, c("temp_z_scores", "swr_z_scores")])
pcor_maxT <- pcor.test(drought$gpp_z_scores, drought$temp_z_scores, 
                       x = drought[, c("sm3_z_scores", "swr_z_scores", "sm2_z_scores")])
pcor_swr <- pcor.test(drought$gpp_z_scores, drought$swr_z_scores, 
                      x = drought[, c("temp_z_scores", "sm3_z_scores", "sm2_z_scores")])


print(pcor_sm1)
print(pcor_sm2)
print(pcor_sm3)
print(pcor_maxT)
print(pcor_swr)



# Plotting modelled scatters ####

# Predicted values using lm and lmer models
drought$model <- predict(model, newdata = drought, re.form = NA)

drought$model2 <- predict(model2, newdata = drought, re.form = NA)


drought$group <- ifelse(drought$year %in% c(2002, 2004), "Non-drought",
                        ifelse(drought$year %in% c(2017, 2019), "2017 & 2019",
                               ifelse(drought$year %in% c(2003, 2018), "Drought", NA)))

palette_drivers <- c("#96DB6B", "#FF8400E0", "#F2E857", "#FF8400E0", "#96DB6B", "#F2E857")
palette_drivers <- c("#96DB6B", "#F2E857", "#FF8400E0", "#2684FF", "#96DB6B", "#F2E857")
palette_drivers <- c("#FC9F35B9",  "#139DED","#FF8400E0", "#3EABE6B2", "#2684FF")


maxTplot <- ggplot(drought, aes(x = temp_z_scores, y = gpp_z_scores, colour = condition)) + 
  geom_point(aes(colour = condition, shape = condition)) +
  geom_smooth(aes(y = model, colour ="LMER with drought"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = model2, colour ="LMER no drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  # geom_smooth(se = FALSE, method = 'lm') +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "MaxT z-score", y = "GPP z-score")

plot(maxTplot)


swrplot <- ggplot(drought, aes(x = swr_z_scores, y = gpp_z_scores, colour = condition)) + 
  geom_point(aes(colour = condition, shape = condition)) +
  geom_smooth(aes(y = model, colour ="LMER with drought"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = model2, colour ="LMER no drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  # geom_smooth(se = FALSE, method = 'lm') +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SWR z-score", y = "GPP z-score")

plot(swrplot)


sm2plot <- ggplot(drought, aes(x = sm2_z_scores, y = gpp_z_scores, colour = condition)) + 
  geom_point(aes(colour = condition, shape = condition)) +
  #geom_smooth(se = FALSE, method = 'gam') +
  geom_smooth(aes(y = model, colour ="LMER with drought years"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = model2, colour ="LMER no drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SM2 z-score", y = "GPP z-score")

plot(sm2plot)


sm3plot <- ggplot(drought, aes(x = sm3_z_scores, y = gpp_z_scores)) + 
  geom_point(aes(colour = condition, shape = condition)) +
  # geom_smooth(se = FALSE, ) +
  # geom_smooth(se = FALSE, method = 'lm', formula = y ~ poly(x,2)) +
  geom_smooth(aes(y = model, colour ="LMER with drought"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = model2, colour ="LMER no drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "gam") +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SM3 z-score", y = "GPP z-score")

plot(sm3plot)



# combine the correlation plots
combined_rq2_plots1 <- grid.arrange(
  maxTplot, swrplot,
  sm2plot, sm3plot,
  nrow = 2, 
  layout_matrix = rbind(c(1,2), c(3, 4)), 
  heights = c(1,1))



combined_rq2_plots2 <- grid.arrange(
  maxT_plotz, swr_plotz,
  sm2_plotz, sm3_plotz, 
  nrow = 2, 
  layout_matrix = rbind(c(1,2), c(3, 4)), 
  heights = c(1,1))


# Save the plot as a PNG file to GitHub
ggsave("GPPvsDrivers.png", path = "Plots", plot = combined_rq2_plots1, width = 10, height = 7, dpi = 500)
ggsave("DriversTimeseries_plots.png", path = "Plots", plot = combined_rq2_plots2, width = 10, height = 7, dpi = 500)










#### Timeseries plots of z-scores ####

zscores$year_group <- ifelse(zscores$year %in% c(2000, 2001, 2002, 2004, 2005, 2015, 2016, 2017, 2019, 2020), "Non-drought years",
                             # ifelse(drivers$year %in% c(2015, 2016, 2017, 2019, 2020), "2015-2020",
                             ifelse(zscores$year %in% c(2003), "2003", 
                                    ifelse(zscores$year %in% c(2018), "2018", NA)))


nondrought2 <- zscores %>%
  filter((!year %in% c(2003, 2018)))

nondrought1 <- nondrought2 %>%
  group_by(doy) %>%
  mutate(mean_gpp = mean(gpp_z_scores, na.rm = TRUE),
         mean_maxT = mean(temp_z_scores, na.rm = TRUE),
         mean_sm1 = mean(sm1_z_scores, na.rm = TRUE),
         mean_sm2 = mean(sm2_z_scores, na.rm = TRUE),
         mean_sm3 = mean(sm3_z_scores, na.rm = TRUE),
         mean_vpd = mean(vpd_z_scores, na.rm = TRUE),
         mean_swr = mean(swr_z_scores, na.rm = TRUE)) %>%
  ungroup()

drought1 <- zscores %>%
  filter((year %in% c(2003, 2018)))

drought1 <- drought1 %>%
  group_by(year) %>%
  mutate(mean_gpp = gpp_z_scores,
         mean_maxT = temp_z_scores,
         mean_sm1 = sm1_z_scores, 
         mean_sm2 = sm2_z_scores, 
         mean_sm3 = sm3_z_scores, 
         mean_vpd = vpd_z_scores, 
         mean_swr = swr_z_scores) %>%
  ungroup()

new1 <- rbind(drought1, nondrought1)

new <- new1 %>%
  filter(week >= 20 & week <= 39)

palette_anomalies <- c("#D6A400", "#B80422", "darkgrey")
palette_anomalies <- c("#29B071", "#D6A400", "darkgrey")

gpp_plotz <- ggplot(new, aes(x = doy, y = mean_gpp, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Summer months",
       y = "GPP z-score",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  #scale_shape_manual() + 
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(126, 154, 182, 217, 252), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"))

plot(gpp_plotz)

maxT_plotz <- ggplot(new, aes(x = doy, y = mean_maxT, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Time (months)",
       y = "MaxT z-score",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  #scale_shape_manual() + 
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(126, 154, 182, 217, 252), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"))

plot(maxT_plotz)

sm1_plotz <- ggplot(new, aes(x = doy, y = mean_sm1, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Time (month)",
       y = "SM1 z-score",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  #scale_shape_manual() + 
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(126, 154, 182, 217, 252), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"))

plot(sm1_plotz)



sm2_plotz <- ggplot(new, aes(x = doy, y = mean_sm2, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Time (month)",
       y = "SM2 z-score",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  #scale_shape_manual() + 
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(126, 154, 182, 217, 252), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"))

plot(sm2_plotz)

sm3_plotz <- ggplot(new, aes(x = doy, y = mean_sm3, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Time (month)",
       y = "SM3 z-score",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  #scale_shape_manual() + 
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(126, 154, 182, 217, 252), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"))

plot(sm3_plotz)


vpd_plotz <- ggplot(new, aes(x = doy, y = mean_vpd, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Time (month)",
       y = "VPD z-score",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  #scale_shape_manual() + 
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(126, 154, 182, 217, 252), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"))
plot(vpd_plotz)

swr_plotz <- ggplot(new, aes(x = doy, y = mean_swr, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Time (months)",
       y = "SWR z-score",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  #scale_shape_manual() + 
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(126, 154, 182, 217, 252), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"))
plot(swr_plotz)

timeseries_rq2_plots <- grid.arrange(
  maxT_plotz, swr_plotz,
  sm2_plotz, sm3_plotz, 
  ncol = 2, 
  layout_matrix = rbind(c(1,2), c(3, 4)), 
  heights = c(1,1))





#### Plotting timeseries of temperature anomalies over 20 years
palette_anomalies <- c("#3EA85A", "#D6A400", "#D6D6D686")

temp_anomaly_plot <- ggplot(zscores_full, aes(x = week, y = temp_z_scores, colour = year_group, group = year)) +
  geom_line(size = 0.8) +
  geom_hline(yintercept = 0, size = 0.4, colour = "black") +
  # geom_text(aes(x = 34, y = 5.2, label = "95th percentile"), colour = "darkorange", size = 3) + 
  labs(title = "",
       x = "Time (months)",
       y = "MaxT anomaly (z-score)",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(18, 22, 27, 32, 36), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"),
                     expand = c(0, 0),
                     limits = c(18, 36)) +
  scale_y_continuous(breaks = c(-4,-3, -2, -1, 0, 1, 2, 3),
                     expand = c(0, 0),
                     limits = c(-3,3))

plot(temp_anomaly_plot)

zscores_full <- na.omit(zscores_full)

sm3_anomaly_plot <- ggplot(zscores_full, aes(x = week, y = sm3_z_scores, colour = year_group, group = year)) +
  geom_line(size = 0.8) +
  geom_hline(yintercept = 0, size = 0.4, colour = "black") +
  # geom_text(aes(x = 34, y = 5.2, label = "95th percentile"), colour = "darkorange", size = 3) + 
  labs(title = "",
       x = "Time (months)",
       y = "SM3 anomaly (z-score)",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies) +
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(18, 22, 27, 32, 36), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"),
                     expand = c(0, 0),
                     limits = c(18, 36)) +
  scale_y_continuous(breaks = c(-4, -3, -2, -1, 0, 1, 2, 3),
                     expand = c(0, 0),
                     limits = c(-4,3))

plot(sm3_anomaly_plot)

# Save the plots as a PNG file to GitHub
ggsave("MaxTanomalies.png", path = "Plots", plot = temp_anomaly_plot, width = 8, height = 5, dpi = 500)
ggsave("SM3anomalies.png", path = "Plots", plot = sm3_anomaly_plot, width = 8, height = 5, dpi = 500)


drought_anomaly_plots <- grid.arrange(
  temp_anomaly_plot, sm3_anomaly_plot,
  ncol = 2, 
  nrow = 1,
  layout_matrix = rbind(c(1,2)), 
  heights = c(1))

ggsave("combined_anomalies.png", path = "Plots", plot = drought_anomaly_plots, width = 10, height = 4, dpi = 500)


