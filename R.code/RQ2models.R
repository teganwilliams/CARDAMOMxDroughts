#### R script for MODELS in RQ2 ####

# Load libraries
library(dplyr)
library(tidyverse)
library(ggplot2) 

# Load Data
zscores <- read.csv("Data/fullanomalies.csv", header = TRUE)

# Create subset dataframes for drought vs non drought

summer_zscores <- zscores %>%
  filter(week >= 20 & week <= 39)

neg_gpp <- zscores %>%
  filter(gpp_z_scores < 0) # 305 out of 624 are negative

zscores <- zscores %>%
  mutate(condition = ifelse(year %in% c(2003, 2018), "drought", "normal"))

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


# Models

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

model <- lmer(gpp_z_scores ~  temp_z_scores + swr_z_scores + sm1_z_scores + sm2_z_scores + sm3_z_scores + (1|condition), data = drought)
summary(model)

residuals <- resid(model)
shapiro.test(residuals)
qqnorm(residuals) 
qqline(residuals)


nondrought <- zscores %>%
  filter(year %in% c(2002, 2004, 2017, 2019)) %>%
  filter(week >= 20 & week <= 39)

model2 <- lmer(gpp_z_scores ~  temp_z_scores + swr_z_scores + sm1_z_scores + sm2_z_scores + sm3_z_scores + (1|year), data = nondrought)
summary(model2)

residuals <- resid(model2)
shapiro.test(residuals)
qqnorm(residuals) 
qqline(residuals)


model <- lmer(gpp_z_scores ~  temp_z_scores * sm1_z_scores * sm2_z_scores * sm3_z_scores * vpd_z_scores + (1|condition), data = summer_zscores)
summary(model)

residuals <- resid(model)
shapiro.test(residuals)
qqnorm(residuals) 
qqline(residuals)


model <- glm(gpp_z_scores ~  temp_z_scores * sm1_z_scores * sm2_z_scores * sm3_z_scores * vpd_z_scores, data = summer_zscores)
summary(model)

residuals <- resid(model)
shapiro.test(residuals)
qqnorm(residuals) 
qqline(residuals)


# Plotting modelled scatters ####

# Predicted values using lm and lmer models
drought$model <- predict(model, newdata = drought, re.form = NA)

drought$model2 <- predict(model2, newdata = drought, re.form = NA)


drought$group <- ifelse(drought$year %in% c(2002, 2004), "Non-drought",
                        ifelse(drought$year %in% c(2017, 2019), "2017 & 2019",
                               ifelse(drought$year %in% c(2003, 2018), "Drought", NA)))

palette_drivers <- c("#96DB6B", "#FF8400E0", "#F2E857", "#FF8400E0", "#96DB6B", "#F2E857")
palette_drivers <- c("#96DB6B", "#F2E857", "#FF8400E0", "#2684FF", "#96DB6B", "#F2E857")
palette_drivers <- c("#FC9F35B9", "#FF8400E0", "#139DED", "#3EABE6B2", "#2684FF")


maxTplot <- ggplot(drought, aes(x = temp_z_scores, y = gpp_z_scores, colour = condition)) + 
  geom_point(aes(colour = condition, shape = condition)) +
  geom_smooth(aes(y = model, colour ="LM"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = model2, colour ="LMER 2000-2005"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  # geom_smooth(se = FALSE, method = 'lm') +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "MaxT z-score", y = "GPP z-score")

plot(maxTplot)


swrplot <- ggplot(drought, aes(x = swr_z_scores, y = gpp_z_scores, colour = condition)) + 
  geom_point(aes(colour = condition, shape = condition)) +
  geom_smooth(aes(y = model, colour ="LMER with drought"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = model2, colour ="LMER non-drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  # geom_smooth(se = FALSE, method = 'lm') +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SWR z-score", y = "GPP z-score")

plot(swrplot)

sm1plot <- ggplot(drought, aes(x = sm1_z_scores, y = gpp_z_scores, colour = condition)) + 
  geom_point(aes(colour = condition, shape = condition)) +
  #geom_smooth(se = FALSE, method = 'gam') +
  geom_smooth(aes(y = model, colour ="LMER with drought years"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = model2, colour ="LMER non-drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SM1 z-score", y = "GPP z-score")

plot(sm1plot)


sm2plot <- ggplot(drought, aes(x = sm2_z_scores, y = gpp_z_scores, colour = condition)) + 
  geom_point(aes(colour = condition, shape = condition)) +
  #geom_smooth(se = FALSE, method = 'gam') +
  geom_smooth(aes(y = model, colour ="LMER with drought years"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = model2, colour ="LMER non-drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("normal" = 16, "drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
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
  geom_smooth(aes(y = model2, colour ="LMER non-drought"), se = FALSE, method = "lm") +
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
  nrow = 2, 
  layout_matrix = rbind(c(1,2)), 
  heights = c(1,1))

combined_rq2_plots2 <- grid.arrange(
  sm1plot, sm2plot, sm3plot, 
  nrow = 2, 
  layout_matrix = rbind(c(1,2), c(3, 3)), 
  heights = c(1,1))



# Save the plot as a PNG file to GitHub
setwd("/exports/csce/datastore/geos/groups/gcel/for_Tegan/droughts")
ggsave("rq2_plots.png", path = "Plots", plot = combined_rq2_plots, width = 10, height = 7, dpi = 500)










#### Timeseries plots of z-scores ####
nondrought <- nondrought %>%
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
  mutate(mean_maxT = temp_z_scores, mean_gpp = gpp_z_scores, mean_sm1 = sm1_z_scores, mean_sm2 = sm2_z_scores, mean_sm3 = sm3_z_scores, mean_vpd = vpd_z_scores, mean_swr = swr_z_scores) %>%
  ungroup()

new <- rbind(drought1, nondrought)

palette_anomalies <- c("#D6A400", "#B80422", "darkgrey")

gpp_plotz <- ggplot(new, aes(x = doy, y = mean_gpp, group = year_group, colour = year_group, linetype = condition)) +
  geom_line(size = 0.8) +
  labs(title = "",
       x = "Summer months",
       y = "GPP (gC/m²/day)",
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
       y = "Max temperature (°C)",
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
       y = "Soil moisture at depth 2 (X)",
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
       y = "Soil moisture at depth 2 (X)",
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
       y = "Soil moisture at depth 3 (X)",
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
       y = "VPD (kPa)",
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
       y = "SWR (MJ/m²/day)",
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







#### Modelling drivers relationship with GPP

# ANOVA
# Combined ANOVA
anova_all <- aov(gpp_z_scores ~ temp_z_scores + sm1_z_scores + sm2_z_scores + sm3_z_scores + vpd_z_scores + swr_z_scores, data = neg_gpp)
summary(anova_all) # no significant effect

anova_maxT <- aov(gpp_z_scores ~ temp_z_scores, data = nondrought)
anova_sm3 <- aov(mod_gpp ~ sm3, data = drought)
anova_vpd <- aov(mod_gpp ~ vpd, data = drought)
anova_swr <- aov(mod_gpp ~ swr, data = drought)
anova_all <- aov(gpp_z_scores ~ temp_z_scores + sm1_z_scores + sm2_z_scores + sm3_z_scores + vpd_z_scores + swr_z_scores, data = nondrought)
summary(anova_all)
summary(anova_maxT)


ggplot()








