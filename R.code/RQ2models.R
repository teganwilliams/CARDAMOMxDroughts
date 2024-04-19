#### R script for MODELS in RQ2 ####

# Load libraries
library(dplyr)
library(tidyverse)
library(ggplot2) 

# Load Data
zscores_full <- read.csv("fullanomalies.csv", header = TRUE)
zscores <- read.csv("Data/fullanomalies.csv", header = TRUE)
zscores <- read.csv("fullanomalies2.csv", header = TRUE)

# Create subset dataframes for drought vs non drought

zscores_full$year_group <- ifelse(zscores_full$year %in% c(2000, 2001, 2002, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2019, 2020), "Non-drought",
                             ifelse(zscores_full$year %in% c(2003), "2003", 
                                    ifelse(zscores_full$year %in% c(2018), "2018", NA)))

zscores_full <- zscores_full %>%
  mutate(condition = ifelse(year %in% c(2003, 2018), "Drought", "Non-drought"))

zscores <- zscores %>%
  mutate(condition = ifelse(year %in% c(2003, 2018), "Drought", "Non-drought"))

zscores2 <- zscores_full %>%
  filter(year >= 2000 & year <= 2005 | year >= 2015 & year <= 2020)

summer_zscores <- zscores_full %>%
  filter(week >= 20 & week <= 39)
summer_zscores2 <- zscores2 %>%
  filter(week >= 20 & week <= 39)

neg_gpp <- zscores %>%
  filter(gpp_z_scores < 0) # 305 out of 624 are negative


zscores$year_group <- ifelse(zscores$year %in% c(2000, 2001, 2002, 2004, 2005, 2015, 2016, 2017, 2019, 2020), "normal",
                             # ifelse(drivers$year %in% c(2015, 2016, 2017, 2019, 2020), "2015-2020",
                             ifelse(zscores$year %in% c(2003), "2003", 
                                    ifelse(zscores$year %in% c(2018), "2018", NA)))

zscores$condition <- ifelse(zscores$year %in% c(2000, 2001, 2002, 2004, 2005, 2015, 2016, 2017, 2019, 2020), "Non-drought",
                                    ifelse(zscores$year %in% c(2003, 2018), "Drought", NA))


nondrought <- zscores %>%
  filter(!(year %in% c(2003, 2018)))

drought <- zscores2 %>%
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
library(viridis)

install.packages("glmmTMB")
library(glmmTMB)

cor(drought$temp_z_scores, drought$sm2_z_scores)
cor(drought$sm2_z_scores, drought$sm3_z_scores)
cor(drought$temp_z_scores, drought$sm3_z_scores)
cor(drought$temp_z_scores, drought$swr_z_scores)
cor(drought$sm3_z_scores, drought$swr_z_scores)

drought <- zscores2 %>%
  filter(year %in% c( 2002, 2003, 2004, 2017, 2018, 2019)) %>%
  filter(week >= 27 & week <= 37)

drought <- zscores2 %>%
  filter(year %in% c( 2002, 2003, 2004, 2017, 2018, 2019)) %>%
  filter(week >= 20 & week <= 40)

drought <- zscores2 %>%
  filter(year %in% c(2003, 2018)) 


drought <- zscores2 %>%
  filter(week >= 20 & week <= 40)


# FINAL

drought <- zscores2 %>%
  filter(year %in% c(2003, 2018))%>%
  filter(week >= 20 & week <= 39)

nondrought <- zscores2 %>%
  filter(!year %in% c(2003, 2018))

nondrought <- zscores2 %>%
  filter(!year %in% c(2003, 2018))%>%
  filter(week >= 20 & week <= 39)

both <- zscores2

model <- glm(gpp_z_scores ~  swr_z_scores + temp_z_scores + sm3_z_scores,
            family = gaussian,
            data = drought)
summary(model)

modelnull <- glm(gpp_z_scores ~ 1, family = gaussian, data = drought)

hist(drought$gpp_z_scores)
hist(nondrought$gpp_z_scores)

modelnon <- lmer(gpp_z_scores ~  swr_z_scores + temp_z_scores + sm3_z_scores + (1|year),
                  data = nondrought)
summary(modelnon)

modelnullnon <- glm(gpp_z_scores ~ 1, data = nondrought)
summary(modelnullnon)

modelboth <- lmer(gpp_z_scores ~  temp_z_scores + sm3_z_scores + precip_z_scores + (1|year), data = both)
summary(modelboth) # does not fit the data (skewed)


logLik_fitted <- logLik(model)
logLik_null <- logLik(modelnull)

# Compute McFadden's R^2
mcfadden_r2 <- 1 - (logLik_fitted/logLik_null)
# Compute Cox & Snell R^2
n <- nrow(drought)
cox_snell_r2 <- 1 - (logLik_fitted/logLik_null)^(2/n)

# Print the pseudo-R^2 values
print(mcfadden_r2)
print(cox_snell_r2)


logLik_fitted <- logLik(modelnon)
logLik_null <- logLik(modelnullnon)

# Compute McFadden's R^2
mcfadden_r2 <- 1 - (logLik_fitted/logLik_null)
# Compute Cox & Snell R^2
n <- nrow(nondrought)
cox_snell_r2 <- 1 - (logLik_fitted/logLik_null)^(2/n)

# Print the pseudo-R^2 values
print(mcfadden_r2)
print(cox_snell_r2)



# THESE TWO 

model <- lmer(gpp_z_scores ~  temp_z_scores + sm2_z_scores + swr_z_scores +
                (1|year), data = nondrought)
summary(model)

model <- lmer(gpp_z_scores ~  temp_z_scores + sm3_z_scores + (1|year), data = nondrought)
summary(model)




model <- glm(gpp_z_scores ~  swr_z_scores + sm3_z_scores + temp_z_scores, data = drought)
summary(model)

modelnon <- lmer(gpp_z_scores ~  swr_z_scores + sm3_z_scores + temp_z_scores + (1|year), data = nondrought1)
summary(modelnon)




model <- lm(gpp_z_scores ~  temp_z_scores + swr_z_scores + vpd_z_scores, data = drought)
summary(model)


modelnon <- lm(gpp_z_scores ~  temp_z_scores + sm3_z_scores, data = nondrought)
summary(modelnon)


model3 <- lmer(gpp_z_scores ~  temp_z_scores + swr_z_scores + precip_z_scores + sm3_z_scores + (1|year), data = drought)
summary(model3)

AIC(model, modelnon, model_null)

model <- lmer(gpp_z_scores ~  sm3_z_scores + sm2_z_scores + sm1_z_scores + (1|condition), data = drought)
summary(model)

model <- lmer(gpp_z_scores ~  swr_z_scores + temp_z_scores + (1|year_group), data = drought)
summary(model)

residuals <- resid(modelnon)
shapiro.test(residuals)
qqnorm(residuals) 
qqline(residuals)

# install.packages("car")
library(car)
vif_values <- vif(model)
print(vif_values)



# Print the pseudo-R^2 values
print(mcfadden_r2)
print(cox_snell_r2)


r2_values <- r.squaredGLMM(model)
rsquared(model)
print(r2_values)

library(MuMIn)

# Compute marginal and conditional R-squared
r2_values <- r.squaredGLMM(model)
print(r2_values)

anova_drought <- anova(model)
print(anova_drought)


drought <- zscores2 %>%
  filter(year %in% c(2003, 2018)) %>%
  filter(week >= 18 & week <= 37)

nondrought <- zscores2 %>%
  filter(!year %in% c(2003, 2018)) %>%
  filter(week >= 15 & week <= 40)


nondrought <- zscores2 %>%
  filter(year %in% c( 2002, 2004, 2017, 2019)) %>%
  filter(week >= 25 & week <= 39)

model <- lm(gpp_z_scores ~ swr_z_scores * temp_z_scores + sm3_z_scores, data = drought)
summary(model)

model <- lm(gpp_z_scores ~ swr_z_scores, data = nondrought)
summary(model)

modelnon <- lm(gpp_z_scores ~  temp_z_scores * sm3_z_scores, data = nondrought)
summary(modelnon)

residuals <- resid(model)
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
library(ppcor)
nondrought <- zscores %>%
  filter(year %in% c(2000, 2001, 2002, 2004, 2005, 2015, 2016, 2017, 2019, 2020)) %>%
  filter(week >= 20 & week <= 39)


nondrought <- na.omit(nondrought)

pcor_sm3 <- pcor.test(drought$gpp_z_scores,drought$sm3_z_scores, 
                      x = drought[, c("temp_z_scores", "swr_z_scores")])
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


drought <- zscores2 %>%
  filter(year %in% c(2003)) %>%
  filter(week >= 18 & week <= 37)

  
drought <- zscores2 %>%
  filter(year %in% c(2018)) %>%
  filter(week >= 18 & week <= 37)


drought <- zscores2 %>%
  filter(!year %in% c(2003, 2018)) %>%
  filter(week >= 18 & week <= 37)


drought <- na.omit(drought)

pcor_sm3 <- pcor.test(drought$gpp_z_scores, drought$sm3_z_scores, 
                      x = drought[, c("temp_z_scores", "swr_z_scores", "sm2_z_scores")])
pcor_sm2 <- pcor.test(drought$gpp_z_scores, drought$sm2_z_scores, 
                      x = drought[, c("temp_z_scores", "swr_z_scores", "sm3_z_scores")])
pcor_sm3 <- pcor.test(drought$gpp_z_scores, drought$sm3_z_scores, 
                      x = drought[, c("temp_z_scores", "swr_z_scores")])


pcor_sm3 <- pcor.test(drought$gpp_z_scores, drought$sm3_z_scores, 
                      x = drought[, c("temp_z_scores", "swr_z_scores")])
pcor_maxT <- pcor.test(drought$gpp_z_scores, drought$temp_z_scores, 
                       x = drought[, c("swr_z_scores")])
pcor_swr <- pcor.test(drought$gpp_z_scores, drought$swr_z_scores, 
                      x = drought[, c("temp_z_scores")])

print(pcor_sm3)
print(pcor_maxT)
print(pcor_swr)


print(pcor_sm1)
print(pcor_sm2)





# Plotting modelled scatters ####

# Predicted values using lm and lmer models
zscores2$model <- predict(model, newdata = zscores2, re.form = NA)

zscores2$modelnon <- predict(modelnon, newdata = zscores2, re.form = NA)


drought$group <- ifelse(drought$year %in% c(2002, 2004), "Non-drought",
                        ifelse(drought$year %in% c(2017, 2019), "2017 & 2019",
                               ifelse(drought$year %in% c(2003, 2018), "Drought", NA)))

palette_drivers <- c("#96DB6B", "#FF8400E0", "#F2E857", "#FF8400E0", "#96DB6B", "#F2E857")
palette_drivers <- c("#96DB6B", "#F2E857", "#FF8400E0", "#2684FF", "#96DB6B", "#F2E857")
palette_drivers <- c("#FC9F35B9", "#3EABE6B2", "#139DED","#FF8400E0", "#3EABE6B2", "#2684FF")

library(ggplot2)

maxTplot <- ggplot(summer_zscores2, aes(x = temp_z_scores, y = gpp_z_scores, colour = condition)) + 
  geom_point(aes(colour = condition, shape = condition)) +
  # geom_smooth(aes(y = model, colour ="LMER with drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = modelnon, colour ="LMER no drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  geom_smooth(se = FALSE, method = 'lm') +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("Non-drought" = 16, "Drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "MaxT z-score", y = "GPP z-score")

plot(maxTplot)

precipplot <- ggplot(zscores2, aes(x = precip_z_scores, y = gpp_z_scores, colour = year_group)) + 
  geom_point(aes(colour = condition, shape = condition)) +
  geom_smooth(aes(y = model, colour ="LMER with drought"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = modelnon, colour ="LMER no drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  # geom_smooth(se = FALSE, method = 'lm') +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("Non-drought" = 16, "Drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "Precip z-score", y = "GPP z-score")

plot(precipplot)

swrplot <- ggplot(summer_zscores2, aes(x = swr_z_scores, y = gpp_z_scores, colour = condition)) + 
  geom_point(aes(colour = condition, shape = condition)) +
  # geom_smooth(aes(y = model, colour ="LMER with drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = modelnon, colour ="LMER no drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  geom_smooth(se = FALSE, method = 'lm') +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("Non-drought" = 16, "Drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
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
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SM2 z-score", y = "GPP z-score")

plot(sm2plot)


sm3plot <- ggplot(summer_zscores2, aes(x = sm3_z_scores, y = gpp_z_scores, colour = condition)) + 
  geom_point(aes(colour = condition, shape = condition)) +
  # geom_smooth(aes(y = model, colour ="LMER with drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = modelnon, colour ="LMER no drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  geom_smooth(se = FALSE, method = 'lm') +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("Non-drought" = 16, "Drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SM3 z-score", y = "GPP z-score")

plot(sm3plot)


sm2plot <- ggplot(zscores2, aes(x = sm2_z_scores, y = gpp_z_scores, colour = year_group)) + 
  geom_point(aes(colour = condition, shape = condition)) +
  geom_smooth(aes(y = model, colour ="LMER with drought"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = modelnon, colour ="LMER no drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  # geom_smooth(se = FALSE, method = 'lm') +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("Non-drought" = 16, "Drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SM3 z-score", y = "GPP z-score")

plot(sm2plot)


sm1plot <- ggplot(zscores2, aes(x = sm1_z_scores, y = gpp_z_scores, colour = year_group)) + 
  geom_point(aes(colour = condition, shape = condition)) +
  geom_smooth(aes(y = model, colour ="LMER with drought"), se = FALSE, method = "lm") +
  geom_smooth(aes(y = modelnon, colour ="LMER no drought"), se = FALSE, method = "lm") +
  # geom_smooth(aes(y = pred_gpp_2015, colour ="LMER 2015-2020"), se = FALSE, method = "lm") +
  # geom_smooth(se = FALSE, method = 'lm') +
  scale_color_manual(values = palette_drivers) +
  scale_shape_manual(values = c("Non-drought" = 16, "Drought" = 17)) +
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold")) +
  labs(x = "SM1 z-score", y = "GPP z-score")

plot(sm1plot)



# combine the correlation plots

library(gridExtra)

combined_rq2_plots1 <- grid.arrange(
  swrplot, maxTplot, sm3plot,
  nrow = 3, 
  layout_matrix = rbind(c(1,2,3)))

combined_rq2_plots1 <- gridExtra::grid.arrange(
  swrplot, sm3plot, maxTplot,
  nrow = 2, 
  layout_matrix = rbind(c(1,2), c(3)),
  heights = c(1, 1)
)

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
ggsave("GPPvsDrivers.png", path = "Plots", plot = combined_rq2_plots1, width = 10, height = 4, dpi = 500)
ggsave("DriversTimeseries_plots.png", path = "Plots", plot = combined_rq2_plots2, width = 10, height = 7, dpi = 500)








#### Timeseries plots of z-scores ####

zscores$year_group <- ifelse(zscores$year %in% c(2000, 2001, 2002, 2004, 2005, 2015, 2016, 2017, 2019, 2020), "Non-drought years",
                             # ifelse(drivers$year %in% c(2015, 2016, 2017, 2019, 2020), "2015-2020",
                             ifelse(zscores$year %in% c(2003), "2003", 
                                    ifelse(zscores$year %in% c(2018), "2018", NA)))

zscores$year_group <- ifelse(zscores$year %in% c(2000, 2001, 2002, 2004, 2005, 2015, 2016, 2017, 2019, 2020), "Non-drought",
                             # ifelse(drivers$year %in% c(2015, 2016, 2017, 2019, 2020), "2015-2020",
                             ifelse(zscores$year %in% c(2003, 2018), "Drought", NA))

library(dplyr)

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
  group_by(week) %>%
  mutate(mean_gpp = mean(gpp_z_scores, na.rm = TRUE),
         mean_maxT = mean(temp_z_scores, na.rm = TRUE),
         mean_sm1 = mean(sm1_z_scores, na.rm = TRUE),
         mean_sm2 = mean(sm2_z_scores, na.rm = TRUE),
         mean_sm3 = mean(sm3_z_scores, na.rm = TRUE),
         mean_vpd = mean(vpd_z_scores, na.rm = TRUE),
         mean_swr = mean(swr_z_scores, na.rm = TRUE)) %>%
  ungroup()

new1 <- rbind(drought1, nondrought1)

new <- new1 %>%
  filter(week >= 20 & week <= 39)

palette_anomalies <- c("#D6A400", "#B80422", "darkgrey")
palette_anomalies <- c("#29B071", "#D6A400", "darkgrey")
palette_anomalies <- c("#FC8F00", "darkgrey")

gpp_plotz <- ggplot(new, aes(x = doy, y = mean_gpp, group = year_group, colour = year_group, linetype = year_group)) +
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
                     labels = c("May", "Jun", "Jul", "Aug", "Sep")) +
  scale_y_continuous(limits = c(-3.1, 3),
                     breaks = c(-3, -2, -1, 0, 1, 2, 3))


plot(gpp_plotz)

maxT_plotz <- ggplot(new, aes(x = doy, y = mean_maxT, group = year_group, colour = year_group, linetype = year_group)) +
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
                     labels = c("May", "Jun", "Jul", "Aug", "Sep"))+
  scale_y_continuous(limits = c(-3.1, 3),
                     breaks = c(-3, -2, -1, 0, 1, 2, 3))

plot(maxT_plotz)


sm2_plotz <- ggplot(new, aes(x = doy, y = mean_sm2, group = year_group, colour = year_group, linetype = year_group)) +
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
                     labels = c("May", "Jun", "Jul", "Aug", "Sep")) +
  scale_y_continuous(limits = c(-3.1, 3),
                     breaks = c(-3, -2, -1, 0, 1, 2, 3))

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
                     labels = c("May", "Jun", "Jul", "Aug", "Sep")) +
  scale_y_continuous(limits = c(-3.1, 3),
                     breaks = c(-3, -2, -1, 0, 1, 2, 3))

plot(sm3_plotz)


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
                     labels = c("May", "Jun", "Jul", "Aug", "Sep")) +
  scale_y_continuous(limits = c(-3.1, 3),
                     breaks = c(-3, -2, -1, 0, 1, 2, 3))
plot(swr_plotz)

timeseries_rq2_plots <- grid.arrange(
  maxT_plotz, swr_plotz,
  sm2_plotz, sm3_plotz, 
  ncol = 2, 
  layout_matrix = rbind(c(1,2), c(3, 4)), 
  heights = c(1,1))


ggsave("DriversTimeseries_plots.png", path = "Plots", plot = timeseries_rq2_plots, width = 8, height = 6, dpi = 500)
ggsave("GPPz-scores.png", path = "Plots", plot = gpp_plotz, width = 5, height = 5, dpi = 300)



# ALL drivers on one plot? ####

new2 <- new %>%
  filter(condition == "Drought")

palette_anomalies_all <- c("#71D673", "#FF9E6E", "#70B3D4", "#4F7BAD", "yellow", "darkgrey")

combined_plotz <- ggplot(new2, aes(x = doy)) +
  geom_line(aes(y = mean_gpp, colour = "GPP"),linewidth = 0.8) +
  geom_line(aes(y = mean_maxT, colour = "MaxT"), linewidth = 0.8) +
  # geom_line(aes(y = vpd_z_scores, colour = "VPD"), linewidth = 0.8) +
  geom_line(aes(y = mean_sm1, colour = "SM1"),linewidth = 0.8) +
  geom_line(aes(y = mean_sm2, colour = "SM2"),linewidth = 0.8) +
  geom_line(aes(y = mean_sm3, colour = "SM3"),linewidth = 0.8) +
  # geom_line(aes(y = mean_swr, colour = "SWR"),linewidth = 0.8) +
  geom_abline(intercept = 0, slope = 0, color = "black", linetype = "dashed") +
  labs(title = "",
       x = "Time (months)",
       y = "Z-score",
       colour = "Year:") +
  scale_colour_manual(values = palette_anomalies_all) +
  #scale_shape_manual() + 
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(126, 154, 182, 217, 252), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep")) +
  scale_y_continuous(limits = c(-3, 2.5),
                     breaks = c(-3, -2, -1, 0, 1, 2, 3))

plot(combined_plotz)
ggsave("combined_anomalies.png", path = "Plots", plot = drought_anomaly_plots, width = 10, height = 4, dpi = 500)

# do the same for individual droughts
new2003 <- new %>%
  filter(year == 2003)

palette_anomalies2003 <- c("#43CC68", "#F27C6A", "#70B3D4", "#4F7BAD", "yellow", "darkgrey")
palette_anomalies2003 <- c("#FF9500", "#FFC1BF", "#8ACCD1",  "#F0E487", "yellow", "darkgrey")

combined_plot2003 <- ggplot(new2003, aes(x = doy)) +
  geom_line(aes(y = gpp_z_scores, colour = "GPP"),linewidth = 1) +
  geom_line(aes(y = temp_z_scores, colour = "MaxT"), linewidth = 0.8) +
  # geom_line(aes(y = sm1_z_scores, colour = "SM1"),linewidth = 0.8) +
  # geom_line(aes(y = vpd_z_scores, colour = "VPD"), linewidth = 0.8) +
  # geom_line(aes(y = sm2_z_scores, colour = "SM2"),linewidth = 0.8) +
  geom_line(aes(y = sm3_z_scores, colour = "SM3"),linewidth = 0.8) +
  # geom_line(aes(y = swr_z_scores, colour = "SWR"),linewidth = 0.8) +
  geom_abline(intercept = 0, slope = 0, color = "black", linewidth = 0.3) +
  labs(title = "",
       x = "Time (months)",
       y = "Z-score",
       colour = "Variable:") +
  scale_colour_manual(values = palette_anomalies2003) +
  #scale_shape_manual() + 
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold", ),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(126, 154, 182, 217, 252), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep")) +
  scale_y_continuous(limits = c(-3, 3),
                     breaks = c(-3, -2, -1, 0, 1, 2, 3))

plot(combined_plot2003)


new2018 <- new %>%
  filter(year == 2018)

palette_anomalies2018 <- c("#FF9500", "#FFC1BF", "#8ACCD1", "#3B878C", "#F0E487", "yellow", "darkgrey")


combined_plot2018 <- ggplot(new2018, aes(x = doy)) +
  geom_line(aes(y = gpp_z_scores, colour = "GPP"),linewidth = 1) +
  geom_line(aes(y = temp_z_scores, colour = "MaxT"), linewidth = 0.8) +
  # geom_line(aes(y = sm1_z_scores, colour = "SM1"),linewidth = 0.8) +
  # geom_line(aes(y = vpd_z_scores, colour = "VPD"), linewidth = 0.8) +
  # geom_line(aes(y = sm2_z_scores, colour = "SM2"),linewidth = 0.8) +
  geom_line(aes(y = sm3_z_scores, colour = "SM3"),linewidth = 0.8) +
  # geom_line(aes(y = swr_z_scores, colour = "SWR"),linewidth = 0.8) +
  geom_abline(intercept = 0, slope = 0, color = "black", linewidth = 0.3) +
  labs(title = "",
       x = "Time (months)",
       y = "Z-score",
       colour = "Variable:") +
  scale_colour_manual(values = palette_anomalies2018) +
  #scale_shape_manual() + 
  theme(legend.position = "bottom", panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(size=12, hjust=0.5),
        axis.title = element_text(size=11),
        axis.text = element_text(size=9),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(126, 154, 182, 217, 252), 
                     labels = c("May", "Jun", "Jul", "Aug", "Sep")) +
  scale_y_continuous(limits = c(-3.1, 3),
                     breaks = c(-3, -2, -1, 0, 1, 2, 3))

plot(combined_plot2018)

RQ2zscore_timeseriesALL <- grid.arrange(
  combined_plot2003, combined_plot2018,
  ncol = 2, 
  nrow = 1,
  layout_matrix = rbind(c(1,2)), 
  heights = c(1))

ggsave("AllZ-scores.png", path = "Plots", plot = RQ2zscore_timeseriesALL, width = 9, height = 4, dpi = 500)







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
plot(gpp_plotz)

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


