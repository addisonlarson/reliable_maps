library(sf); library(tidyverse); library(here); library(RColorBrewer)
# Randomly select 2, 2, and 3
# From bottom, middle, and top tiers of equal interval choro
# ethnTrct <- read.csv(here("outputs", "ethn_orig_trct.csv")) %>%
#   mutate_at(c("GEOID"), as.character) %>%
#   mutate(pct = pct * 100,
#          sd = moe / 1.645,
#          cty = substr(GEOID, 3, 5),
#          plus = pct + moe,
#          minus = pct - moe) %>%
#   filter(cty == "101")
# bottom <- ethnTrct %>%
#   filter(pct <= 100 / 3) %>%
#   sample_n(., 2, replace = FALSE)
# middle <- ethnTrct %>%
#   filter(pct > 100 / 3 & pct <= 200 / 3) %>%
#   sample_n(., 2, replace = FALSE)
# top <- ethnTrct %>%
#   filter(pct > 200 / 3) %>%
#   sample_n(., 3, replace = FALSE)

# I like these estimates, so I'm saving them
# write.csv(bottom, here("outputs", "ex_b.csv"), row.names = FALSE)
# write.csv(middle, here("outputs", "ex_m.csv"), row.names = FALSE)
# write.csv(top, here("outputs", "ex_t.csv"), row.names = FALSE)

bottom <- read.csv(here("outputs", "ex_b.csv"))
middle <- read.csv(here("outputs", "ex_m.csv"))
top <- read.csv(here("outputs", "ex_t.csv"))

# Read geo file to make a single map
ethnTrct <- st_read(here("outputs", "./ethn_orig_trct.shp")) %>%
  mutate_at(c("GEOID"), as.character) %>%
  st_set_crs(4326) %>%
  st_transform(26918) %>%
  mutate(cty = substr(GEOID, 3, 5)) %>%
  subset(cty == 101)
ethnCty <- st_read(here("outputs", "./ethn_orig_cty.shp")) %>%
  mutate_at(c("GEOID"), as.character) %>%
  st_set_crs(4326) %>%
  st_transform(26918) %>%
  mutate(cty = substr(GEOID, 3, 5)) %>%
  subset(cty == 101)

# Define plot colors
primary = "#73A8D0"
secondary = "#2B8DBE"
tertiary = "#04598F"
light = "#F1EEF5" # (Only used as map background)

# Single estimate
p1 <- ggplot(data = data.frame(x = c(-10, 110)), aes(x)) +
  stat_function(fun = dnorm, n = 101, color = primary,
                args = list(mean = top[1,]$pct, sd = top[1,]$sd)) + ylab("") +
  scale_y_continuous(breaks = NULL) +
  ylab("Probability") + xlab("Potential Value of Estimate") +
  ggtitle("Range of Possible Values for One Census Tract") +
  theme_minimal()

# Show location of estimate
est <- ethnTrct %>% subset(GEOID == top[1,]$GEOID)
png(here("figures", "p1_location.png"), width = 5, height = 5, units = "in", res = 500)
plot(st_geometry(ethnTrct), col = light, border = NA, primary = NULL)
plot(st_geometry(est), col = secondary, border = NA, add = TRUE)
plot(st_geometry(ethnCty), col = NA, border = "dimgray", add = TRUE)
dev.off()

# Single estimate with class boundary
p2 <- ggplot(data = data.frame(x = c(-10, 110)), aes(x)) +
  stat_function(fun = dnorm, n = 101, color = primary,
                args = list(mean = top[1,]$pct, sd = top[1,]$sd)) + ylab("") +
  scale_y_continuous(breaks = NULL) +
  annotate("segment", x = 200 / 3, xend = 200 / 3, y = 0.1, yend = 0, color = "gray") +
  ylab("Probability") + xlab("Potential Value of Estimate") +
  ggtitle("Range of Possible Values for One Census Tract") +
  theme_minimal()

# Single estimate with classification error
p3 <- ggplot(data = data.frame(x = c(-10, 110)), aes(x)) +
  stat_function(fun = dnorm, n = 101, color = primary,
                args = list(mean = top[1,]$pct, sd = top[1,]$sd)) +
  stat_function(fun = dnorm, n = 101, fill = primary, alpha = 0.3,
                args = list(mean = top[1,]$pct, sd = top[1,]$sd),
                xlim = c(0, 200 / 3),
                geom = "area") +
  ylab("Probability") + xlab("Potential Value of Estimate") +
  ggtitle("Range of Possible Values for One Census Tract") +
  theme_minimal() +
  annotate("segment", x = 200 / 3, xend = 200 / 3, y = 0.1, yend = 0, color = "gray")

# Three estimates with class boundary
p4 <- ggplot(data = data.frame(x = c(-10, 110)), aes(x)) +
  stat_function(fun = dnorm, n = 101, color = primary,
                args = list(mean = top[1,]$pct, sd = top[1,]$sd)) +
  stat_function(fun = dnorm, n = 101, color = secondary,
                args = list(mean = top[2,]$pct, sd = top[2,]$sd)) +
  stat_function(fun = dnorm, n = 101, color = tertiary,
                args = list(mean = top[3,]$pct, sd = top[3,]$sd)) +
  ylab("Probability") + xlab("Potential Value of Estimate") +
  scale_y_continuous(breaks = NULL) +
  ggtitle("Range of Possible Values for Three Census Tracts") +
  theme_minimal() +
  annotate("segment", x = 200 / 3, xend = 200 / 3, y = 0.1, yend = 0, color = "gray")

# Three estimates with class boundary and classification error
p5 <- ggplot(data = data.frame(x = c(-10, 110)), aes(x)) +
  stat_function(fun = dnorm, n = 101, color = primary,
                args = list(mean = top[1,]$pct, sd = top[1,]$sd)) +
  stat_function(fun = dnorm, n = 101, fill = primary, alpha = 0.3,
                args = list(mean = top[1,]$pct, sd = top[1,]$sd),
                xlim = c(0, 200 / 3),
                geom = "area") +
  stat_function(fun = dnorm, n = 101, color = secondary,
                args = list(mean = top[2,]$pct, sd = top[2,]$sd)) +
  stat_function(fun = dnorm, n = 101, fill = secondary, alpha = 0.3,
                args = list(mean = top[2,]$pct, sd = top[2,]$sd),
                xlim = c(0, 200 / 3),
                geom = "area") +
  stat_function(fun = dnorm, n = 101, color = tertiary,
                args = list(mean = top[3,]$pct, sd = top[3,]$sd)) +
  stat_function(fun = dnorm, n = 101, fill = tertiary, alpha = 0.3,
                args = list(mean = top[3,]$pct, sd = top[3,]$sd),
                xlim = c(0, 200 / 3),
                geom = "area") +
  ylab("Probability") + xlab("Potential Value of Estimate") +
  scale_y_continuous(breaks = NULL) +
  ggtitle("Range of Possible Values for Three Census Tracts") +
  theme_minimal() +
  annotate("segment", x = 200 / 3, xend = 200 / 3, y = 0.1, yend = 0, color = "gray")

# All estimates with boundaries and classification error
# Two estimates with class boundary and classification error
p6 <- ggplot(data = data.frame(x = c(-10, 110)), aes(x)) +
  annotate("segment", x = 200 / 3, xend = 200 / 3, y = 0.18, yend = 0, color = "gray") +
  annotate("segment", x = 100 / 3, xend = 100 / 3, y = 0.18, yend = 0, color = "gray") +
  stat_function(fun = dnorm, n = 101, color = primary,
                args = list(mean = top[1,]$pct, sd = top[1,]$sd)) +
  stat_function(fun = dnorm, n = 101, fill = primary, alpha = 0.5,
                args = list(mean = top[1,]$pct, sd = top[1,]$sd),
                xlim = c(0, 200 / 3),
                geom = "area") +
  stat_function(fun = dnorm, n = 101, color = primary,
                args = list(mean = top[2,]$pct, sd = top[2,]$sd)) +
  stat_function(fun = dnorm, n = 101, fill = primary, alpha = 0.5,
                args = list(mean = top[2,]$pct, sd = top[2,]$sd),
                xlim = c(0, 200 / 3),
                geom = "area") +
  stat_function(fun = dnorm, n = 101, color = primary,
                args = list(mean = top[3,]$pct, sd = top[3,]$sd)) +
  stat_function(fun = dnorm, n = 101, fill = primary, alpha = 0.5,
                args = list(mean = top[3,]$pct, sd = top[3,]$sd),
                xlim = c(0, 200 / 3),
                geom = "area") +
  stat_function(fun = dnorm, n = 101, color = tertiary,
                args = list(mean = middle[1,]$pct, sd = middle[1,]$sd)) +
  stat_function(fun = dnorm, n = 101, fill = tertiary, alpha = 0.5,
                args = list(mean = middle[1,]$pct, sd = middle[1,]$sd),
                xlim = c(0, 100 / 3),
                geom = "area") +
  stat_function(fun = dnorm, n = 101, fill = tertiary, alpha = 0.5,
                args = list(mean = middle[1,]$pct, sd = middle[1,]$sd),
                xlim = c(200 / 3, 100),
                geom = "area") +
  stat_function(fun = dnorm, n = 101, color = tertiary,
                args = list(mean = middle[2,]$pct, sd = middle[2,]$sd)) +
  stat_function(fun = dnorm, n = 101, fill = tertiary, alpha = 0.5,
                args = list(mean = middle[2,]$pct, sd = middle[2,]$sd),
                xlim = c(0, 100 / 3),
                geom = "area") +
  stat_function(fun = dnorm, n = 101, fill = tertiary, alpha = 0.5,
                args = list(mean = middle[2,]$pct, sd = middle[2,]$sd),
                xlim = c(200 / 3, 100),
                geom = "area") +
  stat_function(fun = dnorm, n = 101, color = secondary,
                args = list(mean = bottom[1,]$pct, sd = bottom[1,]$sd)) +
  stat_function(fun = dnorm, n = 101, fill = secondary, alpha = 0.5,
                args = list(mean = bottom[1,]$pct, sd = bottom[1,]$sd),
                xlim = c(100 / 3, 100),
                geom = "area") +
  stat_function(fun = dnorm, n = 101, color = secondary,
                args = list(mean = bottom[2,]$pct, sd = bottom[2,]$sd)) +
  stat_function(fun = dnorm, n = 101, fill = secondary, alpha = 0.5,
                args = list(mean = bottom[2,]$pct, sd = bottom[2,]$sd),
                xlim = c(100 / 3, 100),
                geom = "area") +
  ylab("Probability") + xlab("Potential Value of Estimate") +
  scale_y_continuous(breaks = NULL) +
  ggtitle("Range of Possible Values for All Census Tracts") +
  theme_minimal()

# Compute errors
# Bottom class error
bc_1 <- round(pnorm(100 / 3, bottom[1,]$pct, bottom[1,]$sd, lower.tail = FALSE) * 100, digits = 3)
bc_2 <- round(pnorm(100 / 3, bottom[2,]$pct, bottom[2,]$sd, lower.tail = FALSE) * 100, digits = 3)
bc_mean <- (bc_1 + bc_2) / 2

# Middle class error
mc_1 <- round((pnorm(100 / 3, middle[1,]$pct, middle[1,]$sd) +
                 pnorm(200 / 3, middle[1,]$pct, middle[1,]$sd, lower.tail = FALSE)) * 100, digits = 3)
mc_2 <- round((pnorm(100 / 3, middle[2,]$pct, middle[2,]$sd) +
                 pnorm(200 / 3, middle[2,]$pct, middle[2,]$sd, lower.tail = FALSE)) * 100, digits = 3)
mc_mean <- (mc_1 + mc_2) / 2

# Top class error
tc_1 <- round(pnorm(200 / 3, top[1,]$pct, top[1,]$sd) * 100, digits = 3)
tc_2 <- round(pnorm(200 / 3, top[2,]$pct, top[2,]$sd) * 100, digits = 3)
tc_3 <- round(pnorm(200 / 3, top[3,]$pct, top[3,]$sd) * 100, digits = 3)
tc_mean <- (tc_1 + tc_2 + tc_3) / 3

tot_mean <- round(((bc_mean * 2) + (mc_mean * 2) + (tc_mean * 3)) / 7, digits = 3)

# Export error diagrams
png(here("figures", "p1.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(p1)
dev.off()
png(here("figures", "p2.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(p2)
dev.off()
png(here("figures", "p3.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(p3)
dev.off()
png(here("figures", "p4.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(p4)
dev.off()
png(here("figures", "p5.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(p5)
dev.off()
png(here("figures", "p6.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(p6)
dev.off()
