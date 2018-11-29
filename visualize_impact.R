# Before other things, compute expected classification errors
library(sf); library(tidyverse); library(here)
trctVals <- read.csv(here("outputs", "ethn_trct.csv")) %>%
  select(pct, moe) %>%
  mutate(pct = pct * 100) %>%
  drop_na()
write.csv(trctVals, here("outputs", "ethn_trct_Autoreporter.csv"), row.names = FALSE)
bgVals <- read.csv(here("outputs", "ethn_bg.csv")) %>%
  select(pct, moe) %>%
  mutate(pct = pct * 100) %>%
  drop_na()
write.csv(bgVals, here("outputs", "ethn_bg_Autoreporter.csv"), row.names = FALSE)

# Now, for making visuals
ethnBg <- st_read(here("outputs", "./ethn_orig_bg.shp")) %>%
  mutate_at(c("GEOID"), as.character) %>%
  st_set_crs(4326) %>%
  st_transform(26918) %>%
  mutate(pct = pct * 100,
         sd = moe / 1.645)
ethnTrct <- st_read(here("outputs", "./ethn_orig_trct.shp")) %>%
  mutate_at(c("GEOID"), as.character) %>%
  st_set_crs(4326) %>%
  st_transform(26918) %>%
  mutate(pct = pct * 100,
         sd = moe / 1.645)
ethnCty <- st_read(here("outputs", "./ethn_orig_cty.shp")) %>%
  mutate_at(c("GEOID"), as.character) %>%
  st_set_crs(4326) %>%
  st_transform(26918) %>%
  mutate(pct = pct * 100,
         sd = moe / 1.645)

# Generate random outcomes based on estimate and MOE
alternatives <- 20; idx <- ncol(ethnTrct)
for(i in 1:alternatives){
  ethnTrct[i + idx] <- 0
  colnames(ethnTrct)[i + idx] <- paste0("s_", i)
  for (row in 1:nrow(ethnTrct)) {
    ethnTrct[row, i + idx] <- rnorm(1, ethnTrct$pct[[row]], ethnTrct$sd[[row]])
  }
}

# Display random outcomes with same no classes and class scheme
rand <- ethnTrct %>%
  select(pct, starts_with("s_")) %>%
  mutate_at(vars(s_1:s_20), funs(replace(., . < 0, 0))) %>%
  mutate_at(vars(s_1:s_20), funs(replace(., . > max(pct), max(pct)))) %>%
  drop_na()
idx <- ncol(rand) - 1
eBreaks <- seq(min(rand$pct), max(rand$pct), length.out = 5)
eBreaks[1] <- 0; eBreaks[5] <- 100
for (i in 2:idx){
  png(here("figures", paste0("rand_", i - 1, ".png")), width = 10, height = 7.5, units = "in", res = 500)
  plot(rand[i], breaks = eBreaks, border = NA, main = NULL)
  dev.off()
}

# Number of classes
# Overall = overall map error for the scheme
# Max class = maximum expected classification error for a single class
error_class <- c("0.6% Overall, 5.5% Max Class",
                 "1.7% Overall, 20.3% Max Class",
                 "2.9% Overall, 21.5% Max Class",
                 "4.5% Overall, 29.8% Max Class",
                 "6.2% Overall, 33.2% Max Class",
                 "8.2% Overall, 40.4% Max Class")
for (j in 2:7){
  # We use j + 1 because seq includes minimum and maximum
  eBreaks <- seq(min(ethnTrct$pct, na.rm = TRUE),
                 max(ethnTrct$pct, na.rm = TRUE),
                 length.out = j + 1)
  png(here("figures", paste0("cn_", j - 1, ".png")), width = 10, height = 7.5, units = "in", res = 500)
  plot(ethnTrct["pct"], breaks = eBreaks, border = NA, main = error_class[j - 1])
  dev.off()
}

# Classification scheme
error_class <- c("17.3% Overall, 31.2% Max Class",
                 "37.2% Overall, 57.9% Max Class",
                 "4.5% Overall, 29.8% Max Class")
png(here("figures", "cb_jenks.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(ethnTrct["pct"], breaks = "jenks", nbreaks = 5, border = NA, main = error_class[1])
dev.off()
png(here("figures", "cb_quantile.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(ethnTrct["pct"], breaks = "quantile", nbreaks = 5, border = NA, main = error_class[2])
dev.off()
png(here("figures", "cb_equal.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(ethnTrct["pct"], breaks = "equal", nbreaks = 5, border = NA, main = error_class[3])
dev.off()

# Geography selected
error_class <- c("4.5% Overall, 29.8% Max Class",
                 "10.9% Overall, 49.8% Max Class")
png(here("figures", "geo_trct.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(ethnTrct["pct"], breaks = "equal", nbreaks = 5, border = NA, main = error_class[1])
dev.off()
png(here("figures", "geo_bg.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(ethnBg["pct"], breaks = "equal", nbreaks = 5, border = NA, main = error_class[2])
dev.off()
