library(sf); library(tidyverse); library(here); library(RColorBrewer)
# Before other things, compute expected classification errors
# Requires these specially formatted .csvs in online error calculator
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
alternatives <- 20; idx <- ncol(ethnBg)
for(i in 1:alternatives){
  ethnBg[i + idx] <- 0
  colnames(ethnBg)[i + idx] <- paste0("s_", i)
  for (row in 1:nrow(ethnBg)) {
    normval <- rnorm(1, ethnBg$pct[[row]], ethnBg$sd[[row]])
    ethnBg[row, i + idx] <- ifelse(is.nan(normval), NA, normval)
  }
}

# Display random outcomes with same no classes and class scheme
rand <- ethnBg %>%
  select(pct, starts_with("s_")) %>%
  mutate_at(vars(s_1:s_20), funs(replace(., . < 0, 0))) %>%
  mutate_at(vars(s_1:s_20), funs(replace(., . > 100, 100)))
idx <- ncol(rand) - 1

for (i in 2:idx){
  png(here("figures", paste0("rand_", i - 1, ".png")), width = 10, height = 7.5, units = "in", res = 500)
  plot(rand[i], breaks = "quantile", nbreaks = 5,
       pal = brewer.pal(5, "PuBu"),
       border = NA, main = NULL, reset = FALSE)
  plot(st_geometry(ethnCty), col = NA, border = "dimgray", add = TRUE)
  dev.off()
}

# Number of classes
# Overall = overall map error for the scheme
# Max class = maximum expected classification error for a single class
error_class <- c(#"0.6% Overall Map Error, 5.5% Max. Class Error",
                 "1.7% Overall Map Error, 20.3% Max. Class Error",
                 "2.9% Overall Map Error, 21.5% Max. Class Error",
                 "4.5% Overall Map Error, 29.8% Max. Class Error",
                 "6.2% Overall Map Error, 33.2% Max. Class Error",
                 "8.2% Overall Map Error, 40.4% Max. Class Error")
for (j in 3:7){
  # We use j + 1 because seq includes minimum and maximum
  eBreaks <- seq(min(ethnTrct$pct, na.rm = TRUE),
                 max(ethnTrct$pct, na.rm = TRUE),
                 length.out = j + 1)
  png(here("figures", paste0("cn_", j, ".png")), width = 10, height = 7.5, units = "in", res = 500)
  plot(ethnTrct["pct"], breaks = eBreaks,
       pal = brewer.pal(j, "PuBu"),
       border = NA, main = error_class[j - 2], reset = FALSE)
  plot(st_geometry(ethnCty), col = NA, border = "dimgray", add = TRUE)
  dev.off()
}

# Classification scheme
error_class <- c("17.3% Overall Map Error, 31.2% Max. Class Error",
                 "37.2% Overall Map Error, 57.9% Max. Class Error",
                 "4.5% Overall Map Error, 29.8% Max. Class Error")
png(here("figures", "cb_jenks.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(ethnTrct["pct"], breaks = "jenks", nbreaks = 5,
     pal = brewer.pal(5, "PuBu"),
     border = NA, main = error_class[1], reset = FALSE)
plot(st_geometry(ethnCty), col = NA, border = "dimgray", add = TRUE)
dev.off()
png(here("figures", "cb_quantile.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(ethnTrct["pct"], breaks = "quantile", nbreaks = 5,
     pal = brewer.pal(5, "PuBu"),
     border = NA, main = error_class[2], reset = FALSE)
plot(st_geometry(ethnCty), col = NA, border = "dimgray", add = TRUE)
dev.off()
png(here("figures", "cb_equal.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(ethnTrct["pct"], breaks = "equal", nbreaks = 5,
     pal = brewer.pal(5, "PuBu"),
     border = NA, main = error_class[3], reset = FALSE)
plot(st_geometry(ethnCty), col = NA, border = "dimgray", add = TRUE)
dev.off()

# Geography selected
error_class <- c("4.5% Overall Map Error, 29.8% Max. Class Error",
                 "10.9% Overall Map Error, 49.8% Max. Class Error")
png(here("figures", "geo_trct.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(ethnTrct["pct"], breaks = "equal", nbreaks = 5,
     pal = brewer.pal(5, "PuBu"),
     border = NA, main = error_class[1], reset = FALSE)
plot(st_geometry(ethnCty), col = NA, border = "dimgray", add = TRUE)
dev.off()
png(here("figures", "geo_bg.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(ethnBg["pct"], breaks = "equal", nbreaks = 5,
     pal = brewer.pal(5, "PuBu"),
     border = NA, main = error_class[2], reset = FALSE)
plot(st_geometry(ethnCty), col = NA, border = "dimgray", add = TRUE)
dev.off()

# Bonus for example purposes: NYC Neighborhood Tabulation Areas
# Download and unzip NTA file
# url <- "https://data.cityofnewyork.us/api/geospatial/d3qk-pfyz?method=export&format=Shapefile"
# download.file(url, here("downloads", "nta.zip"), mode = "wb")
# unzip(here("downloads", "nta.zip"), exdir = here("downloads"))
# Find NTA filename (changes with each DL)
file_id <- paste0("./", list.files(here("downloads"), pattern = "*.shp"))
nyc_nta <- st_read(here("downloads", file_id))

cust_pal <- palette(rep_len(c("#F3EEF3", "#BDC9DE", "#70A8D1", "#2B8DBC", "#04598F"), 195))

png(here("figures", "nyc_nta.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(nyc_nta["ntaname"],
     pal = cust_pal,
     border = "white", main = NULL)
dev.off()
