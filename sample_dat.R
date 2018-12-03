# The job of this script is to download necessary data and compute analysis fields.

library(tidycensus); library(tidyverse); library(sf);
library(here); library(tigris); library(magrittr)
options(tigris_use_cache = TRUE)

cty <- get_acs(state = c("PA", "NJ"), geography = "county",
               variables = c("B03003_001", "B03003_003"),
               geometry = TRUE, output = "wide") %>%
  subset(GEOID %in% c("34005", "34007", "34015", "34021",
                      "42017", "42029", "42045", "42091", "42101")) %>%
  mutate(type = "cty") %>%
  replace_na(list(B03003_001M = 0, B03003_003M = 0))
trct <- get_acs(state = c("PA", "NJ"), geography = "tract",
                variables = c("B03003_001", "B03003_003"),
                geometry = TRUE, output = "wide") %>%
  mutate(stcty = substr(GEOID, 1, 5)) %>%
  subset(stcty %in% c("34005", "34007", "34015", "34021",
                      "42017", "42029", "42045", "42091", "42101")) %>%
  select(-stcty) %>%
  mutate(type = "trct")
bgNJ <- get_acs(state = "NJ",
                county = c("005", "007", "015", "021"),
                geography = "block group",
                variables = c("B03003_001", "B03003_003"),
                geometry = TRUE, output = "wide") %>%
  as(., "Spatial")
bgPA <- get_acs(state = "PA",
                county = c("017", "029", "045", "091", "101"),
                geography = "block group",
                variables = c("B03003_001", "B03003_003"),
                geometry = TRUE, output = "wide") %>%
  as(., "Spatial")
bg <- rbind(bgNJ, bgPA) %>%
  st_as_sf(.) %>%
  mutate(type = "bg")

# Round 1: export data before calculating CVs or MOE / estimate
obs <- bind_rows(bg, trct) %>%
  bind_rows(., cty) %>%
  mutate(pct = B03003_003E / B03003_001E, # NOTE this ranges from 0 to 1
         moe = moe_prop(B03003_003E, B03003_001E, B03003_003M, B03003_001M) * 100)

export <- obs %>% group_by(type) %>% do(data = (.)) %>% pull(data)
export_type <- c("bg", "cty", "trct")
for(i in 1:3){
  st_write(export[[i]], here("outputs", paste0("./ethn_orig_", export_type[[i]], ".shp")))
  write.csv(export[[i]] %>% st_set_geometry(NULL), here("outputs", paste0("ethn_orig_", export_type[[i]], ".csv")), row.names = FALSE)
}

obs <- bind_rows(bg, trct) %>%
  bind_rows(., cty) %>%
  mutate(pct = B03003_003E / B03003_001E,
         moe = moe_prop(B03003_003E, B03003_001E, B03003_003M, B03003_001M) * 100,
         relative = moe / pct / 100,
         cv = (moe / 1.645) / (pct * 100) * 100) %>% # Technically this and relative will be proportional, c = 1.645
  select(GEOID, type, pct, moe, relative, cv, geometry) %>%
  drop_na()

obs$relative[is.infinite(obs$relative)] <- NA
obs$cv[is.infinite(obs$cv)] <- NA
obs <- na.omit(obs)

result <- obs %>% group_by(type) %>%
  summarize(tot_relative = mean(relative) * 100,
            tot_cv = mean(cv),
            tot_obs = length(type))
# bg: 4073 initially, 3340 final; tract: 1379 initially, 1346 final

# Round 2: Export data with CVs and dropped obs
export <- obs %>% group_by(type) %>% do(data = (.)) %>% pull(data)
for(i in 1:3){
  st_write(export[[i]], here("outputs", paste0("./ethn_", export_type[[i]], ".shp")))
  write.csv(export[[i]] %>% st_set_geometry(NULL), here("outputs", paste0("ethn_", export_type[[i]], ".csv")), row.names = FALSE)
}

# Export population and land area data for further analysis
# No spatial data needed; can join to existing shapefiles
# Total population = B01003_001; unweighted sample count = B00001_001
cty <- get_acs(state = c("PA", "NJ"), geography = "county",
               variables = c("B01003_001", "B00001_001"),
               geometry = FALSE, output = "wide") %>%
  subset(GEOID %in% c("34005", "34007", "34015", "34021",
                      "42017", "42029", "42045", "42091", "42101")) %>%
  mutate(type = "cty") %>%
  select(-NAME) %>%
  replace_na(list(B01003_001M = 0, B25001_001M = 0))
trct <- get_acs(state = c("PA", "NJ"), geography = "tract",
                variables = c("B01003_001", "B00001_001"),
                geometry = FALSE, output = "wide") %>%
  mutate(stcty = substr(GEOID, 1, 5)) %>%
  subset(stcty %in% c("34005", "34007", "34015", "34021",
                      "42017", "42029", "42045", "42091", "42101")) %>%
  select(-stcty, -NAME) %>%
  mutate(type = "trct")
bgNJ <- get_acs(state = "NJ",
                county = c("005", "007", "015", "021"),
                geography = "block group",
                variables = c("B01003_001", "B00001_001"),
                geometry = FALSE, output = "wide")
bgPA <- get_acs(state = "PA",
                county = c("017", "029", "045", "091", "101"),
                geography = "block group",
                variables = c("B01003_001", "B00001_001"),
                geometry = FALSE, output = "wide")
bg <- rbind(bgNJ, bgPA) %>%
  mutate(type = "bg") %>%
  select(-NAME)
aux <- bind_rows(bg, trct) %>%
  bind_rows(., cty)

ctyArea <- counties(state = c("PA", "NJ")) %>%
  st_as_sf(.) %>%
  st_set_geometry(NULL) %>%
  subset(GEOID %in% c("34005", "34007", "34015", "34021",
                      "42017", "42029", "42045", "42091", "42101")) %>%
  mutate_at(c("ALAND"), as.numeric) %>%
  mutate(landArea = ALAND * 0.00000038610) %>%
  select(GEOID, landArea)
trctAreaNJ <- tracts(state = c("NJ")) %>%
  st_as_sf(.) %>%
  st_set_geometry(NULL) %>%
  mutate(stcty = substr(GEOID, 1, 5)) %>%
  subset(stcty %in% c("34005", "34007", "34015", "34021")) %>%
  mutate_at(c("ALAND"), as.numeric) %>%
  mutate(type = "trct", landArea = ALAND * 0.00000038610) %>%
  select(GEOID, landArea)
trctAreaPA <- tracts(state = c("PA")) %>%
  st_as_sf(.) %>%
  st_set_geometry(NULL) %>%
  mutate(stcty = substr(GEOID, 1, 5)) %>%
  subset(stcty %in% c("42017", "42029", "42045", "42091", "42101")) %>%
  mutate_at(c("ALAND"), as.numeric) %>%
  mutate(type = "trct", landArea = ALAND * 0.00000038610) %>%
  select(GEOID, landArea)
trctArea <- rbind(trctAreaNJ, trctAreaPA)
bgAreaNJ <- block_groups(state = c("NJ")) %>%
  st_as_sf(.) %>%
  st_set_geometry(NULL) %>%
  mutate(stcty = substr(GEOID, 1, 5)) %>%
  subset(stcty %in% c("34005", "34007", "34015", "34021")) %>%
  mutate_at(c("ALAND"), as.numeric) %>%
  mutate(type = "bg", landArea = ALAND * 0.00000038610) %>%
  select(GEOID, landArea)
bgAreaPA <- block_groups(state = c("PA")) %>%
  st_as_sf(.) %>%
  st_set_geometry(NULL) %>%
  mutate(stcty = substr(GEOID, 1, 5)) %>%
  subset(stcty %in% c("42017", "42029", "42045", "42091", "42101")) %>%
  mutate_at(c("ALAND"), as.numeric) %>%
  mutate(type = "bg", landArea = ALAND * 0.00000038610) %>%
  select(GEOID, landArea)
bgArea <- rbind(bgAreaNJ, bgAreaPA)
auxArea <- bind_rows(bgArea, trctArea) %>%
  bind_rows(., ctyArea)

aux %<>% left_join(., auxArea, by = "GEOID") %>%
  mutate(popDens = B01003_001E / landArea,
         popSamp = B00001_001E / B01003_001E * 100)

export <- aux %>% group_by(type) %>% do(data = (.)) %>% pull(data)
for(i in 1:3){
  write.csv(export[[i]], here("outputs", paste0("aux_", export_type[[i]], ".csv")), row.names = FALSE)
}
