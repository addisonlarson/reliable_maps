library(tidyverse); library(sf); library(here); library(magrittr)
# Does reliability change according to population, sample rate, distance from city center?

ethnTrct <- st_read(here("outputs", "./ethn_trct.shp")) %>%
  mutate_at(c("GEOID"), as.character) %>%
  st_set_crs(4326) %>%
  st_transform(26918)
ethnAux <- read.csv(here("outputs", "aux_trct.csv")) %>%
  mutate_at(c("GEOID"), as.character) %>%
  select(-type)
ethnTrct %<>% dplyr::left_join(., ethnAux, by = "GEOID")

ethnCorr <- ethnTrct %>%
  select(pct, relative, cv, landArea, popDens, popSamp, B01003_001E) %>%
  st_set_geometry(NULL)
round(cor(ethnCorr, use = "pairwise.complete.obs"),3)

sampBreaks <- quantile(ethnTrct$popSamp, probs = seq(0, 1, 0.25), na.rm = TRUE)

# See http://help.arcgis.com/en/businessanalyst/apis/rest/reference/ACSVariables.html
# and https://svi.cdc.gov/Documents/Publications/CDC_ATSDR_SVI_Materials/SampleSizeError_v2.pdf
# for reliability criteria
ethnTrct %<>%
  mutate(popCat = case_when(B01003_001E < 1000 ~ "Fewer than 1,000",
                            B01003_001E >= 1000 & B01003_001E < 3000 ~ "1,000-2,999",
                            B01003_001E >= 3000 & B01003_001E < 5000 ~ "3,000-4,999",
                            B01003_001E >= 5000 & B01003_001E < 7000 ~ "5,000-6,999",
                            B01003_001E >= 7000 ~ "7,000 or greater"),
         popSampCat = case_when(popSamp < sampBreaks[2] ~ "Bottom Quartile",
                                popSamp >= sampBreaks[2] & popSamp < sampBreaks[3] ~ "Second Quartile",
                                popSamp >= sampBreaks[3] & popSamp < sampBreaks[4] ~ "Third Quartile",
                                popSamp >= sampBreaks[4] ~ "Top Quartile"),
         cvCat = case_when(cv <= 12 ~ "H",
                           cv > 12 & cv <= 40 ~ "M",
                           cv > 40 ~ "L")) %>%
  mutate_at(c("cvCat"), as.factor)
levels(ethnTrct$cvCat) <- ethnTrct$cvCat %>% fct_relevel("H", "M", "L") %>% levels()

# Smaller pop have less reliability
ethnTrct %>% group_by(popCat) %>%
  summarize(cvMean = mean(cv, na.rm = TRUE),
            cvMedian = median(cv, na.rm = TRUE))
# In this case, sample rate doesn't make much of a difference
ethnTrct %>% group_by(popSampCat) %>%
  summarize(cvMean = mean(cv, na.rm = TRUE),
            cvMedian = median(cv, na.rm = TRUE))
# But the percentage of Hispanic residents in the tract *does* make a difference
png(here("figures", "hisp_cvCat.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(ethnTrct["cvCat"], border = NA, main = NULL)
dev.off()
png(here("figures", "hisp_pct.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(ethnTrct["pct"], border = NA, main = NULL)
dev.off()
cor(ethnTrct$pct, ethnTrct$cv)
png(here("figures", "hisp_cvRel.png"), width = 10, height = 7.5, units = "in", res = 500)
ggplot(ethnTrct, aes(x = pct, y = cv)) +
  geom_point(color = "gray") +
  geom_smooth(color = "darkblue") +
  labs(title = "Relationship of CV and Percentage Estimate by Tract",
       x = "Pct. Estimate, Hispanic Residents",
       y = "Pct. CV") +
  theme_minimal()
dev.off()
