library(sf); library(tidyverse); library(here); library(RColorBrewer);library(rgdal)
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
