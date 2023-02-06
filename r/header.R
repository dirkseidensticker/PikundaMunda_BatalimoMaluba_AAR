# SETUP ----
library(c14bazAAR)
library(concaveman)
library(data.table)
library(dplyr)
library(elevatr)
library(FactoMineR)
library(factoextra)
library(geojsonsf)
library(ggplot2)
library(ggridges)
library(ggrepel)
library(ggnewscale)
library(cowplot)
library(ggthemes)
library(ggsn)
library(gridExtra)
library(ggrepel)
#library(oxcAAR)
#quickSetupOxcal()
library(cowplot)
library(dplyr)
library(osmdata)
library(osmextract)
library(osmplotr)
library(raster)
library(reshape2)
library(rnaturalearth)
library(tidyr)
library(sf)
library(svglite)
library(geojsonsf)
library(viridis)
library(xlsx)

# rcarbon ####
library(rcarbon)
library(parallel)
ncores = (detectCores() - 1)

# radiocarbon database
c14 <- rbind(
  data.table::fread(
    "https://raw.githubusercontent.com/dirkseidensticker/aDRAC/master/aDRAC.csv", 
    encoding = "UTF-8"),
  data.table::fread(
    "C:/Users/dirks/OneDrive/Programmieren/aDRACv2_unpubl.csv", 
    dec = ",", 
    encoding = "UTF-8")
) %>%
  sf::st_as_sf(
    coords = c("LONG", "LAT"),
    crs = 4326,
    remove = F,
    na.fail = F
  )

# Spatial data ----
f <- geojsonsf::geojson_sf("data/gis/regions.geojson")


pottery <- read.csv(
  "https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/potterygroups.csv",
  encoding = "UTF-8")
# styleschrono$POSc <- as.character(styleschrono$POS)
pottery$FROM <- as.numeric(pottery$FROM)
pottery$TO <- as.numeric(pottery$TO)

sites <- data.table::fread(
  "https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/sites.csv",
  encoding = "UTF-8")  %>%
  st_as_sf(crs = 4326, 
           coords = c("LONG", 
                      "LAT"), 
           remove = FALSE, 
           na.fail = F)

