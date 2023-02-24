# Map

source("r/header.R")
source("r/myfct.R")

bb <- c(xmin = 14, xmax = 26, ymin = -6, ymax = 6)

# Vector layers ----
ocean10 <- rnaturalearth::ne_download(scale = 10, type = "ocean", category = "physical", returnclass="sf")
land10 <- rnaturalearth::ne_download(scale = 10, type = "land", category = "physical", returnclass="sf") %>% sf::st_crop(bb)
rivers10 <- rnaturalearth::ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass="sf") %>% sf::st_crop(bb)
lakes10 <- rnaturalearth::ne_download(scale = 10, type = "lakes", category = "physical", returnclass="sf") # %>% sf::st_crop(bb)
coast10 <- rnaturalearth::ne_download(scale = 10, type = "coastline", category = "physical", returnclass="sf")
boundary_lines_land10 <- rnaturalearth::ne_download(scale = 10, type = "boundary_lines_land", category = "cultural", returnclass="sf") %>% sf::st_crop(bb)

osm.rivers.lines <- geojsonsf::geojson_sf("data/gis/OSM_river_lines.geojson") %>% sf::st_crop(bb)
osm.rivers.poly <- geojsonsf::geojson_sf("data/gis/OSM_river_lakes_poly.geojson") %>%
  sf::st_make_valid() # %>% sf::st_union() %>% sf::st_crop(bb)
osm.coast.line <- geojsonsf::geojson_sf("data/gis/OSM_coast_lines.geojson") %>% sf::st_crop(bb)

rainforest <- geojsonsf::geojson_sf("data/white1983.geojson") %>%
  st_set_crs(4326) %>%
  dplyr::filter(DESCRIPTIO %in% c("Anthropic landscapes",
                                  "Dry forest and thicket",
                                  "Swamp forest and mangrove",
                                  "Tropical lowland rainforest"))

#refugia <- geojsonsf::geojson_sf("data/Maley2001_7Fig4.geojson")
refugia <- geojsonsf::geojson_sf("data/Bremond_etal2017Fig1.geojson")


# Elevation ----

library(elevatr)

# setting up boundig box
locations <- data.frame(X1 = c(12, 26), 
                        X2 = c(6.5, -6.5))  

# get gem
dem <- elevatr::get_elev_raster(locations = locations, 
                                prj = "EPSG:4326", 
                                z = 4, 
                                clip = "bbox")

# plot(dem)

dem_df <- raster::as.data.frame(dem, xy=TRUE)
names(dem_df) <- c("x", "y", "z")

dem_df <- dplyr::filter(dem_df, z >= 0)

dem_df$z.class <- cut(dem_df$z, c(0, 500, 1000, Inf))

dem.450 <- raster::rasterToContour(dem, levels = c(450)) %>%
  sf::st_as_sf() %>%
  sf::st_cast("LINESTRING") %>%
  sf::st_crop(xmin = 14, xmax = 26, ymin = -5.25, ymax = 6) %>%
  sf::st_cast("POLYGON") %>%
  sf::st_make_valid() %>%
  dplyr::mutate(area = sf::st_area(geometry)) %>%
  dplyr::group_by(level) %>%
  dplyr::filter(area == max(area)) %>%
  dplyr::select(-c(level, area)) %>%
  sf::st_union()

geo <- geojsonsf::geojson_sf("data/geo7_2ag.geojson") %>%
  dplyr::filter(GEO2_7G_ID %in% c(8447, 8633)) %>%
  sf::st_union()

library(smoothr)

cb.comb <- rbind(sf::st_sf(geom = dem.450), 
                 sf::st_sf(geom = geo)) %>%
  sf::st_make_valid() %>%
  sf::st_union() %>%
  sf::st_buffer(dist = .1) %>%
  sf::st_simplify(preserveTopology = FALSE, dTolerance = 1.5) %>%
  smoothr::smooth(method = "ksmooth", smoothness = 100)
#smoothr::smooth(method = "chaikin")

ggplot() + 
  geom_sf(data = dem.450, fill = "blue", alpha = .2) + 
  geom_sf(data = geo, fill = "red", alpha = .2) + 
  geom_sf(data = cb.comb, fill = "yellow", alpha = .2)

# Minimap ----

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

minimap <- ggplot(data = world) +
  geom_sf(color = NA, fill = "grey") + 
  geom_sf(data = rnaturalearth::ne_download(scale = 110, type = "rivers_lake_centerlines", category = "physical", returnclass="sf"), color = "#616161") + 
  geom_rect(xmin = 15.5, xmax = 25, 
            ymin = -4.25, ymax = 5, 
            fill = NA, color = "black") + 
  coord_sf(xlim = c(-15, 50), 
           ylim = c(-35, 35)) + 
  theme_void() + 
  theme(panel.border = element_rect(colour = "darkgrey", 
                                    fill = NA, size = .5), 
        panel.background = element_rect(fill = "white"))


sites.text <- dplyr::filter(sites %>% dplyr::distinct(SITE, LAT, LONG), SITE %in% c(
  "Imbonga",
  "Longa",
  "Pikunda",
  "Munda",
  "Ngombe",
  "Mitula",
  "Mobaka",
  "Batalimo",
  "Maluba", 
  "Motenge-Boma",
  "Bomane Yangwa"
))
sites.text

# Map ----

p <- ggplot() + 
  geom_tile(data = dem_df, aes(x = x, y = y, fill = z.class), color = NA) + 
  scale_fill_grey(start = 1, end = .75, na.value = 0) +
  ggnewscale::new_scale_fill() + 
  geom_sf(data = ocean10, fill = "#dff1f9", color = NA) + 
  geom_sf(data = sf::st_union(rainforest), 
          fill = "#73a788", color = NA) + 
  #geom_sf(data = sf::st_union(rainforest) %>% st_crop(xmin = 12.5, xmax = 25.5, ymin = -6.5, ymax = 6.5), 
  #        fill = "#00734d", color = NA, alpha = .4) + 
  geom_sf(data = refugia, fill = "#478966", color = NA) + 
  #geom_text(aes(x = 22, y = 0), label = "?", size = 10, color = "#248823") + 
  #geom_sf(data = coast10, size = .5, color = '#44afe3') + 
  geom_sf(data = osm.rivers.lines, size = .5, color = '#44afe3') + 
  geom_sf(data = osm.rivers.poly, size = .5, fill = '#44afe3', color = '#44afe3') + 
  geom_sf(data = lakes10, fill = '#44afe3', color = NA) + 
  geom_sf(data = boundary_lines_land10, size = .2, color = 'white', linetype = "dashed") + 
  geom_sf(data = cb.comb, fill = NA, color = "#782172", linetype = "dashed", linewidth = .5) + 
  geom_point(data = sites, aes(x = LONG, y = LAT), shape = 21, fill = "white", color = "black", size = 2) + 
  geom_label_repel(data = sites.text, 
                   aes(x = LONG, y = LAT, label = SITE), 
                   size = 2.5, 
                   label.padding = 0.1, min.segment.length = 0, 
                   fill = "black", color = "white") + 
  geom_point(data = sites.text, aes(x = LONG, y = LAT), shape = 21, fill = "black", color = "white", size = 3) + 

  shadowtext::geom_shadowtext(aes(x = 16, y = 0), label = "Western\nCongo\nBasin", fontface  = "bold", colour = "white", size = 3) + 
  shadowtext::geom_shadowtext(aes(x = 19.75, y = 4), label = "Northern\nCongo\nBasin", fontface  = "bold", colour = "white", size = 3) + 
  shadowtext::geom_shadowtext(aes(x = 20.1, y = .5), label = "Inner\nCongo\nBasin", fontface  = "bold", colour = "white", size = 3) + 
  shadowtext::geom_shadowtext(aes(x = 24.5, y = 0), label = "Eastern\nCongo\nBasin", fontface  = "bold", colour = "white", size = 3) + 
  
  # COUNTRY NAMES
  annotate("text", x = 22, y = 5.2, label = "Central Africa Rep.", fontface  = "bold", colour = "#485063") + 
  annotate("text", x = 16, y = -1, label = "Rep. Congo", fontface  = "bold", colour = "#485063") + 
  annotate("text", x = 22.5, y = -2, label = "Dem. Rep. Congo", fontface  = "bold", colour = "#485063") + 
  # RIVER NAMES
  annotate("text", x = 22.25, y = 2.3, label = "CONGO", fontface  = "bold", colour = "white", size = 2) + 
  annotate("text", x = 19.7, y = 5.3, label = "UBANGI", fontface  = "bold", colour = "grey30", size = 2) + 
  annotate("text", x = 17.875, y = 1.8, label = "UBANGI", fontface  = "bold", colour = "white", size = 2, angle = 80) + 
  annotate("text", x = 19, y = 3, label = "LUA", fontface  = "bold", colour = "white", size = 2, angle =  15) + 
  annotate("text", x = 23, y = -1.2, label = "TSHUAPA", fontface  = "bold", colour = "white", size = 2, angle = -25) + 
  annotate("text", x = 24, y = 3.85, label = "UELE", fontface  = "bold", colour = "white", size = 2) + 
  annotate("text", x = 19.5, y = -4.2, label = "KASAÏ", fontface  = "bold", colour = "white", size = 2, angle = -20) + 
  annotate("text", x = 16.5, y = 1.5, label = "SANGHA", fontface  = "bold", colour = "white", size = 2, angle = -45) + 
  annotate("text", x = 15.5, y = 2.15, label = "NGOKO", fontface  = "bold", colour = "white", size = 2, angle = -15) + 
  annotate("text", x = 17.15, y = 1.85, label = "LIKWALA-\nAUX-HERBES", fontface  = "bold", colour = "white", size = 2, angle = -80) + 
  annotate("text", x = 15.75, y = 4, label = "KADÉÏ", fontface  = "bold", colour = "white", size = 2, angle = -35) + 
  
  # LEGEND
  geom_rect(aes(xmin = 15.15, xmax = 18, ymin = -3.4, ymax = -4.6), fill = "White", color = "grey") +
  
  geom_segment(aes(x = 15.2, xend = 15.55, y = -3.55, yend = -3.55), 
               color = "#782172", linetype = "dashed") + 
  annotate("text", x = 15.6, y = -3.55, label = paste("Congo Basin"), hjust = 0, size = 2) + 
  geom_rect(aes(xmin = 15.2, xmax = 15.55, ymin = -3.7, ymax = -3.9), fill = "#73a788", color = NA) + 
  annotate("text", x = 15.6, y = -3.8, label = paste("Rainforest"), hjust = 0, size = 2) + 
  geom_rect(aes(xmin = 15.2, xmax = 15.55, ymin = -4, ymax = -4.2), fill = "#478966", color = NA) + 
  annotate("text", x = 15.6, y = -4.1, label = paste("Rainforest Refugia"), hjust = 0, size = 2) + 
  geom_rect(aes(xmin = 15.2, xmax = 15.55, ymin = -4.3, ymax = -4.5), fill = "#e3e3e3", color = NA) + 
  annotate("text", x = 15.6, y = -4.4, label = paste("Topography above 500 m MSL"), hjust = 0, size = 2) + 

  coord_sf(xlim = c(15.5, 25), 
           ylim = c(-4.25, 5), 
           label_graticule = "SE") + 
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(), 
        axis.title = element_blank())

cowplot::ggdraw() +
  draw_plot(p) +
  draw_plot(minimap, 
            x = .74, y = .055, width = .2, height = .2)

ggsave("fig/fig_map.jpg", width = 6.25, height = 6, bg = "white")
ggsave("fig/fig_map.pdf", width = 6.25, height = 6, bg = "white")

