library(ggplot2)
library(ggthemes)
library(ggrepel)
library(sf)
library(tidyr)

land10 <- rnaturalearth::ne_download(scale = 10, type = 'land', category = 'physical', returnclass = "sf")
coast10 <- rnaturalearth::ne_download(scale = 10, type = 'coastline', category = 'physical', returnclass = "sf")
rivers10 <- rnaturalearth::ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass="sf")
lakes10 <- rnaturalearth::ne_download(scale = 10, type = "lakes", category = "physical", returnclass="sf")
boundary_lines_land10 <- rnaturalearth::ne_download(scale = 10, type = "boundary_lines_land", category = "cultural", returnclass="sf")

sites <- data.table::fread(
  "https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/sites.csv", 
  encoding = "UTF-8") %>%
  sf::st_as_sf(coords = c("LONG", "LAT"), 
               remove = F, 
               crs = 4326, 
               na.fail = F)

pottery <- data.table::fread(
  "https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/potterygroups.csv", 
  encoding = "UTF-8") %>%
  dplyr::select(-ID, -DESCRIPTION)

# Ubangi map

ubangi.buffer <- rivers10 %>%
  dplyr::filter(name == "Ubangi" | name == "Uele") %>%
  sf::st_crop(c(xmin = 16, xmax = 20, ymin = -1, ymax = 6)) %>%
  sf::st_union() %>%
  sf::st_transform(crs = 32733) %>%
  sf::st_buffer(20000) %>%
  sf::st_transform(crs = 4326)

sites.ubangi <- sites %>%
  sf::st_filter(ubangi.buffer)

ggplot() + 
  geom_sf(data = ubangi.buffer) + 
  geom_sf(data = sites.ubangi)

sites.ubangi <- sites.ubangi %>%
  dplyr::left_join(pottery, by = "POTTERY")
sites.ubangi

rainforest <- geojsonsf::geojson_sf("data/white1983.geojson") %>%
  sf::st_set_crs(4326) %>%
  dplyr::filter(DESCRIPTIO %in% c("Anthropic landscapes",
                                  "Dry forest and thicket",
                                  "Swamp forest and mangrove",
                                  "Tropical lowland rainforest"))

refugia <- geojsonsf::geojson_sf("data/Bremond_etal2017Fig1.geojson")

sites.ubangi.stats <- sites.ubangi %>%
  reshape2::dcast(SITE + LAT + LONG ~ POTTERY, fun.aggregate = length)

sites.ubangi.stats.cols <- data.frame(POTTERY = names(sites.ubangi.stats[4:20])) %>%
  dplyr::left_join(pottery %>%
                     dplyr::select(POTTERY, COL), 
                   by = "POTTERY") %>%
  dplyr::pull(COL) 


ggplot() + 
  scatterpie::geom_scatterpie(
    data = sites.ubangi.stats, 
    aes(x = LONG, y = LAT), 
    cols = names(sites.ubangi.stats[4:20])) + 
  scale_fill_manual(values = sites.ubangi.stats.cols) +
  coord_sf()

pottery.ubangi <- sites.ubangi %>%
  dplyr::group_by(POTTERY) %>%
  dplyr::summarise(LAT.min = min(LAT), 
                   LAT.max = max(LAT), 
                   TO = mean(TO), 
                   FROM = mean(FROM), 
                   COL = unique(COL)) %>%
  dplyr::mutate(age.cent = (FROM + TO) / 2, 
                lat.cent = (LAT.min + LAT.max) / 2)

plt.map <- ggplot() + 
  geom_sf(data = rainforest %>% sf::st_union(), fill = "#73a788", color = NA) + 
  geom_sf(data = refugia, fill = "#478966", color = NA) + 
  geom_sf(data = rivers10, size = 1, color = '#44afe3') + 
  geom_sf(data = lakes10, fill = '#44afe3', color = NA) + 
  geom_sf(data = boundary_lines_land10, size = .2, color = 'white') + 
  #geom_sf(data = ubangi.buffer, fill = NA) + 
  geom_sf(data = sites) + 
  geom_sf(data = sites.ubangi, shape = 21, size = 2, fill = "black", color = "white") + 
  geom_text_repel(data = sites.ubangi %>% 
                    dplyr::filter(SITE %in% c("Batalimo", 
                                              "Maluba", 
                                              "Motenge-Boma", 
                                              "Dama 1", 
                                              "Mbati-Ngombe")) %>%
                    dplyr::distinct(SITE, LAT, LONG), 
                  aes(x = LONG, y = LAT, label = SITE), 
                  bg.r = .15, bg.color = "white", 
                  max.overlaps = Inf, min.segment.length = 0, 
                  size = 2) + 
  scale_y_continuous("") + 
  coord_sf(xlim = c(17, 21), 
           ylim = c(-1,6)) + 
  theme(axis.title.x = element_blank(), 
        panel.background = element_rect(fill = "#ffebbe"), 
        panel.grid = element_blank())

plt.chrono <- ggplot(pottery.ubangi, 
                     aes(xmin = LAT.min,
                         xmax = LAT.max,
                         ymin = FROM, 
                         ymax = TO,
                         fill = COL,
                         color = COL, 
                         x = lat.cent, 
                         y = age.cent, 
                         label = POTTERY)) + 
  #geom_rect(aes(xmin = 2.5, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#ffebbe", color = NA) + 
  #geom_rect(aes(xmin = -Inf, xmax = 4.5, ymin = 500, ymax = Inf), fill = "#73a788", color = NA) + 
  #geom_rect(aes(xmin = -Inf, xmax = 4.5, ymin = 1750, ymax = Inf), fill = "#73a788", color = NA) + 
  
  #geom_polygon(data = data.frame(age.cent = c(0, 500, 500, 0), 
  #                               lat.cent = c(-Inf, -Inf, 4.5, 2.5)), 
  #             aes(x = lat.cent, y = age.cent, fill = lat.cent), inherit.aes = F) + 
  #scale_fill_gradient(limits=c(0, 500), low = 'white', high = "black") + 
  #geom_rect(aes(xmin = -Inf, xmax = 2.5, ymin = -Inf, ymax = 0), fill = "#478966", color = NA) +   #ggnewscale::new_scale_fill() + 
  geom_rect(alpha = .25, size = 1, linetype = "dotted") + 
  geom_label_repel(aes(fill = COL), size = 2, color = "white", fontface = "bold") + 
  #geom_line(size = 2) + 
  #geom_point(shape = 21, size = 4, color = "black") + 
  #geom_vline(xintercept = c(1, 2), color = "red", linetype = "dashed") + 
  scale_color_identity() + 
  scale_fill_identity() + 
  scale_x_continuous("Latitude", limits = c(-1, 6)) + 
  scale_y_continuous("calBCE/CE") + 
  coord_flip() + 
  #theme_bw() + 
  theme(panel.background = element_rect(fill = "white"), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())

cowplot::plot_grid(
  plt.map, 
  plt.chrono,
  nrow = 1,
  align = "h", axis = "tb", 
  rel_widths = c(1,1.5),
  labels = "AUTO"
)
ggsave("fig_ubangi_chrono.pdf", bg = "white", width = 7.29, height = 4.5)
