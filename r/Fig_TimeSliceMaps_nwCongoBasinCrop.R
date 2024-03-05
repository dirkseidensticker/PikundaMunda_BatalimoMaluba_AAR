# Time-Slice Maps

source("r/header.R")
source("r/myfct.R")

library(scatterpie)

pottery <- pottery %>% dplyr::filter(REGION %in% LETTERS[4:7])

bayes <- data.table::fread("tbl/tbl_bayesphases_comparison.csv") %>%
  dplyr::filter(`Pottery Group` != "Ilambi") %>%
  dplyr::select(1, 3, 5) %>%
  dplyr::rename("POTTERY" = "Pottery Group", 
                "FROM" = "Bayesian Start", 
                "TO" = "Bayesian End")

pottery <- rbind(
  bayes %>% 
    dplyr::left_join(pottery %>% dplyr::select(POTTERY, REGION, COL), by = "POTTERY"),
  pottery %>%
    dplyr::filter(!POTTERY %in% c(bayes %>% dplyr::pull(POTTERY))) %>%
    dplyr::select(POTTERY,FROM,TO,REGION, COL))

bb <- c(xmin = 15.25, xmax = 20.5, ymin = -1.5, ymax = 5.5)

osm.rivers.lines <- geojsonsf::geojson_sf("data/gis/OSM_river_lines.geojson") %>% sf::st_crop(bb)
sf_use_s2(FALSE)
osm.rivers.poly <- geojsonsf::geojson_sf("data/gis/OSM_river_lakes_poly.geojson") %>%
  sf::st_make_valid() %>% sf::st_crop(bb)
sf_use_s2(TRUE)
osm.coast.line <- geojsonsf::geojson_sf("data/gis/OSM_coast_lines.geojson") %>% sf::st_crop(bb)

rivers10 <- rnaturalearth::ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass="sf") %>% sf::st_crop(bb)
lakes10 <- rnaturalearth::ne_download(scale = 10, type = "lakes", category = "physical", returnclass="sf") %>% sf::st_make_valid() %>% sf::st_crop(bb)

breaks <- seq(-400, 2000, 200)
class <- seq(1,length(breaks), 1)
breaks <- data.frame(breaks, class)
for(i in 1:nrow(breaks)){
  #breaks[i, "labels"] <- paste0(breaks[i,"class"], ": ", breaks[i,"breaks"], "/", breaks[i+1,"breaks"])
  if (breaks[i,"breaks"] < 0) {
    breaks[i, "labels"] <- paste0(breaks[i,"breaks"],"/",breaks[i,"breaks"]+200," BCE")
  } else {
    breaks[i, "labels"] <- paste0(breaks[i,"breaks"],"/",breaks[i,"breaks"]+200," CE")
  }
}

# sites per time-slice:
sites.chrono <- sites %>%
  dplyr::filter(LONG > bb[[1]] & LONG < bb[[2]] & LAT > bb[[3]] & LAT < bb[[4]]) %>% 
  dplyr::left_join(pottery, by = "POTTERY") %>%
  dplyr::filter(!is.na(FROM))

# EIA ----

p <- list()
for(i in 1:6){
  
  sites.sel <- sites.chrono %>% 
    dplyr::filter(TO > breaks[i,"breaks"] & FROM < breaks[i+1,"breaks"])
  
  sf::st_geometry(sites.sel) <- NULL
  
  sites.sel.w <- sites.sel %>%
    reshape2::dcast(SITE + LONG + LAT ~ POTTERY, fun.aggregate = length)
  
  cols <- sites.sel %>% dplyr::distinct(POTTERY, COL) %>% dplyr::arrange(POTTERY) %>% dplyr::pull(COL)
  
  p[[i]] <- cowplot::plot_grid(
    # map
    ggplot() + 
      geom_sf(data = osm.rivers.lines, size = .5, color = 'grey') + 
      geom_sf(data = osm.rivers.poly, size = .5, fill = 'grey', color = 'grey') + 
      geom_sf(data = lakes10, fill = 'grey', color = NA) + 
      scatterpie::geom_scatterpie(data = sites.sel.w, 
                                  aes(x = LONG, y = LAT, r = .1), 
                                  cols = names(sites.sel.w[4:ncol(sites.sel.w)]),
                                  pie_scale = 1, color = NA) + 
      scale_fill_manual(values = cols) + 
      coord_sf(xlim = c(bb[[1]], bb[[2]]), 
               ylim = c(bb[[3]], bb[[4]])) + 
      ggtitle(LETTERS[i], subtitle = breaks[i,"labels"]) + 
      scale_x_continuous(expand = c(0, 0), breaks = seq(16, 20, 1)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      annotate("text", x = 19, y = 1.7, label = "CONGO", color = "grey", angle = 35) + 
      annotate("text", x = 20, y = 4.65, label = "UBANGI", color = "grey", angle = -40) + 
      annotate("text", x = 16.1, y = 1.2, label = "SANGHA", color = "grey", angle = -45) + 
      theme_few() + 
      guides(fill = guide_legend(ncol = 1)) + 
      theme(axis.title = element_blank(), 
            plot.subtitle = element_text(hjust = 0.5),
            plot.title = element_text(face = "bold"),
            legend.position = "none"),
    # legend
    cowplot::get_legend(
      ggplot() + 
        scatterpie::geom_scatterpie(data = sites.sel.w, 
                                    aes(x = LONG, y = LAT, r = .1), 
                                    cols = names(sites.sel.w[4:ncol(sites.sel.w)]),
                                    pie_scale = 1) + 
        scale_fill_manual(values = cols) + 
        theme(legend.title = element_blank(),
              #legend.position = "right",
              legend.box.margin = margin(-10, -10, -10, -10),
              legend.margin = margin(0),
              legend.justification = c(0,.5),
              legend.box = "vertical")), 
    rel_widths = c(4,1), nrow = 1
  )
}

plt <- do.call(gridExtra::grid.arrange, p)

ggsave("fig/fig_map_time_slices_1_eia.jpg", plt, width = 9*1.25, height = 13*1.25)
ggsave("fig_map_time_slices_1_eia.pdf", plt, width = 9*1.25, height = 13*1.25)


# LIA ----
breaks <- breaks[7:13,]

p <- list()
for(i in 1:6){
  
  sites.sel <- sites.chrono %>% 
    dplyr::filter(TO > breaks[i,"breaks"] & FROM < breaks[i+1,"breaks"])
  
  sf::st_geometry(sites.sel) <- NULL
  
  sites.sel.w <- sites.sel %>%
    reshape2::dcast(SITE + LONG + LAT ~ POTTERY, fun.aggregate = length)
  
  cols <- sites.sel %>% dplyr::distinct(POTTERY, COL) %>% dplyr::arrange(POTTERY) %>% dplyr::pull(COL)
  
  p[[i]] <- cowplot::plot_grid(
    # map
    ggplot() + 
      geom_sf(data = osm.rivers.lines, size = .5, color = 'grey') + 
      geom_sf(data = osm.rivers.poly, size = .5, fill = 'grey', color = 'grey') + 
      geom_sf(data = lakes10, fill = 'grey', color = NA) + 
      scatterpie::geom_scatterpie(data = sites.sel.w, 
                                  aes(x = LONG, y = LAT, r = .1), 
                                  cols = names(sites.sel.w[4:ncol(sites.sel.w)]),
                                  pie_scale = 1, color = NA) + 
      scale_fill_manual(values = cols) + 
      coord_sf(xlim = c(bb[[1]], bb[[2]]), 
               ylim = c(bb[[3]], bb[[4]])) + 
      ggtitle(LETTERS[i], subtitle = breaks[i,"labels"]) + 
      scale_x_continuous(expand = c(0, 0), breaks = seq(16, 20, 1)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      annotate("text", x = 19, y = 1.7, label = "CONGO", color = "grey", angle = 35) + 
      annotate("text", x = 20, y = 4.65, label = "UBANGI", color = "grey", angle = -40) + 
      annotate("text", x = 16.1, y = 1.2, label = "SANGHA", color = "grey", angle = -45) + 
      theme_few() + 
      guides(fill = guide_legend(ncol = 1)) + 
      theme(axis.title = element_blank(), 
            plot.subtitle = element_text(hjust = 0.5),
            plot.title = element_text(face = "bold"),
            legend.position = "none"),
    # legend
    cowplot::get_legend(
      ggplot() + 
        scatterpie::geom_scatterpie(data = sites.sel.w, 
                                    aes(x = LONG, y = LAT, r = .1), 
                                    cols = names(sites.sel.w[4:ncol(sites.sel.w)]),
                                    pie_scale = 1) + 
        scale_fill_manual(values = cols) + 
        theme(legend.title = element_blank(),
              #legend.position = "right",
              legend.box.margin = margin(-10, -10, -10, -10),
              legend.margin = margin(0),
              legend.justification = c(0,.5),
              legend.box = "vertical")), 
    rel_widths = c(4,1), nrow = 1
  )
}

plt <- do.call(gridExtra::grid.arrange, p)

ggsave("fig/fig_map_time_slices_2_lia.jpg", plt, width = 9*1.25, height = 13*1.25)
ggsave("fig_map_time_slices_2_lia.pdf", plt, width = 9*1.25, height = 13*1.25)
