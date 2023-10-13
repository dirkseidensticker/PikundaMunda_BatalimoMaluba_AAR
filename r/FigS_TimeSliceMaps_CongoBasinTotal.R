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

bb <- c(xmin = 14, xmax = 26, ymin = -6, ymax = 6)

rivers10 <- rnaturalearth::ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass="sf") %>% sf::st_crop(bb)
lakes10 <- rnaturalearth::ne_download(scale = 10, type = "lakes", category = "physical", returnclass="sf")

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
  dplyr::left_join(pottery, by = "POTTERY") %>%
  dplyr::filter(!is.na(FROM))

p <- list()
for(i in 1:(nrow(breaks)-1)){
  
  sites.sel <- sites.chrono %>% 
    dplyr::filter(TO > breaks[i,"breaks"] & FROM < breaks[i+1,"breaks"])
  
  sf::st_geometry(sites.sel) <- NULL
  
  sites.sel.w <- sites.sel %>%
    reshape2::dcast(SITE + LONG + LAT ~ POTTERY, fun.aggregate = length)
  
  cols <- sites.sel %>% dplyr::distinct(POTTERY, COL) %>% dplyr::arrange(POTTERY) %>% dplyr::pull(COL)
  
  p[[i]] <- ggplot() + 
    geom_sf(data = rivers10, size = .5, color = 'grey') + 
    geom_sf(data = lakes10, fill = 'grey', color = NA) + 
    scatterpie::geom_scatterpie(data = sites.sel.w, 
                                aes(x = LONG, y = LAT), 
                                cols = names(sites.sel.w[4:ncol(sites.sel.w)])) + 
    scale_fill_manual(values = cols) + 
    coord_sf(xlim = c(15.5, 25), 
             ylim = c(-4.5, 5)) + 
    ggtitle(LETTERS[i], subtitle = breaks[i,"labels"]) + 
    theme_few() + 
    guides(fill = guide_legend(nrow = 5)) + 
    theme(axis.title = element_blank(), 
          plot.subtitle = element_text(hjust = 0.5),
          plot.title = element_text(face = "bold"),
          legend.title = element_blank(), 
          legend.justification = c(0, 0),
          legend.position = c(.01, .01),
          legend.box = "vertical")
  }

plt <- do.call(gridExtra::grid.arrange, p)

ggsave("fig/fig_s_map_time_slices_congobasin_total.jpg", plt, width = 9*2, height = 13*2)
ggsave("fig_s_map_time_slices_congobasin_total.pdf", plt, width = 9*2, height = 13*2)







sites.cent <- data.frame(matrix(ncol = ncol(sites.chrono) + 1, nrow = 0))
x <- c(names(sites.chrono), "CLASS")
colnames(sites.cent) <- x

for (i in 1:(nrow(breaks)-1)) {
  l <- sites.chrono %>% 
    dplyr::filter(TO > breaks[i,"breaks"] & 
                    FROM < breaks[i+1,"breaks"]) %>% 
    dplyr::mutate(CLASS = breaks[i,"labels"])
  sites.cent <- rbind(sites.cent, as.data.frame(l))
}
sites.cent$AGE <- (as.numeric(sub("/.*", "", sub(".*? ", "", sites.cent$CLASS))) + as.numeric(sub(".*/", "", sub(".*? ", "", sites.cent$CLASS)))) / 2

sites.cent.w <- sites.cent %>%
  reshape2::dcast(AGE + SITE + LONG + LAT ~ POTTERY, fun.aggregate = length, value.var = "SITE")

cols <- sites.cent %>% dplyr::distinct(POTTERY, COL) %>% dplyr::arrange(POTTERY) %>% dplyr::pull(COL)

ggplot() + 
  scatterpie::geom_scatterpie(data = sites.cent.w, 
                              aes(x = LONG, y = LAT), 
                              cols = names(sites.cent.w[5:ncol(sites.cent.w)])) + 
  scale_fill_manual(values = cols) + 
  coord_sf(xlim = c(15.5, 25), 
           ylim = c(-2.5, 5.25)) + 
  facet_wrap(AGE ~ ., 
             ncol = 3) +
  theme_few() + 
  theme(axis.title = element_blank(), 
        legend.title = element_blank())








