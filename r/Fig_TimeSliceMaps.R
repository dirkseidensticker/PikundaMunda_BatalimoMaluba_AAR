# Time-Slice Maps

source("r/header.R")
source("r/myfct.R")

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
  breaks[i, "labels"] <- paste0(breaks[i,"class"], ": ", breaks[i,"breaks"], "/", breaks[i+1,"breaks"])
}

# Frequency of sites per pottery group

pottery.sites.freq <- as.data.frame(stats::aggregate(SITE ~ POTTERY, 
                                                     data =sites, 
                                                     FUN = length)) 

# Area per pottery group (Convex hull)
# see https://github.com/joelgombin/concaveman

id <- dplyr::filter(pottery.sites.freq, SITE > 2)
conc.hull.lst <- list()
for(i in 1:nrow(id)){
  sites.f <- dplyr::filter(sites, POTTERY == id[i,1])
  conc.hull <- concaveman::concaveman(sites.f)
  conc.hull$POTTERY = id[i, "POTTERY"]
  conc.hull.lst[[i]] <- conc.hull
  #pottery.sites.area <- rbind(pottery.sites.area, conc.hull)
}
pottery.sites.area <- do.call(rbind, conc.hull.lst) %>%
  sf::st_make_valid()

# Frequency of pottery groups per bin
pottery.cent <- data.frame(matrix(ncol = ncol(pottery)+1, nrow = 0))
x <- c(names(pottery), "CLASS")
colnames(pottery.cent) <- x

for (i in 1:length(pottery$POTTERY)){
  for (j in 1:(nrow(breaks)-1)) {
    if(pottery[i,"TO"] > breaks[j,"breaks"] & 
       pottery[i,"FROM"] < breaks[j+1,"breaks"]){
      l <- pottery[i,]
      l$CLASS <- breaks[j,"labels"]
      pottery.cent <- rbind(pottery.cent, as.data.frame(l))
    }
  }
}
pottery.cent$AGE <- (as.numeric(sub("/.*", "", sub(".*? ", "", pottery.cent$CLASS))) + as.numeric(sub(".*/", "", sub(".*? ", "", pottery.cent$CLASS)))) / 2
#pottery.cent$AGE.jitter 	<- jitter(pottery.cent$AGE, 2)

pottery.cent.meta <- pottery.sites.area %>%
  dplyr::left_join(pottery.cent, by = "POTTERY") %>%
  dplyr::filter(!is.na(CLASS)) %>%
  sf::st_transform(crs = 32733) %>%
  sf::st_buffer(dist = 20e3) %>%
  sf::st_transform(crs = 4326)

# set missing colours to grey
pottery.cent.meta[pottery.cent.meta$COL == '',"COL"] <- "#808080"

# set label:
lbl <- unique(pottery.cent.meta[,c("AGE", "CLASS")])
lbl$CLASS <- sub(".*? ", "", lbl$CLASS)
lbl <- setNames(lbl$CLASS, lbl$AGE)

# labels for polygons
pottery.cent.meta.labs <- pottery.cent.meta %>%
  sf::st_centroid()

pottery.cent.meta.labs <- cbind(
  pottery.cent.meta.labs, 
  do.call(rbind, 
          st_geometry(pottery.cent.meta.labs)) %>% 
    as_tibble() %>% 
    setNames(c("lon","lat"))) %>%
  dplyr::filter(lon > 15.5 & lon < 25 & lat > -4.25 & lat < 5)

# map ----
p <- ggplot(pottery.cent.meta) +
  geom_sf(data = rivers10, size = .5, color = 'grey') + 
  geom_sf(data = lakes10, fill = 'grey', color = NA) + 
  geom_sf(aes(fill = COL), alpha = .5) + 
  geom_label_repel(data = pottery.cent.meta.labs,
                   aes(x = lon, y = lat, label = POTTERY, fill = COL), 
                   size = 2.5, 
                   label.padding = 0.1, min.segment.length = 0, 
                   color = "white", fontface = "bold", 
                   max.overlaps = Inf) + 
  scale_fill_identity() + 
  facet_wrap(AGE~., 
             labeller = labeller(AGE = lbl), 
             ncol = 3) + 
  coord_sf(xlim = c(15.5, 25), 
           ylim = c(-4.25, 5)) + 
  theme_few() + 
  theme(legend.position = "none", 
        axis.title = element_blank())
ggsave("fig/fig_map_time_slices.jpg", p, width = 9, height = 12.5)
ggsave("fig/fig_map_time_slices.pdf", p, width = 9, height = 12.5)
