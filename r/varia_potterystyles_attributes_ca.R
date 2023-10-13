
source("r/header.R")

attr <- data.table::fread(
  "https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/potterygroups_attributes.csv", 
  encoding = 'UTF-8') %>% 
  dplyr::select(-META) %>%
  dplyr::left_join(pottery, 
                   by = "POTTERY")

table(attr$POTTERY, attr$REGION)

# Unifying Typolgies: ----
# Vessels:
hpw.vessel.con <- data.table::fread(
  "https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/Wotzka1995-Seidensticker2021_VesselTypesConcordance.csv") %>%
  dplyr::select(TypHPW, TypDS)

hpw.vessel.con$TypHPW <- sub("^", "Vessel.", hpw.vessel.con$TypHPW)
hpw.vessel.con$TypDS <- sub("^", "Vessel.", hpw.vessel.con$TypDS)

attr$ATTR <- plyr::mapvalues(
  attr$ATTR, 
  from = hpw.vessel.con$TypHPW, 
  to = hpw.vessel.con$TypDS)

# reduce vessel types to main types:
attr[grep("Vessel.", attr$ATTR), ]$ATTR <- gsub('.{1}$', '', attr[grep("Vessel.", attr$ATTR), ]$ATTR)

# Rims:
hpw.rim.con <- data.table::fread(
  "https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/Wotzka1995-Seidensticker2021_RimTypesConcordance.csv")

hpw.rim.con$TypDS <- gsub("\\..*","",hpw.rim.con$TypDS) #  remove variants
hpw.rim.con$TypDS <- substr(hpw.rim.con$TypDS,1,1) # only use main types

hpw.rim.con$TypHPW <- sub("^", "Rim.", hpw.rim.con$TypHPW)
hpw.rim.con$TypDS <- sub("^", "Rim.", hpw.rim.con$TypDS)

attr$ATTR <- plyr::mapvalues(
  attr$ATTR, 
  from = hpw.rim.con$TypHPW, 
  to = hpw.rim.con$TypDS)

# Correspondance Analysis: ----
res.ca <- attr %>%
  dplyr::distinct(POTTERY, ATTR) %>%
  reshape2::dcast(POTTERY ~ ATTR, 
                  value.var = "POTTERY",
                  fun.aggregate = length) %>% 
  tibble::column_to_rownames("POTTERY") %>%
  FactoMineR::CA(graph = FALSE)

res.ca.coord <- res.ca$row$coord %>%
  as.data.frame() %>%
  tibble::rownames_to_column("POTTERY") %>%
  dplyr::mutate(class = "row") %>%
  dplyr::left_join(pottery, by = "POTTERY") %>%
  dplyr::mutate(AGE = (FROM + TO) / 2)

# res.ca.coord

# Plots: ----
factoextra::fviz_ca_biplot(res.ca,
                           label = "row", 
                           repel = TRUE)  + 
  coord_equal()

ggplot(res.ca.coord, aes(x = `Dim 1`, y = `Dim 2`, color = COL)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  geom_point() + 
  geom_label_repel(
    aes(label = POTTERY, 
        fill = COL),
    size = 2,
    color = "white", 
    segment.colour="black", 
    min.segment.length = 0, label.padding = .1, 
    max.overlaps = Inf) + 
  scale_color_identity() + 
  scale_fill_identity() + 
  coord_equal() + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
#ggsave("fig/fig_pottery_attributes_ca.jpg", width = 8, height = 8)

# only EIA pottery:
res.ca.eia <- attr %>%
  dplyr::filter(
    REGION %in% LETTERS[4:7] & # REGIONS D to G from Seidensticker et al. 2021
      FROM < 700) %>% # Filter only on EIA stlyes
  dplyr::distinct(POTTERY, ATTR) %>%
  reshape2::dcast(POTTERY ~ ATTR, 
                  value.var = "POTTERY",
                  fun.aggregate = length) %>% 
  tibble::column_to_rownames("POTTERY") %>%
  FactoMineR::CA(graph = FALSE)

factoextra::fviz_ca_biplot(res.ca.eia,
                           #label = "row", 
                           repel = TRUE)  + 
  coord_equal()

res.ca.eia.row.coord <- res.ca.eia$row$coord %>%
  as.data.frame() %>%
  tibble::rownames_to_column("POTTERY") %>%
  dplyr::mutate(class = "row") %>%
  dplyr::left_join(pottery, by = "POTTERY") %>%
  dplyr::mutate(AGE = (FROM + TO) / 2)

res.ca.eia.col.coord <- res.ca.eia$col$coord %>%
  as.data.frame() %>%
  tibble::rownames_to_column("attrb")

res.ca.eia.coord <- rbind(
  res.ca.eia.row.coord %>%
    dplyr::rename(LABEL = POTTERY) %>% 
    dplyr::select(LABEL, `Dim 1`, `Dim 2`, class, COL),
  res.ca.eia.col.coord %>%
    dplyr::rename(LABEL = attrb) %>% 
    dplyr::mutate(class = "col", 
                  COL = "#808080") %>%
    dplyr::select(LABEL, `Dim 1`, `Dim 2`, class, COL))


ggplot(data = res.ca.eia.coord, 
       aes(x = `Dim 1`, 
           y = `Dim 2`, 
           color = COL,
           fill = COL,
           label = LABEL)) + 
  geom_point(color = "white", shape = 21) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  geom_label_repel(aes(size = class), 
                   color = "white", 
                   segment.colour="black", 
                   #min.segment.length = 0, 
                   label.padding = .2, 
                   max.overlaps = Inf) + 
  scale_size_manual(values = c(3, 5)) + 
  scale_x_continuous(paste0("Dim1 (", round(res.ca.eia$eig[1,2], 1), " %)")) + 
  scale_y_continuous(paste0("Dim2 (", round(res.ca.eia$eig[2,2], 1), " %)")) + 
  scale_color_identity() + 
  scale_fill_identity() + 
  coord_equal() + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none")
ggsave("fig/varia_potterystyles_eia_attributes_ca.jpg", width = 10, height = 8)
