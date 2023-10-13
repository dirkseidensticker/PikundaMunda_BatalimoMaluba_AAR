# Hiatus sites in Gabon:

source("r/header.R")
source("r/myfct.R")

# script up to here derived from https://github.com/dirkseidensticker/HumActCentralAfrica_Paper/blob/main/script/Supplementary%20Figure%204%20relict%20populations.R

phases <- data.table::fread("https://raw.githubusercontent.com/dirkseidensticker/HumActCentralAfrica_Paper/main/output/rcarbon_phases.csv", 
                            encoding = "UTF-8")

mia.hiatus <- phases %>%
  dplyr::filter(PHASE == "low activity 2")

c14 <- dplyr::filter(c14, 
                     C14AGE > 0 &
                     C14STD > 0 & 
                     CLASS %in% c("Ia","Ib","Ic", "Id"))

cal <- rcarbon::calibrate(x = c14$C14AGE,
                          errors = c14$C14STD,
                          calCurves = 'intcal13', 
                          ncores = ncores, 
                          normalised = FALSE) #running calibration over 3 cores

cal.median <- rcarbon::medCal(cal) # return individual medians
cal.median <- 1950 - cal.median

c14$calBCAD <- cal.median

c14.hiatus <- dplyr::filter(c14, 
                            calBCAD > mia.hiatus$FROM & 
                            calBCAD < mia.hiatus$TO)

# only sites in Gabon:
# rev 3 claims "evidence of continuous settlements at least in North West Gabon"

c14.sel <- c14.hiatus %>%
  dplyr::filter(COUNTRY == "GAB")

c14.sel.labno <- c14.sel %>% dplyr::pull("LABNR")

c14.sel.labno

c14.cal <- c14 %>%
  dplyr::filter(LABNR %in% c14.sel.labno) %>%
  dplyr::rename(c14age = C14AGE, c14std = C14STD) %>%
  c14bazAAR::as.c14_date_list() %>%
  c14bazAAR::calibrate(choices = "calprobdistr") %>% # calibration
  tidyr::unnest(cols = c("calprobdistr"))

c14.cal %>%
  dplyr::mutate(LABEL = paste0(SITE, " (", LABNR, ")")) %>% 
  dplyr::arrange(-c14age) %>% 
  dplyr::mutate_at(vars(LABEL), dplyr::funs(factor(., levels=unique(.)))) %>%
  ggplot() + 
  geom_rect(xmin = 600, xmax = 1000, ymin = -Inf, ymax = Inf, fill = "grey90") + 
  ggridges::geom_ridgeline(
    aes(x = -calage + 1950, 
        y = LABEL, 
        height = density),
    scale = 50) + 
  scale_x_continuous("cal BCE/CE", expand = c(0, 0), 
                     breaks = c(seq(-600, 1800, 200), 1950)) + 
  scale_y_discrete(position = "right") +
  ggtitle("Radicarbon dates from Gabon dating into the 'Hiatus'") + 
  theme_few() +
  theme(strip.placement = "left",
        strip.text.y.left = element_text(angle = 0), 
        axis.title.y = element_blank())
ggsave("fig/varia_c14_hiatus_gabon.jpg", width = 6, height = 6)

# look at the sequences at those sites:

c14.sel.sites <- c14.sel %>% dplyr::distinct(SITE) %>% dplyr::pull(SITE)

c14.cal <- c14 %>%
  dplyr::filter(SITE %in% c14.sel.sites) %>%
  dplyr::rename(c14age = C14AGE, c14std = C14STD) %>%
  c14bazAAR::as.c14_date_list() %>%
  c14bazAAR::calibrate(choices = "calprobdistr") %>% # calibration
  tidyr::unnest(cols = c("calprobdistr"))

c14.cal %>%
  #dplyr::mutate(LABEL = paste0(SITE, " (", LABNR, ")")) %>% 
  dplyr::arrange(-c14age) %>% 
  dplyr::mutate_at(vars(LABNR), dplyr::funs(factor(., levels=unique(.)))) %>%
  ggplot() + 
  geom_rect(xmin = 600, xmax = 1000, ymin = -Inf, ymax = Inf, fill = "grey90") + 
  ggridges::geom_ridgeline(
    aes(x = -calage + 1950, 
        y = LABNR, 
        height = density),
    scale = 50) + 
  facet_grid(SITE ~ ., 
             #facet_grid(SITE + FEATURE ~ ., 
             scales = "free", 
             space = "free", 
             switch = "y") + 
  scale_x_continuous("cal BCE/CE", expand = c(0, 0)) + 
  scale_y_discrete(position = "right") +
  ggtitle("Sites in Gabon with radicarbon dates\ndating into the 'Hiatus'") + 
  theme_few() +
  theme(strip.placement = "left",
        strip.text.y.left = element_text(angle = 0), 
        axis.title.y = element_blank())
ggsave("fig/varia_c14_hiatus_gabon_sites.jpg", width = 6, height = 6)


# Dibamba ----

c14.cal <- c14 %>%
  dplyr::filter(SITE == "Dibamba") %>%
  dplyr::rename(c14age = C14AGE, c14std = C14STD) %>%
  c14bazAAR::as.c14_date_list() %>%
  c14bazAAR::calibrate(choices = "calprobdistr") %>% # calibration
  tidyr::unnest(cols = c("calprobdistr"))

c14.cal %>%
  #dplyr::mutate(LABEL = paste0(SITE, " (", LABNR, ")")) %>% 
  dplyr::arrange(-c14age) %>% 
  dplyr::mutate_at(vars(LABNR), dplyr::funs(factor(., levels=unique(.)))) %>%
  ggplot() + 
  geom_rect(xmin = 600, xmax = 1000, ymin = -Inf, ymax = Inf, fill = "grey90") + 
  ggridges::geom_ridgeline(
    aes(x = -calage + 1950, 
        y = LABNR, 
        height = density),
    scale = 50) + 
  scale_x_continuous("cal BCE/CE", expand = c(0, 0), breaks = seq(-1000, 2000, 200)) + 
  scale_y_discrete(position = "right") +
  ggtitle("Radicarbon dates from Dibamba (de Saulieu et al. 2017)") + 
  theme_few() +
  theme(strip.placement = "left",
        strip.text.y.left = element_text(angle = 0), 
        axis.title.y = element_blank())
ggsave("fig/varia_c14_dibamba.jpg", width = 6, height = 6)


# datings of the Oveng style:

c14.cal <- data.table::fread(
  "https://raw.githubusercontent.com/dirkseidensticker/aDRAC/master/aDRAC.csv", 
  encoding = "UTF-8") %>%
  dplyr::filter(POTTERY == "Oveng") %>%
  dplyr::rename(c14age = C14AGE, c14std = C14STD) %>%
  c14bazAAR::as.c14_date_list() %>%
  c14bazAAR::calibrate(choices = "calprobdistr") %>% # calibration
  tidyr::unnest(cols = c("calprobdistr"))

rbind(c14.cal %>%
        dplyr::filter(grepl("Shell", MATERIAL)) %>%
        dplyr::mutate(MATERIAL = "shell"),
      c14.cal %>%
        dplyr::filter(!grepl("Shell", MATERIAL)) %>%
        dplyr::mutate(MATERIAL = "charcoal")) %>%
  dplyr::arrange(-c14age) %>% 
  dplyr::mutate_at(vars(LABNR), dplyr::funs(factor(., levels=unique(.)))) %>%
  ggplot() + 
  ggridges::geom_ridgeline(
    aes(x = -calage + 1950, 
        y = LABNR, 
        height = density, 
        fill = MATERIAL),
    scale = 50) + 
  scale_x_continuous("cal BCE/CE", expand = c(0, 0), breaks = seq(-1000, 2000, 200)) + 
  scale_y_discrete(position = "right") +
  scale_fill_manual(values = c("grey25", "grey75")) + 
  #ggtitle("Radicarbon dates from Dibamba (de Saulieu et al. 2017)") + 
  theme_few() +
  theme(legend.position = "top",
        strip.placement = "left",
        strip.text.y.left = element_text(angle = 0), 
        axis.title.y = element_blank())
ggsave("fig/varia_c14_oveng.jpg", width = 6, height = 6)
