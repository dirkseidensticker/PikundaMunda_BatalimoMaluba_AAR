source("r/header.R")


c14 %>%
  sf::st_filter(f %>% dplyr::filter(id %in% LETTERS[4:7])) %>% 
  dplyr::rename(c14age = C14AGE, 
              c14std = C14STD) %>%
  c14bazAAR::as.c14_date_list() %>%
  #c14bazAAR::calibrate(choices = "calrange") %>% # calibration
  #tidyr::unnest(cols = c("calrange"))
  tibble::column_to_rownames("LABNR") %>%
  dplyr::select(-geometry) %>%
  dplyr::select(c14age, c14std, C13, 
                SITE, FEATURE, FEATURE_DESC, COUNTRY, 
                MATERIAL, METHOD, 
                LONG, LAT, 
                PHASE, LITHICS, POTTERY, IRON, FRUIT, ZOO, 
                CLASS, REMARK, SOURCE) %>%
  xlsx::write.xlsx2("tbl/Tab_14C_dates_adrac.xlsx")
