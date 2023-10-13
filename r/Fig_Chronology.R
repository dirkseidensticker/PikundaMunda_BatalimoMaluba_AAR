# Chronology

# Chronology Schema ####

source("r/header.R")
source("r/myfct.R")

# replace conventional start/end with results from bayes phase modeling:

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

# LOOP ----
# All dates/styles ----
datalist <- list()
filterlist <- list()
a.sel.list <- list()
cnt <- 1

id.lst <- LETTERS[4:7]

filt.reg <- st_multipolygon()

filt.reg <- st_sf(geom = st_sfc(st_polygon()))
st_crs(filt.reg) = 4326
filt.reg$class <- NA

for(i in 1:length(id.lst)){
  print(paste("[", i, "/", length(id.lst), "] - Region:", id.lst[i]))
  f.sel <- f %>% 
    filter(id == id.lst[i]) %>% 
    st_union()
  
  a <- c14[which(st_intersects(f.sel, c14, sparse = FALSE)), ]
  
  a <- filter(a, 
              C14AGE > 71 & 
              C14AGE < 6000 &
              (POTTERY != '' &  POTTERY != 'indet' &  POTTERY != '(indet)' & POTTERY != '-') & 
              CLASS %in% c("Ia", "Ib", "Ic", "IIc")
  )
  
  if(nrow(a) != 0){
    a$class <- id.lst[i]
    
    styles <- unique(a$POTTERY) # unique styles per region
    styles <- unlist(strsplit(as.character(styles), "; ")) # split fields with multiple entries
    styles <- styles[!grepl("\\(", styles)] # remove cases in parantheses 
    styles <- trimws(styles) #  remove leading/trailing whitespaces
    styles <- unique(styles)
    
    # TODO: loop through all dates per style:

    for(j in 1:length(styles)){
      print(paste("[", j, "/", length(styles), "] -", styles[j]))
      
      # > FILTER DATES ---- 
      d <- filter(a, grepl(styles[j], a$POTTERY)) # filter for dates related to style
      d <- filter(d, !grepl(paste0("\\(" , styles[j], "\\)"), d$POTTERY)) # remove cases in parantheses
      
      res <- rcarbonsum(d, oxcalnorm = TRUE)
      
      res[[1]]$median <- list(res[[2]])
      res[[1]]$start <- res[[3]]
      res[[1]]$style <- styles[j]
      res[[1]]$label <- paste0(res[[1]]$style, " (", nrow(d), ")")
      res[[1]]$region <- id.lst[i]
      
      dat <- res[[1]]
      
      datalist[[cnt]] <- dat
      a.sel.list[[cnt]] <- a
      
      # feature for filter:
      f.sel <- st_sf(geom = f.sel)
      f.sel[["class"]] <- id.lst[i]
      
      filt.reg <- rbind(filt.reg, f.sel)
      
      cnt <- cnt +1
    } # end of loop through styles
  }
} # end of loop through regions

styleprob <- do.call(rbind, datalist)
sites <- do.call(rbind, a.sel.list)

styleprob.med <- unique(styleprob[, c("style", "median", "region")])
styleprob.med <- unnest(styleprob.med, median)

names(styleprob.med)[names(styleprob.med) == "median"] <- "TO"

styleschrono <- pottery %>%
  dplyr::mutate(region = REGION)

## MERGE WITH STYLECHRONO
styleprob <- merge(
  x = styleprob, 
  by.x = "style", 
  y = styleschrono[,c("POTTERY", "FROM", "TO", "COL")], 
  by.y = "POTTERY", 
  sort = FALSE, all.x = TRUE)

styleprob$rel <- TRUE

# add not merged groups (what is left?)
B <- unique(styleprob$style)
A <- unique(styleschrono$POTTERY)
missing <- A[which(!A %in% B)]

style.m <- subset(styleschrono, POTTERY %in% missing) %>%
  dplyr::filter(region != '') %>%
  dplyr::select(POTTERY, FROM, TO, REGION, COL)

style.m <- style.m[,c("POTTERY", "FROM", "TO", "REGION", "COL")]

names(style.m)[names(style.m) == "POTTERY"] <- "style"
names(style.m)[names(style.m) == "REGION"] <- "region"

style.m$label <- style.m$style

style.m$rel <- FALSE

style.m$grid.calBP <- NA
style.m$grid.PrDens <- NA
style.m$median <- NA
style.m$start <- NA

style.m <- style.m[,c("style", "grid.calBP", "grid.PrDens", "median", "start", "label", "region", "FROM", "TO", "rel", "COL")]

styleprob <- rbind(styleprob, style.m)

style.m.lab <- style.m[,c("style", "FROM", "TO", "label", "region", "COL")]

style.m.lab$mean <- (style.m.lab$FROM + style.m$TO) / 2

style.m.lab <- style.m.lab[,c("style", "mean", "label", "region", "COL")]
style.m.lab

style.max <- styleprob %>% 
  group_by(label) %>% 
  slice(which.max(grid.calBP))
names(style.max)[2] <- "max"

style.min <- styleprob %>% 
  group_by(label) %>% 
  slice(which.min(grid.calBP))
names(style.min)[2] <- "min"

styleprob.lab <- merge(x = style.min, y = style.max[,c("style", "max")], by = "style")
styleprob.lab$mean <- (styleprob.lab$max + styleprob.lab$min) / 2

styleprob.lab <- styleprob.lab[,c("style", "mean", "label", "region", "COL")]

styleprob.lab1 <- rbind(styleprob.lab, style.m.lab)

colnames(styleprob.lab1)[2] <- "TO"


style.box <- unique(styleprob[c("style", "FROM", "TO", "start", "region", "rel", "COL")])
style.box

library(xlsx)
regions.labs <- read.xlsx("data/gis/Tab_Regions.xlsx", sheetIndex = 1)
regions.labs$labs <- paste0(regions.labs$Code, " (", regions.labs$Short, ")")

style.box <- merge(x = style.box, 
                   y = regions.labs[,c("Code", "labs")], 
                   by.x = "region", by.y = "Code", 
                   all.x = TRUE)

styleprob <- merge(x = styleprob, y = regions.labs[,c("Code", "labs")], 
                   by.x = "region", by.y = "Code", 
                   all.x = TRUE)

styleprob.med <- merge(x = styleprob.med, y = regions.labs[,c("Code", "labs")], 
                       by.x = "region", by.y = "Code", 
                       all.x = TRUE)

filterA <- LETTERS[seq(from = 1, to = 8)]

style.box[style.box$COL == "" | is.na(style.box$COL), "COL"] <- "#808080"
style.box[is.na(style.box$COL),"COL"] <- "#808080"

unique(styleprob$style)


# PLOT ----
ggplot(data = filter(style.box,
                      style != "mixed" & 
                      region %in% LETTERS[4:7]), 
       aes(x = FROM, 
           y = reorder(style, FROM), 
           xend = TO, 
           yend = style)) + 
  #geom_segment(aes(linetype = rel), alpha = 0) + 
  geom_segment(aes(linetype = rel, color = COL), 
               size = 3, alpha = 0.6) + 
  scale_linetype_manual(values = c("11", "solid")) + 
  scale_color_identity() + 
  ggnewscale::new_scale_color() + 
  geom_line(data = filter(styleprob, 
                          style != "mixed" & 
                            region %in% LETTERS[4:7]), 
            aes(x = grid.calBP,
                y = style,
                color = grid.PrDens), 
            size = 1.5) + 
  geom_point(data = filter(styleprob.med, 
                            style != "mixed" & 
                             region %in% LETTERS[4:7]), 
             aes(x = TO, y = style), 
             color = "black", fill = "white", shape = 21, size = 1) +   
  geom_label(aes(label = style), 
            #angle = 90, 
            hjust = 1, nudge_x = -50, 
            size = 2, label.size = NA, 
            fontface = "bold", 
            label.padding = unit(0.1, "lines")) + 
  scale_colour_gradient(low = "white", 
                        high = "black") + 
  scale_x_continuous("cal BCE/CE", 
                     limits = c(-700,2000), 
                     breaks = c(seq(-1400,1800,200), 1950), 
                     expand = c(0,0)) + 
  facet_grid(labs ~ ., 
             scales = "free", 
             space = "free") + 
  theme_bw() + 
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none", 
        strip.text.y = element_text(angle = 0), 
        strip.background = element_blank())
ggsave("fig_chronology.pdf", width = 6, height = 8)
ggsave("fig/fig_chronology.jpg", width = 6, height = 8)

