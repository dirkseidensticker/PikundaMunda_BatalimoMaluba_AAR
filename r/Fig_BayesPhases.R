# Bayesian ceramic phase modelling

library(ggplot2)
library(tidyr)

library(rcarbon)
library(nimbleCarbon)

c14 <- rbind(
  data.table::fread(
    "https://raw.githubusercontent.com/dirkseidensticker/aDRAC/master/aDRAC.csv", 
    encoding = "UTF-8"),
  data.table::fread(
    "data/aDRAC_new.csv", 
    dec = ",", 
    encoding = "UTF-8")
)

pottery <- data.table::fread("https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/potterygroups.csv", 
                             encoding = "UTF-8") %>%
  dplyr::select(-DESCRIPTION) %>%
  dplyr::filter(REGION %in% LETTERS[4:7]) # only select pottery styles found in the Congo Basin

id <- pottery$POTTERY
res.lst <- list()
c14.lst <- list()

for (i in 1:length(id)) {
  c14.sel <- c14 %>% 
    dplyr::filter(C14AGE  > 70 & CLASS %in% c("Ia", "Ib", "Ic", "IIc")) %>%
    dplyr::filter(grepl(id[i], POTTERY)) %>% # filter for dates related to style
    dplyr::filter(!grepl(paste0("\\(" , id[i], "\\)"), POTTERY)) # remove cases in parantheses
  
  c14.lst[[i]] <- c14.sel
  
  n = nrow(c14.sel)
  
  if(n >= 2){
    print(paste0(i, "/", length(id), " - ", id[i], " (", n, " c14-dates)"))
    
    # Define NIMBLE Model
    phasemodel <- nimbleCode({
      for (i in 1:N){
        #  Likelihood
        theta[i] ~ dunif(alpha[1],alpha[2]);
        # Calibration
        mu[i] <- interpLin(z=theta[i], x=calBP[], y=C14BP[]);
        sigmaCurve[i] <- interpLin(z=theta[i], x=calBP[], y=C14err[]);
        sd[i] <- (sigma[i]^2+sigmaCurve[i]^2)^(1/2);
        X[i] ~ dnorm(mean=mu[i],sd=sd[i]);
      }
      # Prior
      alpha[1] ~ dunif(0,50000);
      alpha[2] ~ T(dunif(0,50000),alpha[1],50000)
    })
    
    #define constant, data, and inits:
    data("intcal20") 
    constants <- list(N = n,
                      calBP = intcal20$CalBP,
                      C14BP = intcal20$C14Age,
                      C14err = intcal20$C14Age.sigma)
    
    data <- list(X = c14.sel$C14AGE, 
                 sigma = c14.sel$C14STD)

    
    m.dates = rcarbon::medCal(rcarbon::calibrate(
      c14.sel$C14AGE,
      c14.sel$C14STD,
      verbose = FALSE))
    
    # important to keep enough 'space' before and after
    start <- floor(min(c14.sel$C14AGE)/1000) * 1000 - 1000
    if (start < 0) { start <- 0 } # avoid negative starts
    end <- ceiling(max(c14.sel$C14AGE)/1000) * 1000 + 1000
    
    inits <- list(alpha = c(start,
                            end),
                  theta = m.dates)
    
    #Run MCMC
    mcmc.samples <- nimbleMCMC(
      code = phasemodel,
      constants = constants,
      data = data,
      niter = 20000, 
      nchains = 1, 
      thin = 1, 
      nburnin = 5000, 
      progressBar = FALSE, 
      monitors = c('alpha','theta'), 
      inits = inits, 
      samplesAsCodaMCMC = TRUE, 
      set.seed(123), 
      summary = TRUE)
    
    start.dens <- density(matrix(mcmc.samples$samples[,'alpha[2]']))
    start.dens <- data.frame(bp = start.dens$x, 
                             prob = start.dens$y)
    
    start.dens$median <- data.frame(mcmc.samples$summary)[2, "Median"]
    start.dens$POTTERY <- id[i]
    start.dens$alpha <- "start"
    
    end.dens <- density(matrix(mcmc.samples$samples[,'alpha[1]']))
    end.dens <- data.frame(bp = end.dens$x, 
                           prob = end.dens$y)
    end.dens$median <- data.frame(mcmc.samples$summary)[1, "Median"]
    end.dens$POTTERY <- id[i]
    end.dens$alpha <- "end"
    
    res <- rbind(start.dens,
                 end.dens)
    
    res.lst[[i]] <- res
  }
}

# list of used 14c dates
do.call(rbind, c14.lst) %>%
  dplyr::arrange(LABNR) %>%
  write.csv("tbl/tbl_14c_used_bayes_phases.csv", row.names = F)

# results
res.bayes <- do.call(rbind, res.lst)

res.bayes.median <- res.bayes %>% 
  dplyr::distinct(median, POTTERY, alpha) %>%
  dplyr::mutate(bp = median, 
                prob = 0) %>%
  dplyr::select(names(res.bayes))

res.bayes

#dplyr::arrange(c14age) %>% 
#  dplyr::mutate_at(vars(LABNR), dplyr::funs(factor(., levels=unique(.)))) %>%

#arrange(persistent, score) %>% mutate(id = factor(id, levels=unique(id))) 

# define order of plotting (oldest start median)
order <- res.bayes.median %>%
  dplyr::filter(alpha == "start") %>%
  dplyr::arrange(dplyr::desc(median)) %>%
  dplyr::pull(POTTERY)

# plotting:
ggplot(res.bayes) + 
  ggridges::geom_ridgeline(
    aes(x = -bp + 1950, 
        #y = stats::reorder(POTTERY, 
        #                   median, 
        #                   decreasing = TRUE), 
        y = factor(POTTERY, levels = order),
        height = prob, 
        fill = alpha), 
    scale = 100, 
    color = "NA") + 
  scale_x_continuous("cal BCE/CE", 
                     limits = c(-500, 2020),
                     expand = c(0, 0), 
                     breaks = c(seq(-1000, 1800, 200))) + 
  scale_fill_discrete("", guide = guide_legend(reverse = TRUE)) + 
  theme_bw() +
  theme(legend.position = "top", 
        axis.title.y = element_blank(), 
        panel.grid.minor.x = element_blank())
ggsave("fig/fig_bayesphases.jpg", width = 6, height = 4)
ggsave("fig/fig_bayesphases.pdf", width = 6, height = 4)

# export medians
res.bayes %>% 
  dplyr::distinct(median, POTTERY, alpha) %>%
  dplyr::mutate(median = round(-median + 1950)) %>%
  reshape2::dcast(POTTERY ~ alpha, value.var = "median") %>%
  dplyr::left_join(pottery, by = "POTTERY") %>%
  dplyr::select(POTTERY, FROM, start, TO, end) %>%
  dplyr::arrange(start) %>%
  dplyr::rename("Pottery Group" = POTTERY, 
                "Start aSCAC" = FROM, 
                "Bayesian Start" = start, 
                "End aSCAC" = TO, 
                "Bayesian End" = end) %>%
  write.csv("tbl/tbl_bayesphases_comparison.csv", row.names = F)
