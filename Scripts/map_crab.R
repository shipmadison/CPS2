
# INSTALL PACKAGES ----------------------------------------------------------------------------------------------------------
# install.packages(c("tidyverse", "gsubfn", "terra", "rgdal", "colorRamps", "sf", "viridis", "grid", "shadowtext",
#                     "ggnewscale"))


# LOAD PACKAGES -------------------------------------------------------------------------------------------------------------
  library(tidyverse)


# LOAD DATA -----------------------------------------------------------------------------------------------------------------

  # Read in POT and TRAWL data
    pot_cpue <- read.csv("./Outputs/CPS2_2024_potcatch.csv") %>%
                dplyr::rename(HAUL = SPN,
                              STATION = BUOY)
    
    pot_bycatch <- read.csv("./Outputs/CPS2_2024_POT_bycatch.csv")
    
    trawl_cpue <- read.csv("./Outputs/CPS2_2024_trawlcatch.csv")
    # trawl_bycatch <- read.csv("./Outputs/CPS2_2024_TRAWL_bycatch.csv")
    
    
# MAP BBRKC POT CPUE --------------------------------------------------------------------------------------------------------
    
  # Combine pot and trawl counts into one dataframe
    cpue <- trawl_cpue %>% 
            rbind(., pot_cpue %>% dplyr::select(-c(POT_ID, DATE_SET, TIME_SET, DATE_HAUL, TIME_HAUL, SOAK_TIME, CATCH_PER_HOUR)))
    
    tot_cpue <- cpue %>%
                      dplyr::filter(MAT_SEX %in% c("Mature male", "Immature male", "Mature female", "Immature female")) %>%
                      dplyr::group_by(VESSEL, STATION, LAT_DD, LON_DD) %>%
                      dplyr::summarise(COUNT = sum(COUNT))
    
  # Transform catch data to correct crs
    cpue_mapdat <- cpue %>%
                       sf::st_as_sf(coords = c(x = "LON_DD", y = "LAT_DD"), crs = sf::st_crs(4326)) %>%
                       sf::st_transform(crs = map.crs)
    
    tot_cpue_mapdat <- tot_cpue %>%
                      sf::st_as_sf(coords = c(x = "LON_DD", y = "LAT_DD"), crs = sf::st_crs(4326)) %>%
                      sf::st_transform(crs = map.crs)
    
    # max.date <- max(pot_cpue_mapdat$DATE_HAUL) # label for most recent pot haul date
    
  # Transform bycatch data to correct crs
    pot_bycatch <- pot_bycatch %>%
                   sf::st_as_sf(coords = c(x = "LON_DD", y = "LAT_DD"), crs = sf::st_crs(4326)) %>%
                   sf::st_transform(crs = map.crs)

  # Set up shape mapping
    shapes <- c(0,15) # set shape mapping
    names(shapes) <- c("n=1", "n>1")

  # Set up RKC labels
    mat_labs <- c("Mature female", "Immature female", "Mature male (>= 120mm)", "Legal male (>= 135mm)", "Immature male (< 120mm)", "Sublegal male")
    names(mat_labs) <- c("Mature female", "Immature female", "Mature male", "Legal male", "Immature male", "Sublegal male")
    
    mat_labs <- data.frame(lab = c("Mature female", "Immature female", "Mature male (>= 120mm)", 
                                   "Legal male (>= 135mm)", "Immature male (< 120mm)", "Sublegal male"),
                           MAT_SEX = c("Mature female", "Immature female", "Mature male", "Legal male", "Immature male", "Sublegal male"))
    
  # Specify palette
    pot_pal <- viridis::mako(10) # set palette
    trawl_pal <- viridis::rocket(10)
      
    
    library(ggh4x)
    
  # Plot total counts
    # total.map <-  
      ggplot() +
                   geom_sf(data = st_transform(map_layers$bathymetry, map.crs), color=alpha("grey70")) +
                   geom_sf(data = st_as_sf(BB_strata), fill = NA, mapping = aes(color = "black"), linewidth = 1) +
                   geom_sf(data = st_as_sf(RKCSA_sub), mapping = aes(color = "red"), fill = NA, alpha = 0.9, linewidth = 1) +
                   geom_sf(data = st_as_sf(RKCSA), fill = NA,  color = "red", alpha =0.5, linewidth = 1) +
                  scale_color_manual(values = c("black", "red"),
                                     labels = c("EBS Summer Survey Boundary", "Red King Crab Savings Area"),
                                     name = "") +
                  guides(color = guide_legend(nrow = 2)) +
                   geom_sf(data = st_transform(map_layers$akland, map.crs), fill = "grey80") +
                  # Plot POT points
                   geom_sf(data = tot_cpue_mapdat %>% filter(VESSEL %in% c("Arctic Lady", "Seabrooke")),
                           mapping = aes(size = COUNT, fill = COUNT, shape = COUNT == 0), 
                           alpha = 0.5, colour = "black") +
                   scale_shape_manual(values = c('TRUE' = 4, 'FALSE' = 21), guide = "none") +
                   scale_size_continuous(range = c(2, 10), limits = c(0, 125), 
                                         breaks = seq(0, 125, by = 25)) + 
                   scale_fill_gradientn(breaks = seq(0, 125, by = 25),
                                        limits = c(0, 125), 
                                        colors = c("gray", rev(pot_pal[5:length(pot_pal)]))) +
                  # new_scale("color") +
                  # POT legend
                  guides(size = guide_legend(title = "Pot Count", title.position = "top", nrow = 2, 
                                             override.aes = list(shape = c(4, rep(21, 5)))),
                         fill = guide_legend(),
                         color = guide_legend(title = "Pot Count", title.position = "top",nrow = 2)) +
                  # start a new scale
                  new_scale_fill() +
                  new_scale("shape") +
                  new_scale("size") +
                  # new_scale("color") +
                  # Plot TRAWL points
                  geom_sf(data = tot_cpue_mapdat %>% filter(VESSEL %in% c("Vesteraalen")),
                          mapping = aes(size = COUNT, fill = COUNT, shape = COUNT == 0), 
                          alpha = 0.5, colour = "black") +
                  scale_shape_manual(values = c('TRUE' = 8, 'FALSE' = 24), guide = "none") +
                  scale_size_continuous(range = c(2, 10), limits = c(0, 25),
                                        breaks = seq(0, 25, by = 5)) +
                  scale_fill_gradientn(breaks = seq(0, 25, by = 5),
                                       limits = c(0, 25), 
                                       colors = c("gray", rev(trawl_pal[5:length(trawl_pal)]))) +
                   scale_x_continuous(breaks = map_layers$lon.breaks) +
                   scale_y_continuous(breaks = map_layers$lat.breaks) +
                   labs(title = "2024 BBRKC Collaborative Pot Sampling", subtitle = "Total BBRKC") +
                  # TRAWL legend
                   guides(size = guide_legend(title = "Trawl Count", title.position = "top", nrow = 2, 
                                              override.aes = list(shape = c(8, rep(24,5)))),
                          fill = guide_legend(title = "Trawl Count", title.position = "top"),
                          color = guide_legend(nrow = 2)) +
                   coord_sf(xlim = plot.boundary$x,
                            ylim = plot.boundary$y) +
                   geom_sf_text(sf::st_as_sf(data.frame(lab = c("50m", "100m"), 
                                                        x = c(-161.5, -165), y = c(58.3, 56.1)),
                                             coords = c(x = "x", y = "y"), crs = sf::st_crs(4326)) %>%
                                  sf::st_transform(crs = map.crs),
                                mapping = aes(label = lab)) +
                   theme_bw() +
                   theme(axis.title = element_blank(),
                         axis.text = element_text(size = 10),
                         legend.text = element_text(size = 10),
                         legend.title = element_text(size = 10),
                         legend.position = "right",
                         legend.direction = "horizontal",
                         plot.title = element_text(face = "bold", size = 15),
                         plot.subtitle = element_text(size = 12))

    ggsave(plot = total.map, "./Figures/BBRKC_TOTAL.png", 
           height = 7, width = 10, units = "in")
  
  # # Plot counts
  #   BBRKC.maps <- mat_sex_combos %>%
  #                 purrr::map(~ ggplot() +
  #                              geom_sf(data = st_transform(map_layers$bathymetry, map.crs), color=alpha("grey70")) +
  #                              geom_sf(data = st_as_sf(BB_strata), fill = NA, mapping = aes(color = "black"), linewidth = 1) +
  #                              geom_sf(data = st_as_sf(RKCSA_sub), mapping = aes(color = "red"), fill = NA, alpha= 0.9, linewidth = 1) +
  #                              geom_sf(data = st_as_sf(RKCSA), fill = NA,  color = "red", alpha =0.5, linewidth = 1) +
  #                              geom_sf(data = st_transform(map_layers$akland, map.crs), fill = "grey80") +
  #                              geom_sf(data = filter(pot_cpue_mapdat, MAT_SEX == .x),
  #                                      mapping = aes(size=COUNT, fill = COUNT, shape = COUNT == 0), alpha = 0.5, colour = "black")+
  #                              scale_shape_manual(values = c('TRUE' = 4, 'FALSE' = 21), guide = "none")+
  #                              scale_color_manual(values = c("black", "red"), 
  #                                                 labels = c("EBS Summer Survey Boundary", "Red King Crab Savings Area"),
  #                                                 name = "") +
  #                              scale_size_continuous(range = c(2, 10), limits = c(0, max(pot_cpue_mapdat$COUNT)), 
  #                                                    breaks = seq(0, max(pot_cpue_mapdat$COUNT), by = 25))+ 
  #                              scale_fill_gradientn(breaks = seq(0, max(pot_cpue_mapdat$COUNT), by = 25),
  #                                                   limits = c(0, max(pot_cpue_mapdat$COUNT)), 
  #                                                   colors = c("gray", rev(pal[5:length(pal)])))+
  #                              scale_x_continuous(breaks = map_layers$lon.breaks)+
  #                              scale_y_continuous(breaks = map_layers$lat.breaks)+
  #                              labs(title = "2023 BBRKC Collaborative Pot Sampling", subtitle = paste(filter(mat_labs, MAT_SEX == .x)$lab))+
  #                              guides(size = guide_legend(title.position = "top", nrow = 2, override.aes = list(shape = c(4, rep(21, 5)))),
  #                                     fill = guide_legend(),
  #                                     color = guide_legend(nrow = 2))+
  #                              coord_sf(xlim = plot.boundary$x,
  #                                       ylim = plot.boundary$y) +
  #                              geom_sf_text(sf::st_as_sf(data.frame(lab= c("50m", "100m"), 
  #                                                                   x = c(-161.5, -165), y = c(58.3, 56.1)),
  #                                                        coords = c(x = "x", y = "y"), crs = sf::st_crs(4326)) %>%
  #                                             sf::st_transform(crs = map.crs),
  #                                           mapping = aes(label = lab))+
  #                              theme_bw() +
  #                              theme(axis.title = element_blank(),
  #                                    axis.text = element_text(size = 10),
  #                                    legend.text = element_text(size = 10),
  #                                    legend.title = element_text(size = 10),
  #                                    legend.position = "bottom",
  #                                    legend.direction = "horizontal",
  #                                    plot.title = element_text(face = "bold", size = 15),
  #                                    plot.subtitle = element_text(size = 12)))
  
    
    # Plot counts
    BBRKC.maps <- mat_sex_combos %>%
      purrr::map(~ ggplot() +
                   geom_sf(data = st_transform(map_layers$bathymetry, map.crs), color=alpha("grey70")) +
                   geom_sf(data = st_as_sf(BB_strata), fill = NA, mapping = aes(color = "black"), linewidth = 1) +
                   geom_sf(data = st_as_sf(RKCSA_sub), mapping = aes(color = "red"), fill = NA, alpha= 0.9, linewidth = 1) +
                   geom_sf(data = st_as_sf(RKCSA), fill = NA,  color = "red", alpha =0.5, linewidth = 1) +
                   geom_sf(data = st_transform(map_layers$akland, map.crs), fill = "grey80") +
                   geom_sf(data = filter(cpue_mapdat, MAT_SEX == .x),
                           mapping = aes(size = COUNT, fill = COUNT, shape = COUNT == 0), alpha = 0.5, colour = "black")+
                   scale_shape_manual(values = c('TRUE' = 4, 'FALSE' = 21), guide = "none")+
                   scale_color_manual(values = c("black", "red"), 
                                      labels = c("EBS Summer Survey Boundary", "Red King Crab Savings Area"),
                                      name = "") +
                   scale_size_continuous(range = c(2, 10), limits = c(0, 125), 
                                         breaks = seq(0, 125, by = 25))+ 
                   scale_fill_gradientn(breaks = seq(0, 125, by = 25),
                                        limits = c(0, 125), 
                                        colors = c("gray", rev(pal[5:length(pal)])))+
                   scale_x_continuous(breaks = map_layers$lon.breaks)+
                   scale_y_continuous(breaks = map_layers$lat.breaks)+
                   labs(title = "2024 BBRKC Collaborative Pot Sampling", subtitle = paste(filter(mat_labs, MAT_SEX == .x)$lab))+
                   guides(size = guide_legend(title.position = "top", nrow = 2, override.aes = list(shape = c(4, rep(21, 5)))),
                          fill = guide_legend(),
                          color = guide_legend(nrow = 2))+
                   coord_sf(xlim = plot.boundary$x,
                            ylim = plot.boundary$y) +
                   geom_sf_text(sf::st_as_sf(data.frame(lab= c("50m", "100m"), 
                                                        x = c(-161.5, -165), y = c(58.3, 56.1)),
                                             coords = c(x = "x", y = "y"), crs = sf::st_crs(4326)) %>%
                                  sf::st_transform(crs = map.crs),
                                mapping = aes(label = lab))+
                   theme_bw() +
                   theme(axis.title = element_blank(),
                         axis.text = element_text(size = 10),
                         legend.text = element_text(size = 10),
                         legend.title = element_text(size = 10),
                         legend.position = "bottom",
                         legend.direction = "horizontal",
                         plot.title = element_text(face = "bold", size = 15),
                         plot.subtitle = element_text(size = 12)))
    
    
    
  # Save plots
    ggsave(plot = BBRKC.maps[[1]], "./Figures/BBRKC_matmale.png", height=7, width=7, units="in")
    ggsave(plot = BBRKC.maps[[2]], "./Figures/BBRKC_immale.png", height=7, width=7, units="in")
    ggsave(plot = BBRKC.maps[[3]], "./Figures/BBRKC_matfem.png", height=7, width=7, units="in")
    ggsave(plot = BBRKC.maps[[4]], "./Figures/BBRKC_imfem.png", height=7, width=7, units="in")
    # ggsave(plot = BBRKC.maps[[5]], "./Figures/BBRKC_legalmale.png", height=7, width=7, units="in")
    # ggsave(plot = BBRKC.maps[[6]], "./Figures/BBRKC_sublegalmale.png", height=7, width=7, units="in")
    
    
  # Plot bycatch species counts
    bycatch_map(method = "POT", data = pot_bycatch,  species = "YellowfinSole",  species_lab = "Yellowfin Sole",  out_name = "yfs", breaks = 4)
    bycatch_map(method = "POT", data = pot_bycatch,  species = "PacificCod",  species_lab = "Pacific Cod",  out_name = "pcod", breaks = 3)
    bycatch_map(method = "POT", data = pot_bycatch,  species = "Tanner",  species_lab = "Tanner Crab",  out_name = "tanner", breaks = 3)
    bycatch_map(method = "POT", data = pot_bycatch,  species = "Snow",  species_lab = "Snow Crab",  out_name = "snow", breaks = 6)
    
