
# INSTALL PACKAGES ----------------------------------------------------------------------------------------------------------
# install.packages(c("tidyverse", "gsubfn", "terra", "rgdal", "colorRamps", "sf", "viridis", "grid", "shadowtext",
#                     "ggnewscale"))


# LOAD PACKAGES -------------------------------------------------------------------------------------------------------------
  library(tidyverse)


# LOAD DATA -----------------------------------------------------------------------------------------------------------------

  # Read in POT and TRAWL data
    # I think I need to reframe this to make long rather than wide df...
    pot_bycatch <- read.csv("./Outputs/CPS2_2024_POT_bycatch.csv") %>%
                   dplyr::rename(HAUL = SPN,
                                 STATION = BUOY)
    trawl_bycatch <- read.csv("./Outputs/CPS2_2024_TRAWL_bycatch.csv") 


# MAP BBRKC POT CPUE --------------------------------------------------------------------------------------------------------

  # Combine pot and trawl bycatch into one dataframe
    bycatch <- trawl_bycatch %>% 
               rbind(., pot_bycatch %>% dplyr::select(-c(POT_ID, DATE_SET, TIME_SET, DATE_HAUL, TIME_HAUL, SOAK_TIME, CATCH_PER_HOUR)))
  
  # Transform bycatch data to correct crs
    bycatch <- bycatch %>%
               sf::st_as_sf(coords = c(x = "LON_DD", y = "LAT_DD"), crs = sf::st_crs(4326)) %>%
               sf::st_transform(crs = map.crs)
  
  # Set up shape mapping
    shapes <- c(0, 15) # set shape mapping
    names(shapes) <- c("n=1", "n>1")
    
  # Set up RKC labels
    # mat_labs <- c("Mature female", "Immature female", "Mature male (>= 120mm)", "Immature male (< 120mm)", "Legal male (>= 135mm)", "Sublegal male")
    # names(mat_labs) <- c("Mature female", "Immature female", "Mature male", "Immature male", "Legal male", "Sublegal male")
    
    mat_labs <- data.frame(lab = c("Mature female", "Immature female", "Mature male (>= 120mm)", 
                                   "Immature male (< 120mm)", "Legal male (>= 135mm)", "Sublegal male"),
                           MAT_SEX = c("Mature female", "Immature female", "Mature male", "Immature male","Legal male", "Sublegal male"))
    
  # Specify palette
    pot_pal <- viridis::mako(10) 
    trawl_pal <- viridis::rocket(10)
  
  
  # bycatch_map <- function(method, data, species, species_lab, out_name, breaks){
  # Plot bycatch counts and weights (pot vs. trawl)
    bycatch.maps <- ggplot() +
                    geom_sf(data = st_transform(map_layers$bathymetry, map.crs), color=alpha("grey70")) +
                    geom_sf(data = st_as_sf(BB_strata), fill = NA, mapping = aes(color = "black"), linewidth = 1) +
                    geom_sf(data = st_as_sf(RKCSA_sub), mapping = aes(color = "red"), fill = NA, alpha = 0.9, linewidth = 1) +
                    geom_sf(data = st_as_sf(RKCSA), fill = NA,  color = "red", alpha =0.5, linewidth = 1) +
                    geom_sf(data = st_transform(map_layers$akland, map.crs), fill = "grey80") +
                    geom_sf(data = data,
                            mapping = aes(size = get(species), fill = get(species), shape = get(species) == 0), 
                            alpha = 0.5, colour = "black") +
                    scale_shape_manual(values = c('TRUE' = 4, 'FALSE' = 21), guide = "none") +
                    scale_color_manual(values = c("black", "red"), 
                                       labels = c("EBS Summer Survey Boundary", "Red King Crab Savings Area"),
                                       name = "") +
                    scale_size_continuous(range = c(2, 10)) + 
                    scale_fill_gradientn(colors = c("gray", rev(pal[5:length(pal)]))) +
                    scale_x_continuous(breaks = c(-165, -160), labels = paste0(c(165, 160), "°W"))+
                    scale_y_continuous(breaks = c(56, 58), labels = paste0(c(56, 58), "°N"))+
                    labs(title = "2024 BBRKC Collaborative Pot Sampling", subtitle = species_lab)+
                    guides(size = guide_legend(title.position = "top", title = "COUNT", nrow = 2, 
                                               override.aes = list(shape = c(4, rep(21, breaks)))),
                           fill = guide_legend(title = "COUNT"),
                           color = guide_legend(nrow = 2)) +
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
                          plot.subtitle = element_text(size = 12))
  
  # # Save figures
  #   ggsave(plot = plot, 
  #          paste0("./Figures/", out_name, "_", method, "_map.png"), 
  #          height = 7, width = 7, units = "in")
  
  # Save plots
    plot_labs <- c("matmale", "immale", "matfem", "imfem", "legalmale", "sublegalmale")
    
    for(i in 1:4){
      ggsave(plot = BBRKC.maps[[i]], paste0("./Figures/BBRKC_", plot_labs[i], ".png"), 
             height = 7, width = 10, units = "in")
    }
  
