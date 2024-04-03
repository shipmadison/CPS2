
# Read in pot haul data
SB_potlifts <- read.csv(paste0(path, "SB_POTLIFTS.csv")) %>% # Seabrooke
               filter(!is.na(TIME_HAUL), TIME_HAUL != "") %>%
               dplyr::mutate(X = NA)

AL_potlifts <- read.csv(paste0(path, "AL_POTLIFTS.csv")) %>%# Arctic Lady
               filter(!is.na(TIME_HAUL), TIME_HAUL != "")

potlifts <- rbind(SB_potlifts, AL_potlifts) %>%
            filter(!nchar(POT_ID) > 3) # filter out CAM, COFFIN, and BAIT POT_IDs

sampled_pots <- unique(potlifts$POT_ID)

pot_hauls <- read.csv("C:/Users/shannon.hennessey/Downloads/wintersurveycoordinates2023_dd.csv") %>%
             dplyr::rename(LAT_DD = Latitude, LON_DD = Longitude, STATION = Station) %>%
             dplyr::mutate(SAMPLED = ifelse(STATION %in% sampled_pots, 1, 0),
                           METHOD = "POT") %>%
             select(METHOD, STATION, LAT_DD, LON_DD, SAMPLED)


# Read in trawl haul data
trawl_hauls <- read.csv("Y:/KOD_Survey/CPS2/Data/Trawl Data/VA_HAULS.csv") %>%
               dplyr::mutate(METHOD = "TRAWL") %>%
               select(METHOD, STATION, LAT_DD, LON_DD, SAMPLED)


hauls <- rbind(pot_hauls, trawl_hauls)

# Transform bycatch data to correct crs
hauls <- hauls %>%
         sf::st_as_sf(coords = c(x = "LON_DD", y = "LAT_DD"), crs = sf::st_crs(4326)) %>%
         sf::st_transform(crs = map.crs)




# MAP -----------------------------------------------------------------------------------------
# Load mapping layers
source("./Scripts/map_setup.R")

station.map <- ggplot() +
                geom_sf(data = st_transform(map_layers$bathymetry, map.crs), color=alpha("grey70")) +
                geom_sf(data = st_as_sf(BB_strata), fill = NA, mapping = aes(color = "black"), linewidth = 1) +
                geom_sf(data = st_as_sf(RKCSA_sub), mapping = aes(color = "red"), fill = NA, alpha = 0.9, linewidth = 1) +
                geom_sf(data = st_as_sf(RKCSA), fill = NA,  color = "red", alpha =0.5, linewidth = 1) +
                scale_color_manual(values = c("black", "red"),
                                   labels = c("EBS Summer Survey Boundary", "Red King Crab Savings Area"),
                                   name = "") +
                guides(color = guide_legend(nrow = 2)) +
                
                new_scale_color()+
                geom_sf(data = st_transform(map_layers$akland, map.crs), fill = "grey80") +
                
                geom_sf(data = hauls %>% filter(METHOD == "POT"),
                        mapping = aes(shape = SAMPLED == 0), colour = "black", fill = "black") +
                scale_shape_manual(values = c('TRUE' = 4, 'FALSE' = 21), labels = c("Sampled", "Unsampled")) +
              
                # POT legend
                guides(shape = guide_legend(title = "Pot",  title.position = "top", nrow = 2,
                                           override.aes = list(shape = c(21, 4)))) +
                
                # start a new scale
                new_scale("fill") +
                new_scale("shape") +
                # new_scale("size") +
                
                # add TRAWL points
                # **need an ifelse to ID fish vs. crab to plot weight or count in legend...
                geom_sf(data = hauls %>% filter(METHOD == "TRAWL"),
                        mapping = aes(shape = SAMPLED == 0), colour = "darkgreen", fill = "darkgreen") +
                scale_shape_manual(values = c('FALSE' = 24, 'TRUE' = 8), labels = c("Sampled", "Unsampled")) +
              
                scale_x_continuous(breaks = map_layers$lon.breaks) +
                scale_y_continuous(breaks = map_layers$lat.breaks) +
                labs(title = "2024 BBRKC Collaborative Pot Sampling", subtitle = "Sampled and unsampled pot and trawl stations") +
                
                # TRAWL legend
                guides(shape = guide_legend(title = "Trawl", title.position = "top", nrow = 2, 
                                           override.aes = list(shape = c(24, 8)))) +
                
                # crop spatial extent, add bathymetry, set theme
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

# Save figures
  ggsave(plot = station.map, "C:/Users/shannon.hennessey/Work/GitHub/CPS2/Figures/station_sampling_map.png",
         height = 7, width = 7, units = "in")
