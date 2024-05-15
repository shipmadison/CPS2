
# INSTALL PACKAGES ----------------------------------------------------------------------------------------------------------
# install.packages(c("tidyverse", "gsubfn", "terra", "rgdal", "colorRamps", "sf", "viridis", "grid", "shadowtext",
#                     "ggnewscale"))


# LOAD PACKAGES -------------------------------------------------------------------------------------------------------------
  library(tidyverse)
  library(gsubfn)
  library(terra)
  # library(rgdal)
  library(colorRamps)
  library(sf)
  library(viridis)
  library(grid)
  library(shadowtext)
  library(gstat)
  library(ggnewscale)


# LOAD DATA -----------------------------------------------------------------------------------------------------------------

  # Read in spatial layers for mapping purposes
    # Set crs
      map.crs <- "EPSG:3338"
    
    # Set spatial data filepath
      spatial_path <- "./Data/Spatial Layers/"
    
    # Read in Bristol Bay management shapefile
      survey_gdb <- paste0(spatial_path, "SAP_layers.gdb")
    
      BB_strata <- st_read(survey_gdb,layer="BristolBaySurveyStrata")
    
    # Read in RKCSA + subarea shapefiles
      RKCSA_sub <- st_read(paste0(spatial_path, "RKCSA layers/RKCSA_sub.shp"))
    
      RKCSA <- st_read(paste0(spatial_path, "RKCSA layers/RKCSA.shp"))
      
      # CPS1_bound <- st_read(paste0(spatial_path, "CPS1_survey_boundary.shp"))
      CPS1_bound <- st_read(paste0(spatial_path, "CPS1_survey_boundary.shp")) %>%
                    st_transform(map.crs) #%>%
                    # vect()
      
      CPS1_bathy <- st_read(paste0(spatial_path, "CPS1_project_depth_contour.shp")) %>%
                    st_transform(map.crs)
    
    # Set up plotting features
      map_layers <- readRDS(paste0(spatial_path, "akgfmaps_layers.rds"))
    
      plot.boundary.untrans <- data.frame(y = c(54.5, 58.5), 
                                          x = c(-164.8, -159)) # plot boundary unprojected
      
      plot.boundary <- plot.boundary.untrans %>%
                       sf::st_as_sf(coords = c(x = "x", y = "y"), crs = sf::st_crs(4326)) %>%
                       sf::st_transform(crs = map.crs) %>%
                       sf::st_coordinates() %>%
                       as.data.frame() %>%
                       dplyr::rename(x = X, y = Y) # plot boundary projected

      
# CREATE MAPPING FUNCTION ---------------------------------------------------------------------------------------------------
  
  # method = "POT" or "TRAWL"
  # data = bycatch
  # species = YellowfinSole
  # species_lab = "Yellowfin Sole"
  #   paste(filter(bc_labs, name == "YellowfinSole")$lab)
  # out_name = yfs
  # breaks = count plot breaks
                  
  # bycatch_map <- function(method, data, species, species_lab, out_name, breaks){
  #   
  #   plot <- ggplot() +
  #           geom_sf(data = st_transform(map_layers$bathymetry, map.crs), color=alpha("grey70")) +
  #           geom_sf(data = st_as_sf(BB_strata), fill = NA, mapping = aes(color = "black"), linewidth = 1) +
  #           geom_sf(data = st_as_sf(RKCSA_sub), mapping = aes(color = "red"), fill = NA, alpha = 0.9, linewidth = 1) +
  #           geom_sf(data = st_as_sf(RKCSA), fill = NA,  color = "red", alpha =0.5, linewidth = 1) +
  #           geom_sf(data = st_transform(map_layers$akland, map.crs), fill = "grey80") +
  #           geom_sf(data = data,
  #                   mapping = aes(size = get(species), fill = get(species), shape = get(species) == 0), 
  #                   alpha = 0.5, colour = "black") +
  #           scale_shape_manual(values = c('TRUE' = 4, 'FALSE' = 21), guide = "none") +
  #           scale_color_manual(values = c("black", "red"), 
  #                              labels = c("EBS Summer Survey Boundary", "Red King Crab Savings Area"),
  #                              name = "") +
  #           scale_size_continuous(range = c(2, 10)) + 
  #           scale_fill_gradientn(colors = c("gray", rev(pal[5:length(pal)]))) +
  #           scale_x_continuous(breaks = c(-165, -160), labels = paste0(c(165, 160), "°W"))+
  #           scale_y_continuous(breaks = c(56, 58), labels = paste0(c(56, 58), "°N"))+
  #           labs(title = "2024 BBRKC Collaborative Pot Sampling", subtitle = species_lab)+
  #           guides(size = guide_legend(title.position = "top", title = "COUNT", nrow = 2, 
  #                                      override.aes = list(shape = c(4, rep(21, breaks)))),
  #                  fill = guide_legend(title = "COUNT"),
  #                  color = guide_legend(nrow = 2)) +
  #           coord_sf(xlim = plot.boundary$x,
  #                    ylim = plot.boundary$y) +
  #           geom_sf_text(sf::st_as_sf(data.frame(lab= c("50m", "100m"), 
  #                                                x = c(-161.5, -165), y = c(58.3, 56.1)),
  #                                     coords = c(x = "x", y = "y"), crs = sf::st_crs(4326)) %>%
  #                          sf::st_transform(crs = map.crs),
  #                        mapping = aes(label = lab))+
  #           theme_bw() +
  #           theme(axis.title = element_blank(),
  #                 axis.text = element_text(size = 10),
  #                 legend.text = element_text(size = 10),
  #                 legend.title = element_text(size = 10),
  #                 legend.position = "bottom",
  #                 legend.direction = "horizontal",
  #                 plot.title = element_text(face = "bold", size = 15),
  #                 plot.subtitle = element_text(size = 12))
  #         
  #   # Save figure
  #     ggsave(plot = plot, 
  #            paste0("./Figures/", out_name, "_", method, "_map.png"), 
  #            height = 7, width = 7, units = "in")
  #            
  # }
    