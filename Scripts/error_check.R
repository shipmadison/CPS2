# ERROR CHECKING -----------------------------------------------------------------------------------------------------

# method: "POT" or "TRAWL"
# potlifts and haul can be NULL depending on the method 
#  (still need to test this...)


# Load mapping layers
  source("./Scripts/map_setup.R")

# Write function
error_chk <- function(method, specimen_table, catch_summary, potlifts, haul, cpue){
  
  print("CHECKING VESSEL AND CRUISE...")
  
  # 1) Does cruise number match 202401?"
    if(FALSE %in% unique(specimen_table$CRUISE == 202301)){
      print("ERROR: wrong cruise number")
    }
  
  
  # "2) Does the vessel # match the vessels utilized in the survey?"
    if(method == "POT"){
      if(FALSE %in% (unique(specimen_table$VESSEL) %in% c("Arctic Lady", "Seabrooke")) == TRUE){
        print("ERROR: vessel numbers entered do not match survey vessels")
      }
    }
  
    if(method == "Trawl"){
      if(FALSE %in% (unique(specimen_table$VESSEL) %in% c("Vesteraalen")) == TRUE){
        print("ERROR: vessel numbers entered do not match survey vessels")
      }
    }
    
    print("Inventory of catch by cruise and vessel")
    invent1 <- specimen_table %>%
      dplyr::group_by(VESSEL, CRUISE) %>%
      dplyr::reframe(COUNT = sum(SAMPLING_FACTOR),
                     N = n()) %>%
      as.data.frame()
    print(invent1)
  
  
  print("CHECKING SPECIES CODES AND SEX...")
  
  
 ## ** SH NOTE: need to incorporate crab species into this!! ** ----------
  # 3) Do species codes match RKC code 69322?"
    if(FALSE %in% unique(specimen_table$SPECIES_CODE == 69322)){
      print("ERROR: wrong code entered for RKC")
    }
  
  
  # 4) Are sex codes assigned to either 1 or 2?"
    if(unique(specimen_table$SEX %in% c(1:2)) == FALSE){
      print("ERROR: sex code not 1 or 2")
    }
  
    print("Inventory of catch by cruise, vessel, species code, and sex")
    invent2 <- specimen_table %>%
      dplyr::group_by(VESSEL, CRUISE, SPECIES_CODE, SEX) %>%
      dplyr::reframe(COUNT = sum(SAMPLING_FACTOR), 
                     N = n()) %>%
      as.data.frame()
    print(invent2)
  
  
  print("CHECKING SHELL CONDITION, EGG COLOR, EGG CONDITION, CLUTCH_SIZE...")
  
  # 5) Are egg color codes valid for females?"
    if(FALSE %in% (unique(filter(specimen_table, SEX != 1)$EGG_COLOR %in% c(0, 2:6))) == TRUE){
      print("ERROR: invalid female egg color code (not 0 or 2:6)")
    }
  
  
  # 6) Are egg condition codes valid for females?"
    if(FALSE %in% (unique(filter(specimen_table, SEX != 1)$EGG_CONDITION %in% c(0:5))) == TRUE){
      print("ERROR: invalid female egg condition code (not 0:5)")
    } 
  
  
  # 7) Are clutch size codes valid for females?
    if(FALSE %in% (unique(filter(specimen_table, SEX != 1)$CLUTCH_SIZE %in% c(0:6, 999))) == TRUE){
      print("ERROR: invalid female clutch size code (not 0:6)")
    }
  
  
  # 8) Any egg, egg condition, or clutch size codes assigned to males?
    if(TRUE %in% (unique((specimen_table$SEX == 1 & is.na(specimen_table$EGG_COLOR | specimen_table$EGG_CONDITION | 
                                                          specimen_table$CLUTCH_SIZE) == FALSE))) == TRUE){
      print("ERROR: egg, egg condition, or clutch size code assigned to male")
    } 
    
  
  # 9) Any questionable egg condition x shell condition combinations for females?"
  # Checking shell condition = 0 and egg condition = 1
    if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION == 0 & 
                         filter(specimen_table, SEX != 1)$EGG_CONDITION == 1)) == TRUE){
      print("ERROR: female with shell condition = 0 and egg condition = 1")
    } 
  
  # Checking shell condition = 1 and egg condition >1
    if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION == 1 & 
                         filter(specimen_table, SEX != 1)$EGG_CONDITION > 1)) == TRUE){
      print("ERROR: female with shell condition = 1 and egg condition >1")
    }
  
  # Checking shell condition = 3, 4, or 5 and egg condition = 1
    if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION %in% c(3:5) & 
                         filter(specimen_table, SEX != 1)$EGG_CONDITION == 1)) == TRUE){
      print("ERROR: female with shell condition 3:5 and egg condition = 1")
    }
  
  # Checking shell condition = 1 and egg condition >=2
    if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION == 1 & 
                         filter(specimen_table, SEX != 1)$EGG_CONDITION >= 2)) == TRUE){
      print("ERROR: female with shell condition = 1 and egg condition >=2")
    }
    
  
  # 10) Any females without egg color, egg condition, or clutch codes?
    if(TRUE %in% (unique(specimen_table$SEX == 2 & (is.na(specimen_table$EGG_COLOR | specimen_table$EGG_CONDITION 
                                                          | specimen_table$CLUTCH_SIZE) == TRUE))) == TRUE){
      print("ERROR: female missing egg color, egg condition, or clutch code")
    }
  
  
    print("Inventory of shell condition")
    invent3 <- specimen_table %>%
      dplyr::group_by(SPECIES_CODE, SEX, SHELL_CONDITION) %>%
      dplyr::reframe(COUNT = sum(SAMPLING_FACTOR),
                     N = n()) %>%
      as.data.frame()
    print(invent3)
    
    print("Inventory of female shell condition and egg codes")
    invent4 <- specimen_table %>%
      dplyr::filter(SEX == 2) %>%
      dplyr::group_by(SPECIES_CODE, SHELL_CONDITION, EGG_COLOR, EGG_CONDITION, CLUTCH_SIZE) %>%
      dplyr::reframe(COUNT = sum(SAMPLING_FACTOR),
                     N = n()) %>%
      as.data.frame()
    print(invent4)
  
  
  print("CHECKING LENGTHS AND WIDTHS...")
  
  
 ## ** SH NOTE: need to incorporate crab species into this!! ** ----------
  # 11) Any missing lengths for RKC?"
    if(unique(is.na(specimen_table$LENGTH)) == TRUE){
      print("ERROR: missing length for RKC")
    } 
  
  
  # 12) Any small female crab with a clutch size?"
    if(unique(filter(specimen_table, SEX != 1)$LENGTH < 65 & 
              filter(specimen_table, SEX != 1)$CLUTCH_SIZE > 0) == TRUE){
      print("ERROR: female <65 with clutch size >0")
    } 
  
  
  # 13) Any small crab with old shell condition?"
    if(TRUE %in% unique(specimen_table$LENGTH < 60 & specimen_table$SHELL_CONDITION > 2)){
      print("ERROR: crab < 60 with shell condition >2")
    } 
  
  
 ## ** SH NOTE: need to incorporate crab species into this!! ** ----------
  # 14) Any widths entered for RKC? 
    if(unique(is.na(specimen_table$WIDTH)) == FALSE) {
      print("ERROR: width entered for RKC when length is needed")
    }
  
  
  # 15) Minimum and maximum lengths by sex?
    print("What are the minimum and maximum lengths reported by sex?")
    A15 <- specimen_table %>%
           dplyr::group_by(SPECIES_CODE, SEX) %>%
           dplyr::reframe(MIN_LENGTH = min(LENGTH),
                          MAX_LENGTH = max(LENGTH)) %>%
           as.data.frame()
    print(A15)
  
  
  print("CHECKING DISEASE CODES...")
  
  # 16) Any black mat recorded but without % coverage entry?
    if(unique(is.na(specimen_table$DISEASE_CODE) == FALSE &
              specimen_table$DISEASE_CODE == 1 & (is.na(specimen_table$DISEASE_DORSAL) == TRUE &
                                                  is.na(specimen_table$DISEASE_LEGS) == TRUE &
                                                  is.na(specimen_table$DISEASE_VENTRAL) == TRUE)) == TRUE){
      print("ERROR: black mat recorded without % coverage")
    } 
  
  
  # 17) Any bitter crab recorded for RKC and/or any bitter crab recorded with entries in % coverage?
    if(unique(is.na(specimen_table$DISEASE_CODE) == FALSE & specimen_table$DISEASE_CODE == 2 |
              is.na(specimen_table$DISEASE_CODE) == FALSE & specimen_table$DISEASE_CODE == 2 & 
              (is.na(specimen_table$DISEASE_DORSAL) == FALSE &
               is.na(specimen_table$DISEASE_LEGS) == FALSE &
               is.na(specimen_table$DISEASE_VENTRAL) == FALSE)) == TRUE){
      print("ERROR: bitter crab recorded for RKC and/or bitter crab recorded with % coverage")
    } 
  
  
  # 18) Any disease code not recorded but entries in % coverage?
    if(unique(is.na(specimen_table$DISEASE_CODE) == TRUE & (is.na(specimen_table$DISEASE_DORSAL) == FALSE &
                                                            is.na(specimen_table$DISEASE_LEGS) == FALSE &
                                                            is.na(specimen_table$DISEASE_VENTRAL) == FALSE)) == TRUE){
      print("ERROR: disease code not recorded but % cover entered")
    } 
  
  
  # 19) Any disease codes >9? 
    if(unique(is.na(specimen_table$DISEASE_CODE) == "FALSE" & specimen_table$DISEASE_CODE > 9) == TRUE){
      print("ERROR: disease code >9")
    }
  
  
  print("CHECKING SAMPLING FACTOR...")
  
  # 20) What is the maximum sampling factor by by sex?
    print("What is the maximum sampling factor by sex?")
    A20 <- specimen_table %>%
           dplyr::group_by(SPECIES_CODE, SEX) %>%
           dplyr::reframe(MIN_SAMPLING_FACTOR = min(SAMPLING_FACTOR),
                          MAX_SAMPLING_FACTOR= max(SAMPLING_FACTOR)) %>%
           as.data.frame() 
    print(A20)
  
    
  # 21) Any sampling factors < 1?"
    if(unique(specimen_table$SAMPLING_FACTOR < 1) == TRUE){
      print("ERROR: minimum sampling factor < 1")
    } 
  
  
  print("CHECKING HAUL, STATION, AND BUOY IDs...")
    if(method == "POT"){
      xx <- cpue %>%
            dplyr::select(VESSEL, SPN, POT_ID, BUOY, LAT_DD, LON_DD) %>%
            distinct() %>%
            as.data.frame()
      
      yy <- potlifts %>%
            dplyr::select(VESSEL, SPN, POT_ID, BUOY, LAT_DD, LON_DD) %>%
            distinct()
      
      if(TRUE %in% is.na(suppressMessages(right_join(xx, yy, keep = TRUE))) == TRUE){
        print("ERROR: POT: pot, station, and/or buoy IDs do not match between potlifts table and pot cpue table")
      }
    }
      
      if(method == "TRAWL"){
        xx <- cpue %>%
              dplyr::select(VESSEL, HAUL, STATION, LAT_DD, LON_DD) %>%
              distinct() %>%
              as.data.frame()
        
        yy <- haul %>%
              dplyr::select(STATION, LAT_DD, LON_DD) %>%
              distinct()
        
        if(TRUE %in% is.na(suppressMessages(right_join(xx, yy, keep = TRUE))) == TRUE){
          print("ERROR: TRAWL: haul and/or station IDs do not match between haul table and cpue table")
        }
      }
  
  
  print("COMPARING SPECIMEN TABLE WITH CATCH SUMMARY...")
  
  # 22) Does the number of crab and number of entries match between the specimen table and catch summary?
    if(method == "POT"){
      spec_sum <- specimen_table %>%
                  dplyr::group_by(CRUISE, VESSEL, POT_ID, SPECIES_CODE) %>%
                  dplyr::reframe(NUMBER_CRAB = sum(SAMPLING_FACTOR),
                                 N_ENTRIES = n())
      
      catch_sum <- catch_summary %>%
                   dplyr::select(CRUISE, VESSEL, POT_ID, SPECIES_CODE, NUMBER_CRAB, N_ENTRIES)
      
      if(TRUE %in% is.na(suppressMessages(right_join(spec_sum, catch_sum))) == TRUE){
        print("ERROR: POT: number of crab and number of entries do not 
                 match between summarized specimen table and catch summary")
      }
    } 

    if(method == "TRAWL"){
      spec_sum <- specimen_table %>%
        dplyr::group_by(CRUISE, VESSEL, HAUL, SPECIES_CODE) %>%
        dplyr::reframe(NUMBER_CRAB = sum(SAMPLING_FACTOR),
                       N_ENTRIES = n())
      
      catch_sum <- catch_summary %>%
        dplyr::select(CRUISE, VESSEL, HAUL, SPECIES_CODE, NUMBER_CRAB, N_ENTRIES)
      
      if(TRUE %in% is.na(suppressMessages(right_join(spec_sum, catch_sum))) == TRUE){
        print("ERROR: TRAWL: number of crab and number of entries do not 
                   match between summarized specimen table and catch summary")
      } 
    }
  
  
  print("CHECKING COORDINATES...")
  
  if(method == "POT"){
    # Transform potlifts data to correct crs
    mapdat <- potlifts %>%
              sf::st_as_sf(coords = c(x = "LON_DD", y = "LAT_DD"), crs = sf::st_crs(4326)) %>%
              sf::st_transform(crs = map.crs)
    
    # Map coordinates
    coords <- ggplot() +
              geom_sf(data = st_as_sf(BB_strata), fill = NA, color = "black", linewidth = 1) +
              geom_sf(data = st_as_sf(RKCSA_sub), fill = NA, color = "red", alpha = 0.5, linewidth = 1) +
              geom_sf(data = st_as_sf(RKCSA), fill = NA, color = "red", alpha = 0.5, linewidth = 1) +
              geom_sf(data = mapdat, shape = 19, size = 1.5, colour = "black", stat = "identity", 
                      position = "identity")+
              ggtitle("Pot coordinate check")+
              theme_bw()
    
    print(coords)
  }
  
  if(method == "TRAWL"){
    # Transform haul data to correct crs
    mapdat <- haul %>%
              sf::st_as_sf(coords = c(x = "LON_DD", y = "LAT_DD"), crs = sf::st_crs(4326)) %>%
              sf::st_transform(crs = map.crs)
    
    # Map coordinates
    coords <- ggplot() +
              geom_sf(data = st_as_sf(BB_strata), fill = NA, color = "black", linewidth = 1) +
              geom_sf(data = st_as_sf(RKCSA_sub), fill = NA, color = "red", alpha = 0.5, linewidth = 1) +
              geom_sf(data = st_as_sf(RKCSA), fill = NA, color = "red", alpha = 0.5, linewidth = 1) +
              geom_sf(data = mapdat, shape = 19, size = 1.5, colour = "black", stat = "identity", 
                      position = "identity")+
              ggtitle("Trawl coordinate check")+
              theme_bw()
    
    print(coords)
  }
}    


