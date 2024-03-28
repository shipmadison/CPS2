# PURPOSE -------------------------------------------------------------------------------------------------------------------
  # 1) To automate processing pot data specimen tables and catch summaries from Collaborative Pot 
  #    Sampling II (CPS2) 2024 for BBRKC
  # 2) To run error checks on processed specimen and catch summaries
  # 3) To calculate and map cpue by pot and maturity/sex category for BBRKC

  # Authors: Shannon Hennessey, Emily Ryznar, NOAA-AFSC


# INSTALL PACKAGES ----------------------------------------------------------------------------------------------------------
  #install.packages("tidyverse")


# LOAD PACKAGES -------------------------------------------------------------------------------------------------------------
  library(tidyverse)


# LOAD DATA -----------------------------------------------------------------------------------------------------------------
  
  # Set pot data filepath
      # path <- "./Data/Pot Data/"
      path <- "Y:/KOD_Survey/CPS2/Data/Pot Data/"

  # Load summary catch and specimen tables
      # **delete catch and specimen files 0021, 0022, 0023, 0056-0059 (AL) because no data
      # Load complete files first, need to remove STATION because not present in "UNRESOLVED" catch files
      catch <- list.files(paste0(path, "Catch - FTP/"), pattern = "SAM 78|SAM 92") %>% 
               purrr::map_df(~read.csv(paste0(path, "Catch - FTP/", .x)) %>% 
                             mutate(STATION = paste0("X", STATION))) %>% 
               dplyr::select(-c(STATION))
      
      # Load "UNRESOLVED" and SAM_90 catch files, bind to above
      catch <- list.files(paste0(path, "Catch - FTP/"), pattern = "UNRESOLVED|SAM_90") %>% # delete files 0001, 0020, 0051-0055 because no data
               purrr::map_df(~read.csv(paste0(path, "Catch - FTP/", .x)) %>%
                             mutate(RECORDING_DEVICE = "a1f09530ae1081b6")) %>% # replace recording device to match ones in raw_specimen_bio
               rbind(catch)
        
      specimen <- list.files(paste0(path, "Specimen - FTP/")) %>%
                  purrr::map_df(~read.csv(paste0(path, "Specimen - FTP/", .x)) %>% 
                                mutate(STATION = paste0("X", STATION)))
      
  # Load raw data for processing below   
      raw_sample <- list.files(paste0(path, "Raw Data - FTP/"), pattern = "_SAMPLE_0") %>% # RECORDS of SAMPLE INFO
                    purrr::map_df(~ read.csv(paste0(path, "Raw Data - FTP/", .x))) #E.G. SEX, SPECIES
      
      raw_sample_values <- list.files(paste0(path, "Raw Data - FTP/"), pattern = "_SAMPLE_VALUES") %>% #RECORDS OF # TOSSED
                           purrr::map_df(~ read.csv(paste0(path, "Raw Data - FTP/", .x))) %>%
                           mutate(TOSSED = ifelse(is.na(COUNT) == FALSE, COUNT,0)) %>%
                           group_by(HAUL_ID, CATCH_SAMPLE_ID) %>%
                           dplyr::reframe(TOSSED = sum(TOSSED)) 
      
      raw_specimen <- list.files(paste0(path, "Raw Data - FTP/"), pattern = "_SPECIMEN_0") %>% 
                      purrr::map_df(~read.csv(paste0(path, "Raw Data - FTP/", .x))) %>%
                      dplyr::select(HAUL_ID, SPECIMEN_ID, CATCH_SAMPLE_ID, SPECIES_CODE) 
      
      raw_specimen_bio <- list.files(paste0(path, "Raw Data - FTP/"), pattern = "_SPECIMEN_BIOMETRICS") %>% 
                          purrr::map_df(~read.csv(paste0(path, "Raw Data - FTP/", .x))) %>%
                          left_join(., catch %>% select(HAUL, HAUL_ID, RECORDING_DEVICE)) 
                        
  # Read in potlifts data
      SB_potlifts <- read.csv(paste0(path, "SB_POTLIFTS.csv")) %>% # Seabrooke
                     filter(!is.na(TIME_HAUL),
                            TIME_HAUL != "") %>%
                     dplyr::rename(GEAR_CODE = GEAR_Code) %>%
                     dplyr::mutate(TIME_SET = format(strptime(paste0(mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(TIME_SET)), TIME_SET), format="%H%M"), format = "%H:%M"),
                                   TIME_HAUL = format(strptime(paste0(mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(TIME_HAUL)), TIME_HAUL), format="%H%M"), format = "%H:%M"))
      
      AL_potlifts <- read.csv(paste0(path, "AL_POTLIFTS.csv")) %>%# Arctic Lady
                     filter(!is.na(TIME_HAUL),
                            TIME_HAUL != "") %>%
                     # dplyr::filter(SPN %in% 1:339) %>% #Hardcoding SS data to removing hotspot tagging sites and Gear code 42
                     dplyr::rename(GEAR_CODE = GEAR_Code) %>%
                     dplyr::mutate(SPN = as.integer(SPN), GEAR_CODE = ifelse(GEAR_CODE == 42, 42, "")) #%>%
                     # dplyr::select(!c(ADFG.Logger, Cory.temp, X.1, X.2, X.3, X)) # Silver Spray
      
      potlifts <- rbind(SB_potlifts, AL_potlifts) %>%
                  filter(DATE_HAUL != "", is.na(VESSEL) == "FALSE" & is.na(GEAR_CODE) == TRUE | GEAR_CODE == 44 | GEAR_CODE == "") %>%
                  mutate(BUOY = paste0("X", BUOY))
      
      
# PROCESS DATA ----------------------------------------------------------------------------------------------------------------
    
  # Calculate soak time and lat/lon in degrees decimal for all potlifts, omit bad or gear testing potlifts based on gear code
      potlifts <- potlifts %>%
                  dplyr::mutate(DATETIME_SET = as.POSIXct(paste(DATE_SET, TIME_SET), format = "%m/%d/%Y %H:%M"),
                                DATETIME_HAUL = as.POSIXct(paste(DATE_HAUL, TIME_HAUL), format = "%m/%d/%Y %H:%M"),
                                SOAK_TIME = as.numeric(difftime(DATETIME_HAUL, DATETIME_SET, units = "hours")),
                                LAT_DD = LAT_DEG + LAT_MIN/60,
                                LON_DD = (LON_DEG + LON_MIN/60)*-1) %>%
                  dplyr::select(!c(DATETIME_SET, DATETIME_HAUL)) 
    
  # Join raw_sample_values and raw_sample to get # tossed per haul, sex, and catch sample id
      samples <- right_join(raw_sample, raw_sample_values) %>%
                 right_join(., catch %>% select(HAUL, HAUL_ID, RECORDING_DEVICE)) %>% 
                 dplyr::select(HAUL, HAUL_ID, CATCH_SAMPLE_ID, SPECIES_CODE, SPECIES_NAME, SEX, TOSSED, RECORDING_DEVICE)
    
  # Expand specimen biometric table, join to raw_specimen table to get catch sample ID, join with samples file to get 
  # number tossed
      specimen_sum <- raw_specimen_bio %>%
                      dplyr::select(HAUL, HAUL_ID, SPECIMEN_ID, BIOMETRIC_NAME, VALUE, RECORDING_DEVICE) %>%
                      pivot_wider(., id_cols = c(HAUL, HAUL_ID, SPECIMEN_ID, RECORDING_DEVICE), 
                                  names_from = "BIOMETRIC_NAME", values_from = "VALUE") %>%
                      dplyr::rename(SHELL_CONDITION = CRAB_SHELL_CONDITION, EGG_COLOR = CRAB_EGG_COLOR,
                                    EGG_CONDITION = CRAB_EGG_CONDITION, CLUTCH_SIZE = CRAB_EGG_CLUTCH_SIZE,
                                    LENGTH = CARAPACE_LENGTH) %>%
                      right_join(., raw_specimen) %>% # need to get catch_sample_id
                      right_join(samples, ., by = c("HAUL", "HAUL_ID", "SEX", "CATCH_SAMPLE_ID", "SPECIES_CODE", "RECORDING_DEVICE"), 
                                 relationship = "many-to-many") #%>%
                      # mutate(LENGTH = ifelse((HAUL == 8 & HAUL_ID == 22 & SPECIMEN_ID == 190) | # modify 2 erroneous small measurements (length x 10)
                      #                        (HAUL == 9 & HAUL_ID == 23 & SPECIMEN_ID == 214), 
                      #                        LENGTH*10, LENGTH)) %>%
                      # distinct() %>%
                      # filter(is.na(HAUL) == "FALSE") # filter out haul 118 AL NAs for now
      
      # test <- specimen_sum %>% 
      #   group_by(HAUL, HAUL_ID, CATCH_SAMPLE_ID, SPECIES_CODE, SPECIES_NAME, SEX, TOSSED, RECORDING_DEVICE, SPECIMEN_ID, SHELL_CONDITION, EGG_COLOR, EGG_CONDITION, CLUTCH_SIZE, LENGTH) %>% 
      #   filter(n()>1)
                      
      # test <- raw_specimen_bio %>%
      #   dplyr::group_by(HAUL, HAUL_ID, SPECIMEN_ID, RECORDING_DEVICE, BIOMETRIC_NAME) %>%
      #   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      #   dplyr::filter(n > 1L) 
      
  # Calculate sampling factor from specimen summary table, join back with specimen_sum file to 
  # get specimen information, join with catch file to get vessel and pot #s, join with potlifts
  # file to get lat/lon, set/haul date and time for each pot (with positive catch)
      specimen_table <- specimen_sum %>%
                        dplyr::group_by(HAUL, HAUL_ID, CATCH_SAMPLE_ID, SEX, RECORDING_DEVICE) %>%
                        dplyr::reframe(KEPT = n(),
                                  TOSSED = TOSSED,
                                  SAMPLING_FACTOR = (KEPT + TOSSED)/KEPT) %>%
                        distinct() %>%
                        dplyr::right_join(specimen_sum, by = c("HAUL", "HAUL_ID", "CATCH_SAMPLE_ID", "SEX", "TOSSED",
                                                               "RECORDING_DEVICE"),
                                          multiple = "all") %>%
                        dplyr::select(-c(WEIGHT)) %>% # remove WEIGHT, gets re-added with catch joining
                        dplyr::right_join(catch %>% select(-c(RECORDING_DEVICE, ID)), ., 
                                          by = c("HAUL", "HAUL_ID", "SPECIES_CODE"), #RECORDING_DEVICE (*maybe need this once we have both vessels?)
                                          multiple = "all") %>%
                        dplyr::left_join(specimen) %>%
                        distinct() %>%
                        dplyr::rename(SPN = HAUL) %>%
                        dplyr::right_join(potlifts, ., by = c("VESSEL", "SPN"), relationship = "many-to-many") %>%
                        dplyr::mutate(VESSEL = ifelse(VESSEL == 162, "Arctic Lady", "Seabrooke")) %>%
                        dplyr::filter(c(is.na(LAT_DD) & is.na(LON_DD) & is.na(SPN)) == FALSE) %>% # bad/gear testing hauls will have NA
                        dplyr::select(CRUISE, VESSEL, SPN, POT_ID, BUOY, LAT_DD, LON_DD, DATE_HAUL, TIME_HAUL, SOAK_TIME, DEPTH_F,
                                      SPECIES_CODE, SEX, LENGTH, WIDTH, SAMPLING_FACTOR, SHELL_CONDITION, EGG_COLOR, EGG_CONDITION, 
                                      CLUTCH_SIZE, WEIGHT, DISEASE_CODE, DISEASE_DORSAL, DISEASE_VENTRAL, DISEASE_LEGS,  
                                      CHELA_HEIGHT, MERUS_LENGTH, COMMENTS, NOTES.x) %>%
                        dplyr::filter(is.na(POT_ID) == "FALSE") # filter out haul 177 SB NAs for now
      
  # Process specimen table for Oracle, save
      specimen_table %>%
        dplyr::select(!c(LAT_DD, LON_DD, DATE_HAUL, TIME_HAUL, SOAK_TIME, DEPTH_F, NOTES.x)) %>%
        dplyr::rename(HAUL = POT_ID, STATION = BUOY) %>% # MAY CHANGE BUOY TO ACTUAL STATION
        write.csv("./DataForOracle/Processed_Pot_Specimen_Data.csv", row.names = FALSE)
      
  # Process specimen table with all haul data, save
      specimen_table %>% 
        rename(NOTES = NOTES.x) %>%
        write.csv("./Outputs/CPS2_2024_Processed_Pot_Specimen_Data.csv", row.names = FALSE)
      
  # Update catch summary table with new crab #s from sampling factor
      catch_summary <- specimen_table %>%
                       dplyr::group_by(CRUISE, VESSEL, SPN, POT_ID, SPECIES_CODE) %>%
                       dplyr::reframe(NUMBER_CRAB = sum(SAMPLING_FACTOR)) %>%
                       dplyr::right_join(catch %>% dplyr::rename(SPN = HAUL, N_ENTRIES = NUMBER_CRAB) %>%
                                           dplyr::mutate(VESSEL = ifelse(VESSEL == 162, "Arctic Lady", "Seabrooke"))) %>%
                       dplyr::select(CRUISE, VESSEL, SPN, POT_ID, SPECIES_CODE, NUMBER_CRAB, N_ENTRIES) %>%
                       na.omit() # bad/gear testing potlifts will have NA for SPN and # crab
      
      ## MAKE CODE TO spit out lines where N_CRAB =/= N_ENTRIES??
      
  # Process catch_summary table for Oracle, save
      catch_summary %>%
        dplyr::select(!N_ENTRIES) %>%
        dplyr::rename(HAUL = SPN) %>%
        write.csv("./DataForOracle/Processed_Pot_Catch_Summary.csv", row.names = FALSE)

      
# CALCULATE BBRKC CPUE -------------------------------------------------------------------------------------------------------     
  
  # Make non-overlapping maturity/sex and legal/sublegal categories, bind together
      maturity <- specimen_table %>%
                  dplyr::mutate(MAT_SEX = dplyr::case_when((SPECIES_CODE == 69322 & SEX == 1 & LENGTH >= 120) ~ "Mature male",
                                                           (SPECIES_CODE == 69322 & SEX == 1 & LENGTH < 120) ~ "Immature male",
                                                           (SPECIES_CODE == 69322 & SEX == 2 & CLUTCH_SIZE >= 1) ~ "Mature female",
                                                           (SPECIES_CODE == 69322 & SEX == 2 & CLUTCH_SIZE == 0) ~ "Immature female"))
      
      legal <- specimen_table %>%
               dplyr::mutate(MAT_SEX = dplyr::case_when((SPECIES_CODE == 69322 & SEX == 1 & LENGTH >= 135) ~ "Legal male",
                                                        (SPECIES_CODE == 69322 & SEX == 1 & LENGTH < 135) ~ "Sublegal male")) %>%
               dplyr::filter(is.na(MAT_SEX) == "FALSE")
      
      
      mat_spec <- rbind(maturity, legal) #bind
      
      
  # Calculate COUNT and CATCH_PER_HOUR per pot, CHANGE VESSEL TO ACTUAL NAME
      positive_pot_cpue <- mat_spec %>%
                           dplyr::mutate(CATCH_PER_HOUR = SAMPLING_FACTOR/SOAK_TIME) %>% 
                           dplyr::group_by(VESSEL, SPN, POT_ID, BUOY, LAT_DD, LON_DD, MAT_SEX) %>%
                           dplyr::reframe(COUNT = sum(SAMPLING_FACTOR),
                                          CATCH_PER_HOUR = sum(CATCH_PER_HOUR))
      
    # Change vessel #s to names in potlifts file
      potlifts <- potlifts %>%
                  dplyr::mutate(VESSEL = ifelse(VESSEL == 162, "Arctic Lady", "Seabrooke"))
  
  # Expand potlifts file to all mat-sex categories and potlifts, join to positive catch file to get zeros 
      mat_sex_combos <- c("Mature male", "Immature male", "Mature female", "Immature female", "Legal male", "Sublegal male")
      
      pot_cpue <- positive_pot_cpue %>%
                  dplyr::right_join(., expand_grid(MAT_SEX = mat_sex_combos,
                                                potlifts)) %>%
                  replace_na(list(COUNT = 0, CPUE = 0)) %>%
                  dplyr::select(VESSEL, SPN, POT_ID, BUOY, LAT_DD, LON_DD, DATE_SET, TIME_SET, DATE_HAUL, TIME_HAUL, SOAK_TIME,
                                MAT_SEX, COUNT, CATCH_PER_HOUR)
      
  # Save csv
      write.csv(pot_cpue, "./Outputs/CPS2_2024_potcatch.csv", row.names = FALSE)
      

# BYCATCH -----------------------------------------------------------------------
  # Process data
      bycatch <- rbind(read.csv(paste0(path, "AL_BYCATCH.csv")),# %>%
                              # select(!c(#RKC.Male.in.NonSurveyPots, RKC.Female.in.NonSurvey.Pots,
                              #           Pollock, Starry.Flounder)), 
                      read.csv(paste0(path, "SB_Bycatch.csv"))) %>%
                 # remove incomplete haul records
                 filter(DATE_HAUL != "", is.na(VESSEL) == "FALSE") %>%
                 mutate(SPN = as.numeric(as.character(SPN)), VESSEL = ifelse(VESSEL == 162, "Arctic Lady", "Seabrooke")) %>%
                 # filter(is.na(SPN) == FALSE, !(VESSEL == "Seabrooke" & SPN %in% 300:311)) %>%
                 right_join(potlifts %>% select(c(VESSEL, SPN, LON_DD, LAT_DD)), .) %>%
                 replace(., is.na(.), 0) %>%
                 # sum across male and female crabs to get species-level counts
                 dplyr::mutate(Tanner = MaleTanner + FemaleTanner,
                               Snow = MaleSnow + FemaleSnow,
                               Hybrid = MaleHybrid + FemaleHybrid) 
      
    # Save .csv
      write.csv(bycatch, "./Outputs/CPS2_2024_POT_bycatch.csv", row.names = FALSE)
  
    
