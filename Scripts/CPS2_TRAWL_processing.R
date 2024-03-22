# PURPOSE -------------------------------------------------------------------------------------------------------------------
# 1) To automate processing trawl data specimen tables and catch summaries from Collaborative Pot 
#    Sampling II (CPS2) 2024 for BBRKC
# ...2) To run error checks on processed specimen and catch summaries
# ...3) To calculate and map cpue by haul and maturity/sex category for BBRKC

# Authors: Shannon Hennessey and Emily Ryznar, NOAA-AFSC


# INSTALL PACKAGES ----------------------------------------------------------------------------------------------------------
#install.packages("tidyverse")


# LOAD PACKAGES -------------------------------------------------------------------------------------------------------------
library(tidyverse)


# LOAD CRAB DATA -----------------------------------------------------------------------------------------------------------------

  # Set trawl data filepath
    # path <- "./Data/Trawl Data/"
    path <- "Y:/KOD_Survey/CPS2/Data/Trawl Data/"  

  # Load summary catch and specimen tables
    # **delete catch and specimen files 0015, 0019, 0021, 0022, 0028, 0030, 0031 because no data
    catch <- list.files(paste0(path, "Catch - FTP/")) %>% 
             purrr::map_df(~read.csv(paste0(path, "Catch - FTP/", .x)))
  
    specimen <- list.files(paste0(path, "Specimen - FTP/")) %>%
                purrr::map_df(~read.csv(paste0(path, "Specimen - FTP/", .x))) 
  
  # Load raw data for processing below   
    raw_sample <- list.files(paste0(path, "Raw Data - FTP/"), pattern = "_SAMPLE_0") %>% # RECORDS of SAMPLE INFO
                  purrr::map_df(~read.csv(paste0(path, "Raw Data - FTP/", .x))) #E.G. SEX, SPECIES
  
    raw_sample_values <- list.files(paste0(path, "Raw Data - FTP/"), pattern = "_SAMPLE_VALUES") %>% #RECORDS OF # TOSSED
                         purrr::map_df(~read.csv(paste0(path, "Raw Data - FTP/", .x))) #%>%
                         # mutate(TOSSED = ifelse(is.na(COUNT) == FALSE, COUNT,0)) %>%
                         # group_by(HAUL_ID, CATCH_SAMPLE_ID) %>%
                         # dplyr::reframe(TOSSED = sum(TOSSED)) 
  
    raw_specimen <- list.files(paste0(path, "Raw Data - FTP/"), pattern = "_SPECIMEN_0") %>% 
                    purrr::map_df(~read.csv(paste0(paste0(path, "Raw Data - FTP/", .x)))) #%>%
                   # dplyr::select(HAUL_ID, SPECIMEN_ID, CATCH_SAMPLE_ID, SPECIES_CODE)
  
    raw_specimen_bio <- list.files(paste0(path, "Raw Data - FTP/"), pattern = "_SPECIMEN_BIOMETRICS") %>% 
                        purrr::map_df(~read.csv(paste0(paste0(path, "Raw Data - FTP/", .x)))) %>%
                        right_join(., catch %>% select(HAUL, HAUL_ID, RECORDING_DEVICE), 
                                   relationship = "many-to-many") %>%
                        distinct()
  
  # Read in haul data
    hauls <- read.csv(paste0(path, "VA_HAULS.csv")) %>% # Vesteraalen
             dplyr::filter(SAMPLED == 1)
    
# PROCESS DATA ----------------------------------------------------------------------------------------------------------------
    
  # # Calculate tow time and lat/lon in degrees decimal for all hauls, omit bad or gear testing hauls based on gear code
  #   hauls <- hauls %>%
  #            dplyr::mutate(#DATETIME_SET = as.POSIXct(paste(DATE_SET, TIME_SET), format = "%m/%d/%Y %H:%M"),
  #                          #DATETIME_HAUL = as.POSIXct(paste(DATE_HAUL, TIME_HAUL), format = "%m/%d/%Y %H:%M"),
  #                          #SOAK_TIME = as.numeric(difftime(DATETIME_HAUL, DATETIME_SET, units = "hours"))) %>%
  #            dplyr::select(!c(DATETIME_SET, DATETIME_HAUL)) 
    
  # Join raw_sample_values and raw_sample to get # tossed per haul, sex, and catch sample id
    # samples <- right_join(raw_sample, raw_sample_values) %>%
    #            right_join(., catch %>% select(HAUL, HAUL_ID, RECORDING_DEVICE)) %>%
    #            dplyr::select(HAUL, HAUL_ID, CATCH_SAMPLE_ID, SPECIES_CODE, SPECIES_NAME, SEX, TOSSED, RECORDING_DEVICE)
    
    samples <- left_join(raw_sample_values %>% select(HAUL_ID, CATCH_SAMPLE_VALUE_ID, CATCH_SAMPLE_ID, WEIGHT, KEEP, 
                                                      RECORDING_DEVICE, RECORDER), 
                         raw_sample %>% select(HAUL_ID, CATCH_SAMPLE_ID, MIX, SAMPLED_ALL, PARENT_SAMPLE_ID, SPECIES_CODE, SPECIES_NAME,
                                               SEX, RECORDING_DEVICE, RECORDER), 
                         by = c("HAUL_ID", "CATCH_SAMPLE_ID", "RECORDING_DEVICE", "RECORDER")) %>%
               left_join(., distinct(catch %>% select(HAUL, HAUL_ID, RECORDING_DEVICE))) # %>%
               # # filter out weight corrections
               # filter(if_all(c(MIX, SAMPLED_ALL, SPECIES_CODE, SPECIES_NAME, SEX), complete.cases)) %>%
               # distinct() %>%
               #left_join(., raw_specimen, relationship = "many-to-many") %>% # get SPECIMEN_ID
                 # if 2 baskets of same spp/sex in a CATCH_SAMPLE_ID, sum weights and replace for all obs....
    samples2 <- samples %>%  
               dplyr::group_by(HAUL, HAUL_ID, CATCH_SAMPLE_ID, SEX, SPECIES_CODE, RECORDING_DEVICE) %>%
               dplyr::summarise(WEIGHT = sum(WEIGHT)) %>%
               # distinct() %>%
               dplyr::left_join(samples %>% select(-c(WEIGHT, CATCH_SAMPLE_VALUE_ID))) %>%
               distinct() %>%
               dplyr::select(HAUL, HAUL_ID, CATCH_SAMPLE_ID, MIX, SAMPLED_ALL, PARENT_SAMPLE_ID,
                             SPECIES_CODE, SPECIES_NAME, SEX, WEIGHT, KEEP, RECORDING_DEVICE) #
    
  # Expand specimen biometric table, join to raw_specimen table to get catch sample ID, join with samples file to get 
  # number tossed
    specimen_sum <- raw_specimen_bio %>%
                    dplyr::select(HAUL, HAUL_ID, SPECIMEN_ID, BIOMETRIC_NAME, VALUE, RECORDING_DEVICE) %>%
                    pivot_wider(., id_cols = c(HAUL, HAUL_ID, SPECIMEN_ID, RECORDING_DEVICE), 
                                names_from = "BIOMETRIC_NAME", values_from = "VALUE") %>%
                    dplyr::rename(SHELL_CONDITION = CRAB_SHELL_CONDITION, EGG_COLOR = CRAB_EGG_COLOR,
                                  EGG_CONDITION = CRAB_EGG_CONDITION, CLUTCH_SIZE = CRAB_EGG_CLUTCH_SIZE,
                                  LENGTH = CARAPACE_LENGTH, WIDTH = CARAPACE_WIDTH) %>%
                    left_join(., raw_specimen) %>%
                    dplyr::select(-c(WEIGHT)) %>% # remove weight
                    right_join(samples2, ., by = c("HAUL", "HAUL_ID", "SEX", "RECORDING_DEVICE", "SPECIES_CODE", "CATCH_SAMPLE_ID"), 
                               relationship = "many-to-many") %>%
                    distinct()

    
  # Calculate sampling factor from specimen summary table, join back with specimen_sum file to 
  # get specimen information, join with catch file to get vessel and station #s, join with hauls
  # file to get lat/lon, set/haul date and time for each haul (with positive catch)
    specimen_table <- specimen_sum %>%
                      dplyr::group_by(HAUL, HAUL_ID, CATCH_SAMPLE_ID, SEX, RECORDING_DEVICE) %>%
                      # # dplyr::reframe(KEPT = n(),
                      # #                TOSSED = TOSSED,
                      # #                SAMPLING_FACTOR = (KEPT + TOSSED)/KEPT) %>%
                      # distinct() %>%
                      # dplyr::right_join(specimen_sum, multiple = "all") %>%
                      dplyr::right_join(catch %>% select(-c(WEIGHT)), ., by = c("HAUL", "HAUL_ID", "SPECIES_CODE", "RECORDING_DEVICE")) %>%
                      dplyr::left_join(specimen) %>%
                      # distinct() %>%
                      # dplyr::rename(SPN = HAUL) %>%
                      dplyr::right_join(hauls, ., by = c("STATION"), relationship = "many-to-many") %>%
                      dplyr::mutate(VESSEL = ifelse(VESSEL == 176, "Vesteraalen", ""),
                                    SAMPLING_FACTOR = 1) %>% ## NEED TO DOUBLE CHECK SF (make sure no >1 ever)..this doesn't join right
                      #dplyr::filter(c(is.na(LAT_DD) & is.na(LON_DD) & is.na(SPN)) == FALSE) %>% # bad/gear testing hauls will have NA
                      dplyr::select(CRUISE, VESSEL, HAUL, STATION, LAT_DD, LON_DD, #DATE_HAUL, TIME_HAUL, SOAK_TIME, DEPTH_F,
                                    SPECIES_CODE, SPECIES_NAME, SEX, LENGTH, WIDTH, SAMPLING_FACTOR, SHELL_CONDITION, EGG_COLOR, EGG_CONDITION, 
                                    CLUTCH_SIZE, WEIGHT, DISEASE_CODE, DISEASE_DORSAL, DISEASE_VENTRAL, DISEASE_LEGS,  
                                    CHELA_HEIGHT, MERUS_LENGTH, COMMENTS, NOTES)
    
  # Process specimen table for Oracle, save
    specimen_table %>%
      dplyr::select(!c(LAT_DD, LON_DD, NOTES)) %>% #DATE_HAUL, TIME_HAUL, SOAK_TIME, DEPTH_F, 
      # dplyr::rename(HAUL = POT_ID, STATION = BUOY) %>% # MAY CHANGE BUOY TO ACTUAL STATION
      write.csv("./DataForOracle/Processed_Trawl_Specimen_Data.csv", row.names = FALSE)
    
  # Process specimen table with all haul data, save
    specimen_table %>% 
      # rename(NOTES = NOTES.x) %>%
      write.csv("./Outputs/CPS2_2024_Processed_Trawl_Specimen_Data.csv", row.names = FALSE)
    
  # Update catch summary table with new crab #s from sampling factor
    catch_summary <- specimen_table %>%
                     dplyr::group_by(CRUISE, VESSEL, HAUL, STATION, SPECIES_CODE) %>%
                     dplyr::reframe(NUMBER_CRAB = sum(SAMPLING_FACTOR)) %>%
                     dplyr::right_join(catch %>% dplyr::rename(N_ENTRIES = NUMBER_CRAB) %>%
                                                 dplyr::mutate(VESSEL = ifelse(VESSEL == 176, "Vesteraalen", ""))) %>%
                     dplyr::select(CRUISE, VESSEL, HAUL, STATION, SPECIES_CODE, NUMBER_CRAB, N_ENTRIES) %>%
                     na.omit() # bad/gear testing potlifts will have NA for SPN and # crab
                   
  # Process catch_summary table for Oracle, save
    catch_summary %>%
      dplyr::select(!N_ENTRIES) %>%
      # dplyr::rename(HAUL = SPN) %>%
      write.csv("./DataForOracle/Processed_Trawl_Catch_Summary.csv", row.names = FALSE)
    
    
# CALCULATE BBRKC CPUE -------------------------------------------------------------------------------------------------------     
    
    # Make non-overlapping maturity/sex and legal/sublegal categories, bind together
    maturity <- specimen_table %>%
                dplyr::filter(SPECIES_CODE == 69322) %>% # filter just RKC
                dplyr::mutate(MAT_SEX = dplyr::case_when((SPECIES_CODE == 69322 & SEX == 1 & LENGTH >= 120) ~ "Mature male",
                                                         (SPECIES_CODE == 69322 & SEX == 1 & LENGTH < 120) ~ "Immature male",
                                                         (SPECIES_CODE == 69322 & SEX == 2 & CLUTCH_SIZE >= 1) ~ "Mature female",
                                                         (SPECIES_CODE == 69322 & SEX == 2 & CLUTCH_SIZE == 0) ~ "Immature female"))
    
    legal <- specimen_table %>%
             dplyr::filter(SPECIES_CODE == 69322) %>% # filter just RKC
             dplyr::mutate(MAT_SEX = dplyr::case_when((SPECIES_CODE == 69322 & SEX == 1 & LENGTH >= 135) ~ "Legal male",
                                                      (SPECIES_CODE == 69322 & SEX == 1 & LENGTH < 135) ~ "Sublegal male")) %>%
             dplyr::filter(is.na(MAT_SEX) == "FALSE")
           
    
    mat_spec <- rbind(maturity, legal) #bind
    
  
    
  # Summarize counts by sex/maturity category per station
    mat_spec_sum <- mat_spec %>%
                    dplyr::group_by(VESSEL, HAUL, STATION, LAT_DD, LON_DD, MAT_SEX) %>%
                    dplyr::reframe(COUNT = sum(SAMPLING_FACTOR))
    
    positive_haul_cpue <- mat_spec_sum
  
  # # Calculate COUNT and CATCH_PER_HOUR per pot, CHANGE VESSEL TO ACTUAL NAME
  #   positive_haul_cpue <- mat_spec %>%
  #                        dplyr::mutate(CATCH_PER_HOUR = SAMPLING_FACTOR/SOAK_TIME) %>% 
  #                        dplyr::group_by(VESSEL, SPN, POT_ID, BUOY, LAT_DD, LON_DD, MAT_SEX) %>%
  #                        dplyr::reframe(COUNT = sum(SAMPLING_FACTOR),
  #                                       CATCH_PER_HOUR = sum(CATCH_PER_HOUR))
  #   
  # # Change vessel #s to names in haul file
  #   hauls <- hauls %>% dplyr::mutate(VESSEL = ifelse(VESSEL == 162, "Arctic Lady", "Seabrooke"))
    
  # Expand potlifts file to all mat-sex categories and potlifts, join to positive catch file to get zeros 
    mat_sex_combos <- c("Mature male", "Immature male", "Mature female", "Immature female", "Legal male", "Sublegal male")
    
    trawl_cpue <- positive_haul_cpue %>%
                  dplyr::right_join(expand_grid(MAT_SEX = mat_sex_combos,
                                                hauls)) %>%
                  replace_na(list(COUNT = 0, VESSEL = "Vesteraalen")) %>%
                  dplyr::select(VESSEL, HAUL, STATION, LAT_DD, LON_DD,
                                MAT_SEX, COUNT) #, CATCH_PER_HOUR) 

    
  # Save csv
    write.csv(trawl_cpue, "./Outputs/CPS2_2024_trawlcatch.csv", row.names = FALSE)
    
    
# # BYCATCH -----------------------------------------------------------------------
#   # # Process data
#   #   bycatch <- rbind(read.csv(paste0(path, "VA_BYCATCH.csv")) %>%
#   #                      select(!c(RKC.Male.in.NonSurveyPots, RKC.Female.in.NonSurvey.Pots,
#   #                                Pollock, Starry.Flounder)),
#   #                    read.csv(paste0(path, "SB_BYCATCH.csv"))) %>%
#   #     mutate(SPN = as.numeric(as.character(SPN)), VESSEL = ifelse(VESSEL == 162, "Arctic Lady", "Seabrooke")) %>%
#   #     filter(is.na(SPN) == FALSE, !(VESSEL == "Seabrooke" & SPN %in% 300:311)) %>%
#   #     right_join(potlifts %>% select(c(VESSEL, SPN, LON_DD, LAT_DD)), .) %>%
#   #     replace(., is.na(.), 0) %>%
#   #     # sum across male and female crabs to get species-level counts
#   #     dplyr::mutate(Tanner = MaleTanner + FemaleTanner,
#   #                   Snow = MaleSnow + FemaleSnow,
#   #                   Hybrid = MaleHybrid + FemaleHybrid) 
#   #   
#   # # Save csv
#   #   write.csv(bycatch, "./Outputs/CPS2_2024_TRAWL_bycatch.csv", row.names = FALSE)
# 
# # LOAD FISH DATA -----------------------------------------------------------------------------------------------------------------
#     
#     # Set pot data filepath
#       path <- "./Data/Trawl Data/"
#     
#     # Load summary catch and specimen tables
#       catch <- list.files(paste0(path, "Groundfish Catch - FTP/")) %>%
#                purrr::map_df(~read.csv(paste0(paste0(path, "Groundfish Catch - FTP/", .x))))
#       
#       specimen <- list.files(paste0(path, "Specimen - FTP/")) %>%
#                   purrr::map_df(~read.csv(paste0(paste0(path, "Specimen - FTP/", .x))) %>% 
#                                   mutate(STATION = paste0("X", STATION))) 
#     
#     # Load raw data for processing below   
#       # raw_sample <- list.files(paste0(path, "Groundfish Raw - FTP/"), pattern = "_SAMPLE_0") %>% # RECORDS of SAMPLE INFO
#                     purrr::map_df(~read.csv(paste0(paste0(path, "Groundfish Raw - FTP/", .x)))) #E.G. SEX, SPECIES
#       
#       # raw_sample_values <- list.files(paste0(path, "Groundfish Raw - FTP/"), pattern = "_CATCH_VALUE") %>% #RECORDS OF # TOSSED
#                            purrr::map_df(~read.csv(paste0(paste0(path, "Groundfish Raw - FTP/", .x)))) %>%
#                            # mutate(TOSSED = ifelse(is.na(COUNT) == FALSE, COUNT,0)) %>%
#                            # group_by(HAUL_ID, CATCH_SAMPLE_ID) %>%
#                            # dplyr::reframe(TOSSED = sum(TOSSED)) 
#       
#       raw_specimen <- list.files(paste0(path, "Groundfish Raw - FTP/"), pattern = "_SPECIMEN_0") %>% 
#                       purrr::map_df(~read.csv(paste0(paste0(path, "Groundfish Raw - FTP/", .x)))) %>%
#                       dplyr::select(HAUL_ID, SPECIMEN_ID, CATCH_SAMPLE_ID, SPECIES_CODE)
#       
#       # raw_specimen_bio <- list.files(paste0(path, "Raw Data - FTP/"), pattern = "_SPECIMEN_BIOMETRICS") %>% 
#       #                     purrr::map_df(~ read.csv(paste0(paste0(path, "Raw Data - FTP/", .x)))) %>%
#       #                     right_join(., catch %>% select(HAUL, HAUL_ID, RECORDING_DEVICE))
#                       
#     # Read in haul data
#     hauls <- read.csv(paste0(path, "VA_HAULS.csv")) # Vesteraalen
#     
#     
# # PROCESS FISH DATA ----------------------------------------------------------------------------------------------------------------
#     
#     # Calculate tow time and lat/lon in degrees decimal for all hauls, omit bad or gear testing hauls based on gear code
#     hauls <- hauls %>%
#             dplyr::mutate(DATETIME_SET = as.POSIXct(paste(DATE_SET, TIME_SET), format = "%m/%d/%Y %H:%M"),
#                           DATETIME_HAUL = as.POSIXct(paste(DATE_HAUL, TIME_HAUL), format = "%m/%d/%Y %H:%M"),
#                           SOAK_TIME = as.numeric(difftime(DATETIME_HAUL, DATETIME_SET, units = "hours")),
#                           LAT_DD = LAT_DEG + LAT_MIN/60,
#                           LON_DD = (LON_DEG + LON_MIN/60)*-1) %>%
#             dplyr::select(!c(DATETIME_SET, DATETIME_HAUL)) 
#     
#     # Join raw_sample_values and raw_sample to get # tossed per haul, sex, and catch sample id
#     samples <- right_join(raw_sample, raw_sample_values) %>%
#       right_join(., catch %>% select(HAUL, HAUL_ID, RECORDING_DEVICE)) %>%
#       dplyr::select(HAUL, HAUL_ID, CATCH_SAMPLE_ID, SPECIES_CODE, SPECIES_NAME, SEX, TOSSED, RECORDING_DEVICE)
#     
#     # Expand specimen biometric table, join to raw_specimen table to get catch sample ID, join with samples file to get 
#     # number tossed
#     specimen_sum <- raw_specimen_bio %>%
#       dplyr::select(HAUL, HAUL_ID, SPECIMEN_ID, BIOMETRIC_NAME, VALUE, RECORDING_DEVICE) %>%
#       pivot_wider(., id_cols = c(HAUL, HAUL_ID, SPECIMEN_ID, RECORDING_DEVICE), 
#                   names_from = "BIOMETRIC_NAME", values_from = "VALUE") %>%
#       dplyr::rename(SHELL_CONDITION = CRAB_SHELL_CONDITION, EGG_COLOR = CRAB_EGG_COLOR,
#                     EGG_CONDITION = CRAB_EGG_CONDITION, CLUTCH_SIZE = CRAB_EGG_CLUTCH_SIZE,
#                     LENGTH = CARAPACE_LENGTH) %>%
#       #right_join(., raw_specimen) %>%
#       right_join(samples, ., by = c("HAUL", "HAUL_ID", "SEX", "RECORDING_DEVICE"), 
#                  relationship = "many-to-many")
#     
#     # Calculate sampling factor from specimen summary table, join back with specimen_sum file to 
#     # get specimen information, join with catch file to get vessel and station #s, join with hauls
#     # file to get lat/lon, set/haul date and time for each haul (with positive catch)
#     specimen_table <- specimen_sum %>%
#       dplyr::group_by(HAUL, HAUL_ID, CATCH_SAMPLE_ID, SEX, RECORDING_DEVICE) %>%
#       dplyr::reframe(KEPT = n(),
#                      TOSSED = TOSSED,
#                      SAMPLING_FACTOR = (KEPT + TOSSED)/KEPT) %>%
#       distinct() %>%
#       dplyr::right_join(specimen_sum, by = c("HAUL", "HAUL_ID", "CATCH_SAMPLE_ID", "SEX", "TOSSED",
#                                              "RECORDING_DEVICE"),
#                         multiple = "all") %>%
#       dplyr::right_join(catch,., by = c("HAUL", "HAUL_ID", "SPECIES_CODE", "RECORDING_DEVICE"),
#                         multiple = "all") %>%
#       dplyr::left_join(specimen) %>%
#       distinct() %>%
#       dplyr::rename(SPN = HAUL) %>%
#       dplyr::right_join(potlifts, ., by = c("VESSEL", "SPN"), relationship = "many-to-many") %>%
#       dplyr::mutate(VESSEL = ifelse(VESSEL == 162, "Arctic Lady", "Seabrooke")) %>%
#       dplyr::filter(c(is.na(LAT_DD) & is.na(LON_DD) & is.na(SPN)) == FALSE) %>% # bad/gear testing hauls will have NA
#       dplyr::select(CRUISE, VESSEL, SPN, POT_ID, BUOY, LAT_DD, LON_DD, DATE_HAUL, TIME_HAUL, SOAK_TIME, DEPTH_F,
#                     SPECIES_CODE, SEX, LENGTH, WIDTH, SAMPLING_FACTOR, SHELL_CONDITION, EGG_COLOR, EGG_CONDITION, 
#                     CLUTCH_SIZE, WEIGHT, DISEASE_CODE, DISEASE_DORSAL, DISEASE_VENTRAL, DISEASE_LEGS,  
#                     CHELA_HEIGHT, MERUS_LENGTH, COMMENTS, NOTES.x)
#     
#     # Process specimen table for Oracle, save
#     specimen_table %>%
#       dplyr::select(!c(LAT_DD, LON_DD, DATE_HAUL, TIME_HAUL, SOAK_TIME, DEPTH_F, NOTES.x)) %>%
#       dplyr::rename(HAUL = POT_ID, STATION = BUOY) %>% # MAY CHANGE BUOY TO ACTUAL STATION
#       write.csv("./DataForOracle/Processed_Trawl_Specimen_Data.csv", row.names = FALSE)
#     
#     # Process specimen table with all haul data, save
#     specimen_table %>% 
#       rename(NOTES = NOTES.x) %>%
#       write.csv("./Outputs/CPS2_2024_Processed_Trawl_Specimen_Data.csv", row.names = FALSE)
#     
#     # Update catch summary table with new crab #s from sampling factor
#     catch_summary <- specimen_table %>%
#       dplyr::group_by(CRUISE, VESSEL, SPN, POT_ID, SPECIES_CODE) %>%
#       dplyr::reframe(NUMBER_CRAB = sum(SAMPLING_FACTOR)) %>%
#       dplyr::right_join(catch %>% dplyr::rename(SPN = HAUL, N_ENTRIES = NUMBER_CRAB) %>%
#                           dplyr::mutate(VESSEL = ifelse(VESSEL == 162, "Arctic Lady", "Seabrooke"))) %>%
#       dplyr::select(CRUISE, VESSEL, SPN, POT_ID, SPECIES_CODE, NUMBER_CRAB, N_ENTRIES) %>%
#       na.omit() # bad/gear testing potlifts will have NA for SPN and # crab
#     
#     # Process catch_summary table for Oracle, save
#     catch_summary %>%
#       dplyr::select(!N_ENTRIES) %>%
#       dplyr::rename(HAUL = SPN) %>%
#       write.csv("./DataForOracle/Processed_Trawl_Catch_Summary.csv", row.names = FALSE)
#     
#     
# # CALCULATE CPUE -------------------------------------------------------------------------------------------------------     
#     
#     # Make non-overlapping maturity/sex and legal/sublegal categories, bind together
#     maturity <- specimen_table %>%
#                 dplyr::mutate(MAT_SEX = dplyr::case_when((SPECIES_CODE == 69322 & SEX == 1 & LENGTH >= 120) ~ "Mature male",
#                                                          (SPECIES_CODE == 69322 & SEX == 1 & LENGTH < 120) ~ "Immature male",
#                                                          (SPECIES_CODE == 69322 & SEX == 2 & CLUTCH_SIZE >= 1) ~ "Mature female",
#                                                          (SPECIES_CODE == 69322 & SEX == 2 & CLUTCH_SIZE == 0) ~ "Immature female"))
#     
#     legal <- specimen_table %>%
#             dplyr::mutate(MAT_SEX = dplyr::case_when((SPECIES_CODE == 69322 & SEX == 1 & LENGTH >= 135) ~ "Legal male",
#                                                      (SPECIES_CODE == 69322 & SEX == 1 & LENGTH < 135) ~ "Sublegal male")) %>%
#             dplyr::filter(is.na(MAT_SEX) == "FALSE")
#           
#           
#     mat_spec <- rbind(maturity, legal) #bind
#     
#     
#     # Calculate COUNT and CATCH_PER_HOUR per pot, CHANGE VESSEL TO ACTUAL NAME
#     positive_haul_cpue <- mat_spec %>%
#                           dplyr::mutate(CATCH_PER_HOUR = SAMPLING_FACTOR/SOAK_TIME) %>% 
#                           dplyr::group_by(VESSEL, SPN, POT_ID, BUOY, LAT_DD, LON_DD, MAT_SEX) %>%
#                           dplyr::reframe(COUNT = sum(SAMPLING_FACTOR),
#                                          CATCH_PER_HOUR = sum(CATCH_PER_HOUR))
#     
#     # Change vessel #s to names in haul file
#     hauls <- hauls %>% dplyr::mutate(VESSEL = ifelse(VESSEL == 162, "Arctic Lady", "Seabrooke"))
#     
#     # Expand potlifts file to all mat-sex categories and potlifts, join to positive catch file to get zeros 
#     mat_sex_combos <- c("Mature male", "Immature male", "Mature female", "Immature female", "Legal male", "Sublegal male")
#     
#     trawl_cpue <- positive_haul_cpue %>%
#       dplyr::right_join(expand_grid(MAT_SEX = mat_sex_combos,
#                                     hauls)) %>%
#       replace_na(list(COUNT = 0, CPUE = 0)) %>%
#       dplyr::select(VESSEL, SPN, POT_ID, BUOY, LAT_DD, LON_DD, DATE_SET, TIME_SET, DATE_HAUL, TIME_HAUL, SOAK_TIME,
#                     MAT_SEX, COUNT, CATCH_PER_HOUR)
#     
#     # Save csv
#     write.csv(trawl_cpue, "./Outputs/CPS2_2024_trawlcatch.csv", row.names = FALSE)      
#     
#     