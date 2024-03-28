
# RUN DATA PROCESSING WORKFLOW -----------------------------------------------------------------------------------------------------------------

  # Load mapping layers
    source("./Scripts/map_setup.R")

  # Load error checking function
    source("./Scripts/error_check.R")


  # POT processing
    # Process POT crab data
      source("./Scripts/CPS2_POT_processing.R")

    # Run error checks
      error_chk(method = "POT", 
                specimen_table = specimen_table, 
                catch_summary = catch_summary,
                potlifts = potlifts, 
                haul = NULL, 
                cpue = pot_cpue)
  
  # TRAWL processing
    # Process TRAWL crab data
     source("./Scripts/CPS2_TRAWL_processing.R")
      
    # Run error checks
      error_chk(method = "TRAWL", 
                specimen_table = specimen_table[which(specimen_table$SPECIES_CODE == 69322),], 
                catch_summary = catch_summary[which(catch_summary$SPECIES_CODE == 69322),], 
                potlifts = NULL, 
                haul = hauls, 
                cpue = trawl_cpue)
   
    # Process TRAWL fish data
      source("./Scripts/CPS2_TRAWL_FISH_processing.R")
      
      
      
    # Map CRAB COUNTS by pot/haul and maturity/sex category
      # standard function for mapping, with inputs for sex/mat or species??
      source("./Scripts/map_crab.R")
      
    # Map BYCATCH for trawl and pot
      source("./Scripts/map_fish.R")
      
      ## MAKE PLOTS with both pot and trawl data!
      

  #   -- can we make this into one script or one function to source?? 
  #   -- term for pot/trawl, crab/fish...

  

  
  

