source('./MMS_module/Functions/Storage_MMS.R')


## ----------------------- COMPUTE N/TAN DIRECTLY SPREAD TO THE FIELDS ----------------------- ##
## --------------------------------------------------------------------------------------------##

compute_N_spreading_slurry <- function(year) {
  # computes the N in slurry for direct and immediate spreading
  # unit : kg N yr-1
  
  compute_slurry_N_entering_storage(year = year, manure_spreading = TRUE)
}

compute_TAN_spreading_slurry <- function(year) {
  # computes the TAN in slurry for direct and immediate spreading
  # unit : kg TAN yr-1
  
  compute_slurry_TAN_entering_storage(year = year, manure_spreading = TRUE)
}

compute_TAN_spreading_solid <- function(year) {
  # computes the TAN in solid manure for direct and immediate spreading
  # unit : kg TAN yr-1
  
  compute_solid_TAN_entering_storage(year = year, manure_spreading = TRUE)
}

compute_N_spreading_solid <- function(year) {
  # computes the N in solid manure for direct and immediate spreading
  # unit : kg N yr-1
  
  compute_solid_N_entering_storage(year = year, manure_spreading = TRUE)
}



## ----------------------- COMPUTE N/TAN MANURE APPLICATION ----------------------- ##
## ---------------------------------------------------------------------------------##


compute_total_storage_emissions <- function(year, animal_class, manure_type) {
  # this function computes the sum of all storage gaseous sources (NH3, N2O, NOx, N2) for a given animal class and manure type
  # this function is used to compute the total N/TAN in manure application
  # unit: kg N yr-1
  
  gaseous_sources <- c('N2O', 'NOx', 'N2')
  calc_df <- get_MMS_subfolder_output_file(subfolder = 'Storage', 
                                           subfolderX2 = 'NH3', 
                                           year = year, 
                                           file_pattern = paste0(animal_class, '_', manure_type))
  for (i in gaseous_sources) {
    gaseous_source_df <- get_MMS_subfolder_output_file(subfolder = 'Storage', 
                                                       subfolderX2 = i, 
                                                       year = year, 
                                                       file_pattern = paste0(animal_class, '_', manure_type))
    # sum the gaseous sources
    calc_df[, seq(4, ncol(calc_df))] <- mapply("+", calc_df[, seq(4, ncol(calc_df))], gaseous_source_df[, seq(4, ncol(gaseous_source_df))])
  }
  return(calc_df)
}


compute_gross_field_app <- function(year, N_flow, manure_type) {
  # N flow is either TAN or NN
  ## Computes the total N/TAN in the gross manure application (i.e. total manure N before field application)
  ## TAN GROSS MANURE AVAILABLE APPLICATIOn = TAN_STORAGE + TAN_DIRECT_SPREADING - SUM_GASEOUS_STORAGE
  ## TAN GROSS MANURE APPLICATION = TAN GROSS MANURE AVAILABLE APPLICATIOn * MANURE_APPLICATION_RATES_MUNI (Statistics Portugal, 2009)
  ## Unit: kg NTAN yr-1
  
  ## Note: this is where N-NH3 emissions from manure application are to be calculated
  
  animal_class <- get_animal_classes()
  for (i in animal_class) {
    
    # get N/TAN manure in storage
    # condition: if slurry and TAN flow get the activity data for storage environmental N losses
    if (manure_type=='slurry' && N_flow=='TAN') {
      manure_storage <- compute_storage_slurry_TAN_emission(year, animal_class = i)
    } 
    else {
      manure_storage <- get_MMS_subfolder_output_file(subfolder = 'Storage', 
                                                          subfolderX2 = 'Entering_storage', 
                                                          year = year, 
                                                          file_pattern = paste0(N_flow ,'_', i, '_', manure_type))
    }

    manure_spreading <- get_MMS_subfolder_output_file(subfolder = 'Spreading', 
                                                          subfolderX2 = 'Direct_N_spreading', 
                                                          year = year, 
                                                          file_pattern = paste0(N_flow,'_', i, '_', manure_type))
    storage_gaseous_sources <- compute_total_storage_emissions(year = year, animal_class = i, manure_type = manure_type)
    
    calc_cols <- seq(4, ncol(manure_storage))

        # calculate gross manure TAN
    manure_storage[,calc_cols] <- manure_storage[,calc_cols] + manure_storage[,calc_cols] - storage_gaseous_sources[, calc_cols]
    # calculate proportion of this manure applied to the soil
    manure_app_rate <- get_manure_application_rates(year)
    manure_storage[,calc_cols] <- mapply('*', manure_storage[,calc_cols], manure_app_rate)
    
    write_annual_data(subfolder = 'Spreading', file = manure_storage, filename = paste0(N_flow, '_',i, '_', manure_type), year = year, 
                      subfolder_nameX2 = 'Gross_soil_manure_N')
  }
}


compute_NH3_manure_spreading <- function(year, manure_type) {
  # unit: kg N-NH3 yr-1
  
  animal_class <- get_animal_classes()
  for (i in animal_class) {
    
    TAN_gross_manure <- get_MMS_subfolder_output_file(subfolder = 'Spreading', 
                                                      subfolderX2 = 'Gross_soil_manure_N', 
                                                      year = year, 
                                                      file_pattern = paste0('TAN_', i, '_', manure_type))
    
    animal_subclass <- colnames(TAN_gross_manure)[-c(1,2,3)]  
    for (j in animal_subclass) {
      ef_spreading <- select_EFs_NH3_MMS(animal_class = i, 
                                         animal_subclass = j, 
                                         pathway = 'Spreading', 
                                         manure_type = manure_type)[, 4]
      print(ef_spreading)
      # calculate N-NH3 emission from spreading
      TAN_gross_manure[,j] <- TAN_gross_manure[,j] * ef_spreading
    }
    write_annual_data(subfolder = 'Spreading', file = TAN_gross_manure, filename = paste0(i, '_', manure_type), year = year, 
                      subfolder_nameX2 = 'NH3')
  }
}

compute_spreading_total <- function(year, subfolder) {
  # computes TOTAL N-NH3 emissions from animal housing
  # TOTAL = SLURRY + SOLID
  # unit: kg N-NH3 yr-1
  
  animal_class <- get_animal_classes()
  
  for (i in animal_class) {
    # data dump 
    solid <- get_MMS_subfolder_output_file(subfolder = 'Spreading', 
                                               subfolderX2 = subfolder, 
                                               year = year, 
                                               file_pattern = paste0(i, '_solid'))
    slurry <- get_MMS_subfolder_output_file(subfolder = 'Spreading', 
                                                subfolderX2 = subfolder, 
                                                year = year, 
                                                file_pattern = paste0(i, '_slurry'))
    
    animal_subclass_cols <- colnames(slurry)[-c(1,2,3)]  
    total <- slurry
    total[, animal_subclass_cols] <- mapply("+", slurry[, animal_subclass_cols], solid[, animal_subclass_cols])
    
    write_annual_data(subfolder = 'Spreading', file = total, filename = paste0(i, '_total'), year = year, subfolder_nameX2 = subfolder)
  }
}

compute_spreading_nh3 <- function(year) {
  compute_spreading_total(year, subfolder = 'NH3')
}

