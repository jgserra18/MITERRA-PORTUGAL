source('./MMS_module/Functions/Nex_computation.R')
source('./MMS_module/Functions/Prepare_MMS_EFs.R')
source('./MMS_module/Functions/Housing_MMS.R')


## ----------------------- COMPUTE N/TAN ENTERING STORAGE (SOLID SYSTEMS) ----------------------- ##
## -----------------------------------------------------------------------------------------------##

## NOTE: ALL THESE FUNCTIONS APPLY ALSO FOR THE FRACTION OF MANURE THAT IS DIRECTLY SPREAD TO THE FIELD
## IN SUCH CASES THE PARAM MANURE_SPREADING == TRUE
## WHEREAS IN STORAGE, MANURE_SPREADING == FALSE

compute_solid_TAN_entering_storage <- function(year, manure_spreading) {
  # computes the total TAN flows from manure entering storage
  # TAN_enter_storage =  [ TAN_housing_solid - NH3_housing_solid - (TAN_bedding * f_imm) ] * Xstore_solid
  # unit: kg TAN yr -1
  
  f_imm <- 0.0067 # unit: kg TAN kg-1 ;; immobilization in bedding ;; default EMEP param
  
  animal_class <- get_animal_classes()
  
  for (i in animal_class) {
    # select only TAN solid MMS in housing
    animal_class_mms <- paste0(i, '_', 'solid') 
    housing_TAN <- compute_TAN_flows_distribution(year = year, 
                                                  dist_system = 'Housing', 
                                                  animal_class = animal_class_mms)
    nh3_solid <- get_MMS_subfolder_output_file(subfolder = 'Housing', 
                                               subfolderX2 = 'NH3', 
                                               year = year, 
                                               file_pattern = paste0(i, '_solid'))
    calc_df <- housing_TAN
    animal_subclass <- colnames(housing_TAN)[-c(1,2,3)]  
    
    for (j in animal_subclass) {
      
      # manure spreading condition
      ifelse(manure_spreading==TRUE,
             Xstore_solid <- 1 - get_manure_stored_subclass(year = year, livestock_subclass = j, manure_type = 'solid'),
             Xstore_solid <- get_manure_stored_subclass(year = year, livestock_subclass = j, manure_type = 'solid'))
      
      bedding_TAN <- compute_animal_bedding_amount(year = year, livestock_class = i, livestock_subclass = j)
      calc_df[, j] <- ( housing_TAN[, j] - nh3_solid[, j] - ( bedding_TAN * f_imm ) ) * Xstore_solid
    }
    
    if (manure_spreading==TRUE) {
      write_annual_data(subfolder = 'Spreading', file = calc_df, filename = paste0('TAN_',i, '_solid'), year = year, 
                        subfolder_nameX2 = 'Direct_N_spreading')
    } else {
      write_annual_data(subfolder = 'Storage', file = calc_df, filename = paste0('TAN_',i, '_solid'), year = year, 
                        subfolder_nameX2 = 'Entering_storage')
    }
  }
}


compute_solid_N_entering_storage <- function(year, manure_spreading) {
  # computes the total N flows from manure entering storage
  # N_enter_storage = (N_housing_solid - NH3_housing_solid - N_bedding ) * Xstore_solid
  # unit: kg N yr -1
  
  animal_class <- get_animal_classes()
  
  for (i in animal_class) {
    # select only N solid MMS in housing
    animal_class_mms <- paste0(i, '_', 'solid') 
    N_build_solid <- get_MMS_subfolder_output_file(subfolder = 'Housing', 
                                              subfolderX2 = 'N_excretion', 
                                              year = year, 
                                              file_pattern = animal_class_mms)
    NH3_build_solid <- get_MMS_subfolder_output_file(subfolder = 'Housing', 
                                                    subfolderX2 = 'NH3', 
                                                    year = year, 
                                                    file_pattern = animal_class_mms)
    calc_df <- NH3_build_solid
    animal_subclass <- colnames(N_build_solid)[-c(1,2,3)]  
    
    for (j in animal_subclass) {
      
      # manure spreading condition
      ifelse(manure_spreading==TRUE,
             Xstore_solid <- 1 - get_manure_stored_subclass(year = year, livestock_subclass = j, manure_type = 'solid'),
             Xstore_solid <- get_manure_stored_subclass(year = year, livestock_subclass = j, manure_type = 'solid'))

            N_bedding_subclass <- compute_animal_bedding_N(year = year, livestock_class = i, livestock_subclass = j)
      calc_df[, j] <- ( N_build_solid[, j] - NH3_build_solid[, j] + N_bedding_subclass ) * Xstore_solid
    }
    
    if (manure_spreading==TRUE) {
      write_annual_data(subfolder = 'Spreading', file = calc_df, filename = paste0('NN_',i, '_solid'), year = year, 
                        subfolder_nameX2 = 'Direct_N_spreading')
    } else {
      write_annual_data(subfolder = 'Storage', file = calc_df, filename = paste0('NN_',i, '_solid'), year = year, 
                        subfolder_nameX2 = 'Entering_storage')
    }
  }
}


## ----------------------- COMPUTE N/TAN ENTERING STORAGE (SLURRY SYSTEMS) ----------------------- ##
## ------------------------------------------------------------------------------------------------##

compute_slurry_TAN_entering_storage <- function(year, manure_spreading) {
  # computes slurry TAN entering storage
  # slurry_TAN_storage = ( TAN_yard + TAN_housing_slurry - NH3_yard - NH3_housing_slurry )* Xstore_slurry
  # unit: kg TAN yr-1
  
  animal_class <- get_animal_classes()
  
  for (i in animal_class) {
    # select only TAN solid MMS in housing
    animal_class_mms <- paste0(i, '_slurry') 
    
    yard_TAN <- compute_TAN_flows_distribution(year = year, dist_system = 'Yards', animal_class = i)
    TAN_build_slurry <- compute_TAN_flows_distribution(year = year, dist_system = 'Housing', animal_class = animal_class_mms)
    
    yard_NH3 <- get_MMS_subfolder_output_file(subfolder = 'Yards', 
                                              subfolderX2 = 'NH3', 
                                              year = year, 
                                              file_pattern = i)
    NH3_build_slurry <- get_MMS_subfolder_output_file(subfolder = 'Housing', 
                                                      subfolderX2 = 'NH3', 
                                                      year = year, 
                                                      file_pattern = animal_class_mms)
    calc_df <- NH3_build_slurry
    animal_subclass <- colnames(NH3_build_slurry)[-c(1,2,3)]  
    
    for (j in animal_subclass) {
      
      # SPREADING CONDITIOn
      ifelse(manure_spreading==TRUE,
             Xstore_slurry <- 1 - get_manure_stored_subclass(year = year, livestock_subclass = j, manure_type = 'slurry'),
             Xstore_slurry <- get_manure_stored_subclass(year = year, livestock_subclass = j, manure_type = 'slurry'))

      calc_df[, j] <- ( TAN_build_slurry[, j] + yard_TAN[, j] - NH3_build_slurry[, j] - yard_NH3[, j] ) * Xstore_slurry
    }
    
    if(manure_spreading==TRUE) {
      write_annual_data(subfolder = 'Spreading', file = calc_df, filename = paste0('TAN_',i, '_slurry'), year = year, 
                        subfolder_nameX2 = 'Direct_N_spreading')
    } else {
      write_annual_data(subfolder = 'Storage', file = calc_df, filename = paste0('TAN_',i, '_slurry'), year = year, 
                        subfolder_nameX2 = 'Entering_storage')
    }
  }
  rm(list=c('yard_TAN', 'TAN_build_slurry', 'yard_NH3', 'NH3_build_slurry', 'calc_df'))
}




compute_slurry_N_entering_storage <- function(year, manure_spreading) {
  # computes slurry N entering storage
  # slurry_TAN_storage = ( N_yards + N_build_slurry - NH3_yards - NH3_build_slurry )* Xstore_slurry
  # unit: kg N yr-1
  
  animal_class <- get_animal_classes()
  
  for (i in animal_class) {
    # select only N slurry MMS in housing
    animal_class_mms <- paste0(i, '_', 'slurry') 
    N_yards <- get_MMS_subfolder_output_file(subfolder = 'Yards', 
                                                   subfolderX2 = 'N_excretion', 
                                                   year = year, 
                                                   file_pattern = i)
    NH3_yards <- get_MMS_subfolder_output_file(subfolder = 'Yards', 
                                                     subfolderX2 = 'NH3', 
                                                     year = year, 
                                                     file_pattern = i)
    
    N_build_slurry <- get_MMS_subfolder_output_file(subfolder = 'Housing', 
                                                    subfolderX2 = 'N_excretion', 
                                                    year = year, 
                                                    file_pattern = animal_class_mms)
    NH3_build_slurry <- get_MMS_subfolder_output_file(subfolder = 'Housing', 
                                                     subfolderX2 = 'NH3', 
                                                     year = year, 
                                                     file_pattern = animal_class_mms)
    calc_df <- NH3_build_slurry
    animal_subclass <- colnames(NH3_build_slurry)[-c(1,2,3)]  
    
    for (j in animal_subclass) {
      # SPREADING CONDITIOn
      ifelse(manure_spreading==TRUE,
             Xstore_slurry <- 1 - get_manure_stored_subclass(year = year, livestock_subclass = j, manure_type = 'slurry'),
             Xstore_slurry <- get_manure_stored_subclass(year = year, livestock_subclass = j, manure_type = 'slurry'))      
      
      calc_df[, j] <- ( N_yards[,j] + N_build_slurry[,j] - NH3_yards[, j] - NH3_build_slurry[,j] ) * Xstore_slurry
    }
    
    if(manure_spreading==TRUE) {
      write_annual_data(subfolder = 'Spreading', file = calc_df, filename = paste0('NN_',i, '_slurry'), year = year, 
                        subfolder_nameX2 = 'Direct_N_spreading')
    } else {
      write_annual_data(subfolder = 'Storage', file = calc_df, filename = paste0('NN_',i, '_slurry'), year = year, 
                        subfolder_nameX2 = 'Entering_storage')
    }
  }
  rm(list=c('N_yards', 'NH3_yards', 'N_build_slurry', 'NH3_build_slurry', 'calc_df'))
}



## ----------------------- COMPUTE SLURRY TAN STORAGE FROM EMISSIONS CAN OCCUR ----------------------- ##
## ----------------------------------------------------------------------------------------------------##

compute_storage_slurry_TAN_emission <- function(year, animal_class) {
  # computes the TAN in storage of slurry systems, corrcted to account for the N mineralization from organic N to TAN
  # to be implemented when calculating gaseous emissions from slurry storage systems
  # unit: kg TAN yr-1
  
  F_min <- 0.1 # fraction of organic N that is mineralized to TAN ;; standard EMEP (2016) value
  
  N_build_slurry <- get_MMS_subfolder_output_file(subfolder = 'Storage', 
                                                    subfolderX2 = 'Entering_storage', 
                                                    year = year, 
                                                    file_pattern = paste0('NN_', animal_class, '_slurry'))
  TAN_build_slurry <- get_MMS_subfolder_output_file(subfolder = 'Storage', 
                                                    subfolderX2 = 'Entering_storage', 
                                                    year = year, 
                                                    file_pattern = paste0('TAN_', animal_class, '_slurry'))
    calc_df <- TAN_build_slurry
    
    for (j in 4:ncol(N_build_slurry)) {
      calc_df[,j] <- TAN_build_slurry[,j] - ( (N_build_slurry[,j]-TAN_build_slurry[,j]) * F_min )
    }
    return(calc_df)
}


compute_storage_gaseous_NH3 <- function(year, TAN_storage_df, animal_class, manure_type) {
  # calculates NH3 emissions from manure storage for a given animal class and manure type
  # to be implemented in compute_storage_gaseous_emissions
  # unit: kg N-NH3 yr-1
  
  animal_subclass <- colnames(TAN_storage_df)[-c(1,2,3)]  
  
  for (j in animal_subclass) {
    ef_nh3_storage <- select_EFs_NH3_MMS(animal_class = animal_class, animal_subclass = j, 
                                         pathway = 'Storage', manure_type = manure_type)[, 'Storage']
    
    TAN_storage_df[,j] <- TAN_storage_df[,j] * ef_nh3_storage
  }
  return(TAN_storage_df)
}

compute_storage_gaseous_emissions <- function(year, gaseous_source) {
  
  animal_class <- get_animal_classes()
  animal_mms <- c('solid', 'slurry')

  for (i in animal_class) {
    
    for (j in animal_mms) {
      # call activity data of TAN in storage to calculate emissions 
      # slurry storage TAN was modified to account for organic N mineralization and conversion to TAN
      ifelse(j=='slurry',
             TAN_storage_emission <- compute_storage_slurry_TAN_emission(year, animal_class = i),
             TAN_storage_emission <- get_MMS_subfolder_output_file(subfolder = 'Storage', 
                                                                   subfolderX2 = 'Entering_storage', 
                                                                   year = year, 
                                                                   file_pattern = paste0('TAN_', i, '_solid')))
      if (gaseous_source != 'NH3') {
        # call appropriate storage EF
        ef_storage_gaseous <- select_storage_EFs_gaseous(gaseous_source = gaseous_source, animal_class = i, manure_type = j)
  
        # subset columns to calculate emissions
        calc_cols <- seq(4, ncol(TAN_storage_emission))
        
        # compute emissions for the gaseous source specified
        TAN_storage_emission[, calc_cols] <- sapply(TAN_storage_emission[, calc_cols], function(x) x*ef_storage_gaseous)
      }
      # if gaseous source is NH3
      else {
        TAN_storage_emission <- compute_storage_gaseous_NH3(year = year,
                                                            TAN_storage_df = TAN_storage_emission, 
                                                            animal_class = i, 
                                                            manure_type = j)
      }
      write_annual_data(subfolder = 'Storage', file = TAN_storage_emission, filename = paste0(i, '_', j), 
                        year = year, subfolder_nameX2 = gaseous_source)
    }
  }
}

