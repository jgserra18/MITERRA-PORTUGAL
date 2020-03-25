source('./MMS_module/Functions/Totals_aggregation_MMS.R')
source('./Fertilization_module/Functions/Crop_N_requirements.R')
source('./Fertilization_module/Functions/Manure_distribution.R')
source('./MMS_module/Functions/Nex_computation.R')
source('./Fertilization_module/Functions/Biosolids_distribution.R')
source('./Main_functions.R')



## ----------------------- MANURE DISTRIBUTION ----------------------- ##
##---------------------------------------------------------------------##

Nreq_minus_manure_frv <- function(year, main_crop, crop) {
  # support function
  # represents the subtraction operation between crop N requirements and the N provided by manure through the crop distribution
  # unit: kg N yr-1
  
  crop_Nreq <- get_fertilization_output(submodule = 'Crop_N_requirement',submoduleX2 = main_crop, file_pattern = crop, submoduleX3 = year)
  crop_manure_FRV <- get_module_subfolder_output(module = 'Fertilization_module', 
                                                 submodule = 'Manure_crop_distribution', 
                                                 file_pattern = paste0(crop, '_total'),
                                                 submoduleX2 = main_crop,  
                                                 submoduleX3 = year)
  crop_Nreq[, 4] <- crop_Nreq[, 4] - crop_manure_FRV[, 4]
  return(crop_Nreq)
  rm(list=c('crop_Nreq', 'crop_manure_FRV'))
}


compute_manure_surplus <- function(crop_Nreq_update_df, year, main_crop, crop) {
  # compute crop manure N_FRV surplus after manure spreading
  # comparatively to crop N requirements
  # unit: kg N yr-1
  
  # identify municipalities with manure surplus (negative_idx)
  negativ_idx <- which(crop_Nreq_update_df[, 4]<0)
  crop_Nreq_update_df[negativ_idx, 4] <- round(crop_Nreq_update_df[negativ_idx, 4]  * -1, 0)

  # set municipalities wo/ manure surplus to 0
  positive_idx <- -negativ_idx
  crop_Nreq_update_df[positive_idx, 4] <- 0

  write_annual_data(module_name = 'Fertilization_module',
                    subfolder_name = 'Manure_surplus',
                    file = crop_Nreq_update_df,
                    filename = crop,
                    year = year,
                    subfolder_nameX2 = main_crop)
}


compute_manure_application_rate <- function(crop_Nreq_update, year, main_crop, crop) {
  # computes crop manure application rates according to the manure crop distribution mechanism and crop N requirements
  # unit: kg N (ha crop)-1 yr-1
  
  # condition 1: if crop N requirements <0 && ha>0, manure_app <- crop N req / ha
  # condition 2: if crop N requirements > 0 && ha>0, manure_app <- manure_H / ha
  # condition 3: if crop N requirements = 0 && ha>0, manure_app <- 0
  
  crop_Nreq <- get_fertilization_output(submodule = 'Crop_N_requirement',
                                        submoduleX2 = main_crop, 
                                        file_pattern = crop, 
                                        submoduleX3 = year)
  crop_manure_FRV <- get_module_subfolder_output(module = 'Fertilization_module', 
                                                 submodule = 'Manure_crop_distribution', 
                                                 file_pattern = paste0(crop, '_total'),
                                                 submoduleX2 = main_crop,  
                                                 submoduleX3 = year)
  calc_df <- crop_Nreq_update
  crop_area <- get_crop_areas(year, main_crop, crop)
  
  for (i in 1:nrow(calc_df)) {
    
    # condition 1
    if (crop_area[i, 4]>0 && calc_df[i, 4]<0) {
      calc_df[i, 4] <- crop_Nreq[i, 4] / crop_area[i,4]
    }
    # condition 2
    else if (crop_area[i, 4]>0 && calc_df[i, 4]>0) {
      calc_df[i, 4] <- crop_manure_FRV[i, 4] / crop_area[i,4]
    }
    # condition 3
    else {
      calc_df[i,4] <- 0
    }
  }
  write_annual_data(module_name = 'Fertilization_module',
                    subfolder_name = 'Manure_rates',
                    file = calc_df,
                    year = year,
                    filename = crop,
                    subfolder_nameX2 = main_crop)
  
  rm(list=c('crop_Nreq', 'crop_manure_FRV', 'crop_area', 'calc_df'))
}


cropNreq_after_manure_application <- function(year, main_crop, crop) {
  # function that implements thee calculations related to the remaining crop N requirements after manure spreading
  # similarly, it exports the manure surplus per crop
  # unit: kg N yr-1
  
  # Calculate crop N requirements - crop manure N FRV ------------------------------------
  crop_Nreq_update <- Nreq_minus_manure_frv(year, main_crop, crop)
  
  # Calculate manure surplus following crop manure application ---------------------------
  manure_surplus <- compute_manure_surplus(crop_Nreq_update_df = crop_Nreq_update, 
                                           year = year, 
                                           crop = crop, 
                                           main_crop = main_crop)
  # Calculate crop manure application rates
  manure_app_rate <- compute_manure_application_rate(crop_Nreq_update = crop_Nreq_update, 
                                                     year = year, 
                                                     main_crop = main_crop, 
                                                     crop = crop)
  
  # Correct municipalities where manure N application exceeded crop N demand (i.e., where there is manure surplus)
  crop_Nreq_update[crop_Nreq_update<0] <- 0
  return(crop_Nreq_update)
}


# FUCK BIOSOLID EXCESS
cropNreq_after_biosolids_application <- function(crop_Nreq_update, year, main_crop, crop) {
  # calculates (updated) crop N requirements following biosolids
  # the output equals to crop fertiliser N, but it is a secret ehehe
  # unit: kg N yr-1
  
  # crop N requirements following manure spreading
  Ndemand_man <- crop_Nreq_update
  
  # load crop sludge N FRV
  sludgeN_FRV_crop <- get_module_subfolder_output(module = 'Fertilization_module', 
                                                  submodule = 'Biosolids_crop_distribution', 
                                                  submoduleX2 = main_crop,
                                                  file_pattern = crop, 
                                                  submoduleX3 = year)
  
  # calculate updated crop N requirements following biosolids
  Ndemand_man[, 4] <- Ndemand_man[, 4] - sludgeN_FRV_crop[, 4]
  Ndemand_man[Ndemand_man<0] <- 0
  return(Ndemand_man)
}

compute_vegetable_fertiliserN <- function(year) {
  
  main_crops <- c('industry', 'horticulture')
  for (i in main_crops) {
    crops <- get_fertilization_crops(main_crop = i)
    for (j in crops) {
      crop_fert_rate <- get_crop_fert_rate(i, j, year)
      
      crop_area <- get_crop_areas(year, i, j)
      crop_fert_rate[, as.character(year)] <- crop_fert_rate[, as.character(year)]  * crop_area[, 4]
      write_annual_data(module_name = 'Fertilization_module',
                        subfolder_name = 'Fertiliser_N_unadjusted',
                        file = crop_fert_rate,
                        year = year,
                        filename = j,
                        subfolder_nameX2 = i)
      
    }
  }
}


compute_mechanism_crop_fertiliserN <- function(year) {
  # this function employs several sub-functional routines
  # 1 - MANURE: 
  # 1.1 - calculates the difference of crop N requirements and crop manure N FRV (kg N yr-1)
  # 1.2 - calculates manure surplus following spreading (kg N yr-1)
  # 1.3 - calculates crop manure application rates (kg N ha_crop-1 yr-1)
  # 2 - BIOSOLID
  # 2.1 - calculates the difference of the remaining crop N demand (1.1) and crop biosolid N FRV (kg N yr-1)
  # 2.2 Sludge surplus is not calculated currently  
  # 3 - Fertiliser
  # 3.1 - Calculates unfit rop fertiliser N (from 2.1)
  
  main_crops <- append(get_fodder_main.crops(), get_non.fodder_main.crops())
  for (i in main_crops) {
    crops <- get_fertilization_crops(main_crop = i)
    for (j in crops) {
      print(paste0('Computing fertiliser N and real fertiliser N rates for ', j))
      # calculate (i) N demand after manure application, (ii) manure surplus and (iii) manure N application rates
      Ndemand_man <- cropNreq_after_manure_application(year, main_crop = i, crop = j)
      
      # calculate crop fertiliser N
      fertN_crop <- cropNreq_after_biosolids_application(crop_Nreq_update = Ndemand_man, 
                                                         year = year, 
                                                         main_crop = i, 
                                                         crop = j)
      write_annual_data(module_name = 'Fertilization_module',
                        subfolder_name = 'Fertiliser_N_unadjusted',
                        file = fertN_crop,
                        year = year,
                        filename = j,
                        subfolder_nameX2 = i)
    }
  }
  rm(list=c('Ndemand_man', 'fertN_crop', 'fertN_crop', 'crop_area'))
}






