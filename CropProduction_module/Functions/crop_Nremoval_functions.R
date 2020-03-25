source('./Main_functions.R')
source('./Fertilization_module/Functions/Crop_N_requirements.R')


## ----------------------- CROP DATA GETTERS ----------------------- ##
## ------------------------------------------------------------------##

get_crop_offtake_params <- function(offtake_param) {
  # gets the necessary params to compute BNF for legumes
  # legume category is either Forage or Grain
  
  crop_param <- get_activity_data(subfolder = 'Crop_production', 
                                 subfolderX2 = offtake_param, 
                                 file_pattern = offtake_param)
  return(crop_param)
}


get_maincrops_offtake <- function() {
  # define the main crop classes for crop N offtake calculation
  
  main_crops <- get_all_maincrop_names()
  remove_ids <- which(main_crops=='forage' | main_crops == 'pastures')
  main_crops <- main_crops[-remove_ids]
  return(main_crops)
}

get_spatially_disaggregated_cropYields <- function(year, main_crop, crop) {
  # spatially disaggregates crop yields from the agrarian to the municipality level
  # unit: kg dm ha-1
  
  crop_yield_agrarian <- get_crop_yields(year, main_crop, crop)
  crop_yield_muni <- spatial_disagg_agrarian_muni(agrarian_df = crop_yield_agrarian)
  return(crop_yield_muni)
  rm(list='crop_yield_agrarian')
}


## ----------------------- BIOMASS PRODUCTION----------------------- ##
## ------------------------------------------------------------------##

select_crop_offtake_params <- function(offtake_param, crop) {
  # selects the crop offtake param (i.e., DM fraction or N removal coefficient) 
  # for the specified crop
  
  crop_param <- get_crop_offtake_params(offtake_param)
  crop_id <- which(crop_param[, 2]==crop)
  get_crop_param <- crop_param[crop_id, 3]
  return(get_crop_param)
  rm(list=c('crop_param', 'crop_id'))
}


compute_crop_DM_production <- function(year, main_crop, crop) {
  # crop DM prod = crop area * crop yield * DM_frac
  # unit: kg dm yr-1
  
  crop_area <- get_crop_areas(year = year, main_crop = main_crop, crop = crop)
  crop_yield <- get_spatially_disaggregated_cropYields(year = year, main_crop = main_crop, crop = crop)
  crop_DMfrac <- select_crop_offtake_params(offtake_param = 'DM', crop = crop)
  
  # calculate crop DM production
  crop_area[, as.character(year)] <- round(crop_area[, as.character(year)] * crop_yield[, as.character(year)]* crop_DMfrac, 2)
  return(crop_area)
  rm(list=c('crop_yield', 'crop_DMfrac'))
}

## ----------------------- CROP N REMOVAL----------------------- ##
## --------------------------------------------------------------##

compute_crop_N_offtake <- function(year, main_crop, crop, write) {
  # calculates crop N removal according to the yearly biomass production (in kg DM yr-1)
  # note that because the N offtake coefficient is in kg N tonnes dm-1, the yearly biomass production was converted to tonnes dm
  # unit: kg N yr-1
  
  crop_Nofftake <- select_crop_offtake_params('N_offtake', crop)
  crop_DM_prod <- compute_crop_DM_production(year, main_crop, crop)
  crop_DM_prod[, as.character(year)] <- round( crop_DM_prod[, as.character(year)]/1000 * crop_Nofftake, 2)
  
  if (missing(write)==TRUE) {
    return(crop_DM_prod)
  } else {
    write_annual_data(module_name = 'CropProduction_module', 
                      subfolder_name = 'Crop_Nremoval', 
                      year = year, 
                      file = crop_DM_prod, 
                      filename = crop, 
                      subfolder_nameX2 = main_crop) 
  }
  rm(list=c('crop_Nofftake'))
}



compute_total_N_offake <- function(year) {
  # general function to compute crop N offtake for each relevant crop
  # unit: kg N yr-1
  
  main_crops <- get_maincrops_offtake()
  
  for (i in main_crops) {
    crops <- get_all_crop_names(main.crop = i)
    for (j in crops) {
      compute_crop_N_offtake(year = year, main_crop = i, crop = j, write = TRUE)
    }
  }
}

## ----------------------- CROP N REMOVAL !! TOTALS ----------------------- ##
## -------------------------------------------------------------------------##


compute_maincrop_N_offtake <- function(year) {
  # this function has two primary goals:
    # 1 - aggregate crop N offtake into individual datasets according to main crops (e.g., maize, wheat, total_cereals)
    # 2 - aggregate the totals for each main crop class into one dataset (e.g., total_cereals, total_pulses, total_municipality)
  # unit: kg N yr-1
  
  total_df <- create_main_csv()
  
  main_crops <- get_maincrops_offtake()
  
  for (i in main_crops) {
    maincrop_df <- create_main_csv()
    crops <- get_all_crop_names(main.crop = i)
    for (j in crops) {
      crop_offtake <- get_module_subfolder_output(module = 'CropProduction', 
                                                  submodule = 'Crop_Nremoval', 
                                                  submoduleX2 = i, 
                                                  submoduleX3 = year,
                                                  file_pattern = j)
      maincrop_df[, j] <- crop_offtake[, as.character(year)]
    }
    
    if (ncol(maincrop_df)>4) {
      maincrop_df[, paste0('total_', i)] <- rowSums(maincrop_df[, seq(4, ncol(maincrop_df))])
    } else {
      colnames(maincrop_df)[4] <- paste0('total_', i)
    }
    # write main crop files with the respective crops
    write_annual_data(module_name = 'CropProduction_module', 
                      subfolder_name = 'Crop_Nremoval', 
                      year = year, 
                      file = maincrop_df, 
                      filename = paste0(i, '_total'), 
                      subfolder_nameX2 = 'MainCrop_Totals')  
    
    # add main crop totals ONLY to total_df
    total_df[, paste0('total_', i)] <- maincrop_df[, paste0('total_', i)]
  }
  # sum total crop N offtake for each municipality
  total_df[, 'total_municipality'] <- rowSums(total_df[, seq(4, ncol(total_df))])
  write_annual_data(module_name = 'CropProduction_module', 
                    subfolder_name = 'Crop_Nremoval', 
                    year = year, 
                    file = total_df, 
                    filename = 'Total_municipality', 
                    subfolder_nameX2 = 'MainCrop_Totals')
}


                      
