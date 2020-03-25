source('./CropProduction_module/Functions/fodder_dairy_functions.R')
source('./CropProduction_module/Functions/fodder_NonDairy_functions.R')
source('./CropProduction_module/Functions/crop_Nremoval_functions.R')
source('./CropProduction_module/Functions/crop_Residues_functions.R')
source('./Main_functions.R')


## ----------------------- CROP ACREAGE DISTIBUTION FUNCTIONS----------------------- ##
## ----------------------------------------------------------------------------------##

crop_acrea_dist_path <- function(N_output, year, main_crop) {
  # creates the necessary directories for the crop acreage distribution within CropProduction_module
  # N_output is either Crop_Nroughage, Crop_Nres_burnt Crop_Nres_removed, Crop_Nremoval
  
  module_path <- select_module_output('CropProduction_module')
  submodule_path <- file.path(module_path, 'Crop_acreage_distribution')
  dir.create(submodule_path, showWarnings = FALSE)
  nflow_path <- file.path(submodule_path, N_output)
  dir.create(nflow_path, showWarnings = FALSE)
  year_path <- file.path(nflow_path, year)
  dir.create(year_path, showWarnings = FALSE)
  maincrop_path <- file.path(year_path, main_crop)
  dir.create(maincrop_path, showWarnings = FALSE)
  
  return(maincrop_path)
  rm(list=c('module_path', 'submodule_path', 'nflow_path', 'year_path', 'maincrop_path'))
}

write_crop_acreage_dist <- function(N_output, year, main_crop, crop, file) {
  # writes directly to Crop_acreage_distribution

  maincrop_path <- crop_acrea_dist_path(N_output, year, main_crop)
  file_path <- file.path(maincrop_path, paste0(crop, '.csv'))
  write.csv(file, file_path, row.names = F)
  rm(list=c('module_path', 'submodule_path', 'nflow_path', 'year_path', 'maincrop_path', 'file_path'))
}

get_crop_acreage_dist_Noutput <- function(N_output, year, main_crop, crop) {
  
  maincrop_path <- crop_acrea_dist_path(N_output, year, main_crop)
  crop_path <- list.files(path = maincrop_path, pattern = crop, full.names = TRUE)
  crop_path <- read.csv(crop_path)
  return(crop_path)
  rm(list = 'maincrop_path')
}



## -----------------------  FODDER PRODUCTION------------------------------- ##
## ----------------------- SPREAD ROUGHAGE N TO CROPS----------------------- ##
## --------------------------------------------------------------------------##


compute_crop_roughage_fracs <- function(year) {
  # computes the fraction of roughage crop acreage vs the total roughage acreage
  # main crops are pastures and forage crops
  
  main_df <- create_main_csv()
  
  # aggregate roughage crop acreage into one dataset --------------------
  roughage_main_crops <- c('forage', 'pastures')
  for (i in roughage_main_crops) {
    crop <- get_all_crop_names(main.crop = i)
    for (j in crop) {
      crop_area <- get_crop_areas(year = year, main_crop = i, crop = j)
      main_df[, j] <- crop_area[, as.character(year)]
    }
  }
  main_df[, 'total_roughage'] <- rowSums(main_df[, seq(4, ncol(main_df))])
  
  # calculate roughage crop acreage FRAC vs total roughage acreage -----
  calcs <- seq(4, ncol(main_df), 1)
  main_df[, calcs] <- sapply(calcs, function(x) round(
    main_df[, x] / main_df[, 'total_roughage'], 3))
  
  return(main_df)
  rm(list = c('roughage_main_crops', 'crop', 'crop_area', 'calcs'))
}


disaggregate_crop_roughageN <- function(year) {
  # spread total roughage N to the different roughage crops based on their acreage
  # unit: kg N yr-1
  
  # load total roughage N for a given year --------------------------------------
  roughage_N <- get_module_subfolder_output(module = 'CropProduction_module', 
                                            submodule = 'Roughage_N', 
                                            submoduleX2 = year, 
                                            file_pattern = 'Total_roughageN')
  roughage_N <- roughage_N[, c(1,2,3, ncol(roughage_N))]
  
  # load roughage crop FRACS ----------------------------------------------------
  roughage_fracs <- compute_crop_roughage_fracs(year)
  
  # distribute roughage N for each crop based on the acreage FRACS --------------
  crop_roughage_N <- roughage_fracs
  
  calcs <- seq(4, ncol(roughage_fracs))
  crop_roughage_N[, calcs] <- sapply(calcs, function(x) round(
    crop_roughage_N[, x] * roughage_N[, 4], 2))
  
  return(crop_roughage_N)
  rm(list=c('roughage_N', 'roughage_fracs', 'calcs'))
}

compute_crop_roughageN_ha <- function(year) {
  # calculates the crop roughage N removal
  # unit: kg N (crop area)-1 yr-1
  
  crop_roughageN <- disaggregate_crop_roughageN(year)
  
  roughage_main_crops <- c('forage', 'pastures')
  for (i in roughage_main_crops) {
    crop <- get_all_crop_names(main.crop = i)
    for (j in crop) {
      # calculate crop roughage N per crop ha
      crop_area <- get_crop_areas(year = year, main_crop = i, crop = j)
      crop_area[, as.character(year)] <- round ( crop_roughageN[, j] / crop_area[, as.character(year)], 2)
      crop_area <- data_cleaning(crop_area)
      write_crop_acreage_dist(N_output = 'Crop_Nroughage', 
                              year = year,
                              main_crop = i, 
                              crop = j, 
                              file = crop_area)
    }
  }
  rm(list = c('crop_roughageN', 'roughage_main_crops', 'crop', 'crop_area'))
}



## ----------------------- CROP PRODUCTION DISTRIBUTION---------------------- ##
## ---------------------------------------------------------------------------##

compute_crop_Nremoval_ha <- function(year) {
  # computes the crop N uptake (except for roughage crops)
  # in terms of kg N (crop area)-1 yr-1
  
  main_crops <- get_maincrops_offtake()
  
  for (i in main_crops) {
    maincrop_df <- create_main_csv()
    crops <- get_all_crop_names(main.crop = i)
    for (j in crops) {
      crop_area <- get_crop_areas(year = year, main_crop = i, crop = j)
      crop_offtake <- get_module_subfolder_output(module = 'CropProduction', 
                                                  submodule = 'Crop_Nremoval', 
                                                  submoduleX2 = i, 
                                                  submoduleX3 = year,
                                                  file_pattern = j)
      crop_offtake[, as.character(year)] <- round( crop_offtake[, as.character(year)] / crop_area[, as.character(year)], 2)
      crop_offtake <- data_cleaning(crop_offtake)
      write_crop_acreage_dist(N_output = 'Crop_Nremoval', 
                              year = year,
                              main_crop = i, 
                              crop = j, 
                              file = crop_offtake)
    }
  }
}



## ----------------------- RESIDUES CROP DISTRIBUTION ---------------------- ##
## --------------------------------------------------------------------------##


compute_crop_Nres_burnt_ha <- function(year) {
  # computes the crop N uptake (except for roughage crops)
  # in terms of kg N (crop area)-1 yr-1
  
  main_crops <- get_burnt_maincrops()
  
  for (i in main_crops) {
    maincrop_df <- create_main_csv()
    crops <- get_all_crop_names(main.crop = i)
    for (j in crops) {
      crop_area <- get_crop_areas(year = year, main_crop = i, crop = j)
      res_burtN <- get_module_subfolder_output(module = 'CropProduction', 
                                               submodule = 'Residues_burnt', 
                                               submoduleX2 = i, 
                                               file_pattern = j, 
                                               submoduleX3 = year)
      res_burtN[, as.character(year)] <- round( res_burtN[, as.character(year)] / crop_area[, as.character(year)], 2)
      res_burtN <- data_cleaning(res_burtN)
      write_crop_acreage_dist(N_output = 'Crop_Nres_burnt', 
                              year = year,
                              main_crop = i, 
                              crop = j, 
                              file = res_burtN)
    }
  }
  rm(list=c('main_crops', 'maincrop_df', 'crops', 'crop_area', 'res_burtN'))
}


compute_crop_Nres_removed_ha <- function(year) {
  # computes the crop N uptake (except for roughage crops)
  # in terms of kg N (crop area)-1 yr-1
  
  main_crops <- c('cereals', 'industry', 'potato', 'pulses', 'pastures')
  
  for (i in main_crops) {
    maincrop_df <- create_main_csv()
    crops <- get_all_crop_names(main.crop = i)
    for (j in crops) {
      crop_area <- get_crop_areas(year = year, main_crop = i, crop = j)
      res_removedN <- get_module_subfolder_output(module = 'CropProduction', 
                                           submodule = 'Residues_removed', 
                                           submoduleX2 = i, 
                                           file_pattern = j, 
                                           submoduleX3 = year)
      res_removedN[, as.character(year)] <- round( res_removedN[, as.character(year)] / crop_area[, as.character(year)], 2)
      res_removedN <- data_cleaning(res_removedN)
      write_crop_acreage_dist(N_output = 'Crop_Nres_removed', 
                              year = year,
                              main_crop = i, 
                              crop = j, 
                              file = res_removedN)
    }
  }
  rm(list=c('main_crops', 'maincrop_df', 'crops', 'crop_area', 'res_removedN'))
}
