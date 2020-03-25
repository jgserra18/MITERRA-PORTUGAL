source('./Main_functions.R')



## ----------------------- CREATE DIRECTORY AND SUBFOLDERS ----------------------- ##
## -------------------------------------------------------------------------------##

create_fertilization_dir <- function() {
  
  dir_path <- create_module_dir(module.name =  'Fertilization_module')
}

create_fertilization_subfolders <- function(subfolder_name) {
  
  subfolder_path <- create_module_output_subfolder(module.name = 'Fertilization_module', subfolder.name = subfolder_name)
}


## ----------------------- CROP DATA GETTERS ----------------------- ##
## ------------------------------------------------------------------##

get_crop_yields <- function(year, main_crop, crop) {
  # get crop yields
  # unit: kg dm ha-1
  
  crop_yields <- get_crop_data(year, 'Yields', main_crop, crop)
  return(crop_yields)
}

get_crop_areas <- function(year,  main_crop, crop) {
  # get crop area
  # unit: ha yr-1
  
  crop_area  <- get_crop_data(year, 'Areas', main_crop, crop)[, -1]
  return(crop_area)
}



## ----------------------- SUPPORT DATA GETTERS ----------------------- ##
## ---------------------------------------------------------------------##

select_fertilization_support_folder <- function(support_data) {
  # selects the specified support data folder regarding fertilization support
  # e.g., Fertiliser_modifier
  
  crop_folder <- load_raw_data('Crop_data')
  fert.support_folder <- list.files(path = crop_folder, pattern = 'Fertilization_support', full.names = TRUE)
  
  # select support data
  fert.support_param <- list.files(path = fert.support_folder, pattern = support_data, full.names = TRUE)
  return(fert.support_param)
}

get_historical_fertilizerN <- function(year, col) {
  # gets the historical fertiliser N dataset from APA (2018)
  # selects the total fertiliser N from the year specified
  # unit: ktonnes N yr-1
  
  fert.support_param <- list.files(select_fertilization_support_folder('National_fertilizer_historical'), full.names = TRUE)
  historical_fertN <- read.csv(fert.support_param)
  
  select_yr <- which(historical_fertN[, 1]==year)
  select_fert <- historical_fertN[select_yr, col]
  return(select_fert)
  rm(list=c('fert.support_param', 'historical_fertN', 'select_yr'))
}

get_fertilization_support_data <- function(support_data, main_crop, crop) {
  
  fert.support_param <- select_fertilization_support_folder(support_data)
  if (support_data != 'National_fertilizer_historical') {
    
    main.crop_support <- read.csv(
      list.files(path = fert.support_param, pattern = main_crop, full.names = TRUE))
    crop_support <- main.crop_support[, crop]
    return(crop_support)
  }
}


## ----------------------- OUTPUT DATA GETTERS ----------------------- ##
## --------------------------------------------------------------------##

get_fertilization_output <- function(submodule, submoduleX2, file_pattern, submoduleX3) {
  # fertilization data getter
  # wraup up
  
  output_file <- get_module_subfolder_output(module = 'Fertilization_module', 
                                             submodule = submodule, 
                                             submoduleX2 = submoduleX2, 
                                             submoduleX3 = submoduleX3, 
                                             file_pattern = file_pattern)
}


get_crop_fert_rate <- function(main_crop, crop, year) {
  # gets crop fertilization rates for a given year
  # spatially disaggregates fert rates from the agrarian to the municipality level
  # unit: kg N/ha/yr
  
  crop_fert_rate <- get_fertilization_output(submodule = 'Fertilization_rates', 
                                             submoduleX2 = main_crop, 
                                             file_pattern = crop)
  colnames(crop_fert_rate) <- gsub(pattern = 'X', replacement = '', x = colnames(crop_fert_rate))
  crop_fert_rate <- crop_fert_rate[, c(1,2, which(names(crop_fert_rate)==year))]
  
  # spatial disaggregation
  crop_fert_rate <- spatial_disagg_agrarian_muni(crop_fert_rate)
  
  return(crop_fert_rate)
}

## ----------------------- CROP NAMES ----------------------- ##
## -----------------------------------------------------------##


get_all_maincrop_names <- function(intensive) {
  # define main crop classes and drop intensive pasture and horticulture, which are treated separately
  # e.g. get_maincrop_names(intensive==TRUE)
  # please don't confunde these functions with those from irrigation
  
  crop_classes <- gsub('.csv', '', list.files(select_fertilization_support_folder('Fertiliser_modifier')))
  id_exceptions <- which(crop_classes=='pastures' | crop_classes=='horticulture')
  
  # select or unselect intensive pastures
  if (missing(intensive)==TRUE) {
    crop_classes <- crop_classes
  } else if (intensive==FALSE) {
    crop_classes <- crop_classes[-id_exceptions]
  } else if (intensive==TRUE) {
    crop_classes <- crop_classes[id_exceptions]
  }
  return(crop_classes)
}

get_all_crop_names <- function(main.crop) {
  
  crop_data_folder <- load_raw_data(subfolder = 'Crop_data')
  select_crop_param <- list.files(path = crop_data_folder, pattern = 'Areas', full.names = T)
  census <- list.files(path = select_crop_param, pattern = 'Census', full.names = T)
  maincrop <- list.files(path = census, pattern = main.crop, full.names = T)
  crops <- gsub('.csv', '', list.files(maincrop))
  
  return(crops)
  rm(list=c('crop_data_folder', 'select_crop_param', 'census', 'maincrop'))
}

get_fertilization_crops <- function(main_crop) {
  # this function is a wrap up of get_all_crop_names
  # however, rather than using both perma and tempo grassland terminology
  # it uses intensive_pasture
  
  ifelse(main_crop=='pastures',
         crops <- 'intensive_pasture',
         crops <- get_all_crop_names(main.crop = main_crop))
  return(crops)
}

## ----------------------- CROP N REQUIREMENTS ----------------------- ##
## --------------------------------------------------------------------##

compute_yield_difference <- function(main_crop, crop, year) {
  # computes the yield difference = real_yield - reference_yield (in kg dm ha-1)
  # and converts it to ton ha-1
  # if year is not specified yield differences are ccomputed for all years
  # unit: ton dm ha-1
  
  ref_yield <- get_fertilization_support_data(support_data = 'Reference_Yields', main_crop, crop)
  
  ifelse(missing(year)==TRUE,
         real_yield <- get_crop_yields(main_crop = main_crop, crop = crop),
         real_yield <- get_crop_yields(year = year, main_crop = main_crop, crop = crop))
  yield_dif <- real_yield
  
  ifelse(missing(year)==TRUE,
         yield_dif[, seq(3, ncol(real_yield))] <- mapply(function(x, y){(x-y)/1000}, x=real_yield[, seq(3, ncol(real_yield))], y = ref_yield),
         yield_dif[, 3] <- ((real_yield[, 3]-ref_yield)/1000))
  return(yield_dif)
}


compute_crop_fertilizer_rates <- function(main_crop, crop, year) {
  # computes the fertilization rates for a given crop
  # if year is not specified the historical fertilization rates for each agrarian region level are computed
  # fert_rate = rec_fert_rate + (real_yield - ref_yield)*fert_modifier
  # unit: kg N ha-1 yr-1
  
  recommended_fert <- get_fertilization_support_data('Recommended_fertiliser', main_crop, crop)
  
  # for horticulture and pasture yields are not accounted, only the recommended N fertilization
  if (main_crop == 'horticulture' | main_crop =='pastures') {
    calc_df <- compute_yield_difference('cereals', 'rice', 2009)[, -4]
    calc_df[, as.character(year)] <- recommended_fert
    return(calc_df)
  } 
  else {
    yield_dif <- compute_yield_difference(main_crop, crop, year)
    fert_modifier <- get_fertilization_support_data('Fertiliser_modifier', main_crop, crop)
    fert_rate <- yield_dif
    
    ifelse(ncol(yield_dif)>3, feature <- 3:ncol(yield_dif),
           feature <- 3)
    
    for (i in feature) {
      fert_rate[, i] <- recommended_fert + fert_modifier *  yield_dif[, i]
    }
    fert_rate[fert_rate<0] <- 0
    return(fert_rate)
  }
}

compute_fertilizer_rates <- function(year) {
  # this function calls each main crop and associated crops
  # and computes crop N requirements and exports all the data to the subfolder Fertilization_rates
  # unit: kg N ha-1 yr-1
  
  main_crops <- get_all_maincrop_names()
  
  for (i in main_crops) {
    crops <- get_fertilization_crops(main_crop = i)
    for (j in crops) {
      print(paste0(i, ' and ', j))
      crop_fert_rate <- compute_crop_fertilizer_rates(main_crop = i, crop = j, year = year)
      write_output_subfolder(module_name = 'Fertilization_module',
                             subfolder_name = 'Fertilization_rates', 
                             file =crop_fert_rate, filename = j, 
                             subfolderX2_name = i)
    }
  }
}



compute_crop_N_requirements <- function(year) {
  # computes the crop N requirements for each crop for a given year
  # Crop_Nreq = crop_FertRate * crop_Area
  # unit: kg N yr-1
  
  main_crops <- get_all_maincrop_names()

  for (i in main_crops) {
    crops <- get_fertilization_crops(main_crop = i)
    
    for (j in crops) {
      crop_area <- get_crop_areas(year, i, j)[, 4] # crop area (in ha)
      crop_fert_rate <- get_crop_fert_rate(i, j, year)
      crop_Nreq <- crop_fert_rate
      crop_Nreq[, 4] <- crop_fert_rate[, 4] * crop_area
      write_annual_data(module_name = 'Fertilization_module',
                        subfolder_name = 'Crop_N_requirements',
                        file = crop_Nreq,
                        year = year,
                        filename = j,
                        subfolder_nameX2 = i)
      
      rm(list=c('crop_area', 'crop_fert_rate'))
    }
  }
}
