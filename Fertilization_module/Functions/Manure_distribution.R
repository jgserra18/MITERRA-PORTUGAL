source('./MMS_module/Functions/Totals_aggregation_MMS.R')
source('./Fertilization_module/Functions/Crop_N_requirements.R')
source('./MMS_module/Functions/Nex_computation.R')

## ----------------------- ANIMAL DIVISION INTO RUMINANTS AND NON-RUMINANTS ----------------------- ##
## -------------------------------------------------------------------------------------------------##

## Manure distribution assumptions
## 25 % of pig manure AND 100% ruminant manure --> fodder crops + intensive pasture
## 100% of poultry, equides, rabbits and 75% pig manure --> non-fodder crops


get_ruminant_animals <- function() {
  
  ruminants <- c('Bovine', 'Sheep', 'Goats', 'Swine')
  return(ruminants)
}

get_non.ruminant_animals <- function() {
  
  others <- c('Equides', 'Poultry', 'Rabbits', 'Swine')
  return(others)
}

## ----------------------- CROP DIVISION INTO FODDER AND NON-FODDER----------------------- ##
## ----------------------------------------------------------------------------------------##

get_fodder_main.crops <- function() {
  
  fodder_crops <- c('forage', 'pastures')
  return(fodder_crops)
}

get_non.fodder_main.crops <- function() {
  
  non.fodder_crops <- get_all_maincrop_names(intensive = FALSE)
  return(non.fodder_crops[-which(non.fodder_crops=='forage' | non.fodder_crops=='industry')])
}

## ----------------------- GETTERS ----------------------- ##
## --------------------------------------------------------##


get_FRV <- function(organic_fertilizer_type) {
  # gets the fertilizer replacement values from Hijbeek et al (2018)
  # to distribute the organic N to the crop N requirements
  
  FRV_df <- data.frame(Org_fert_type = c('slurry', 'solid', 'sludge'),
                       FRV = c(1.12, 0.58, 0.58))
  return(FRV_df[which(FRV_df[, 1]==organic_fertilizer_type), 2])
}


get_manure_crop_dist_weights <- function(crop) {
  # gets the manure crop distribution weights for the fertilizer distribution mechanism
  # these are derived from an AHP and it is only the final output
  
  crop_manure_weights <- select_EMEP_param_folders(subfolder = 'Manure_crop_distribution')
  crop_manure_weights <- read.csv(list.files(crop_manure_weights, full.names = TRUE))
  return(crop_manure_weights[which(crop_manure_weights[, 2]==crop), 3])
}



## ----------------------- PREPARE MANURE CROP DISTRIBUTION BASED ON MANURE_TYPE ----------------------- ##
## ------------------------------------------------------------------------------------------------------##


compute_animal_gross_manureN <- function(year, animal_division, manure_type) {
  # gets the total gross manure N available for spreading for each animal category
  # this follows the animal division into ruminant and non-ruminant animal
  # unit: kg N yr-1
  
  # Establish swine condition for the manure distribution: 25% to fodder cromps, 75% for non-fodder cromps
  if (animal_division == 'ruminants') {
    animals <- get_ruminant_animals()
    pigs_prop <- 0.25
  } else {
    animals <- get_non.ruminant_animals()
    pigs_prop <- 0.75
  }

  # gross soil manure N derived 
  gross_manureN <- aggregate_totals(year = year, 
                                    subfolder =  'Spreading', 
                                    subfolderX2 = 'Gross_soil_manure_N', 
                                    manure_type = manure_type, 
                                    animal_class_list = animals)
  # correct pigs available manrue for spreading
  gross_manureN[, 'Swine'] <- gross_manureN[, 'Swine'] * pigs_prop
  
  return(gross_manureN)
}


compute_animal_manure_FRV <- function(year, animal_division, manure_type, subset) {
  # this function gets the output of compute_animal_gross_manureN and converts it into its fertilizer replacement value
  # this value is the one that must be distributed based on the crop assumptions
  # e.g., compute_animal_manure_FRV(2009, 'ruminants', 'solid)
  # unit: kg N yr-1
  
  gross_manureN <- compute_animal_gross_manureN(year, animal_division, manure_type)
  manure_frv <- gross_manureN
  frv <- get_FRV(organic_fertilizer_type = manure_type)
  
  calc_cols <- seq(4, ncol(gross_manureN))
  
  manure_frv[, calc_cols] <- sapply(gross_manureN[, calc_cols], function(x) x * frv)
  if (missing(subset)==FALSE) {
    manure_frv <- manure_frv[, c(1,2,3, ncol(manure_frv))]
  }
  return(manure_frv)
  rm(list = 'gross_manureN')
}


compute_manure_FRV_crop_dist <- function(year, animal_division, manure_type) {
  # computes crop manure N_frv for the specified manure type and based on the manure distribution assumptions according to animal division
  # unit: kg N yr-1
  
  # select main crops according to the animal division into ruminants and non-ruminants
    ifelse(animal_division=='ruminants',
           main_crops <- get_fodder_main.crops(),
           main_crops <- get_non.fodder_main.crops())
  
  manure_frv <- compute_animal_manure_FRV(year, animal_division, manure_type, TRUE)
  
  for (i in main_crops) {
    crops <- get_fertilization_crops(main_crop = i)
    for (j in crops) {
      crop_weight <- select_crop_AHP_weights(j)
      crop_manure_frv <- manure_frv
            # calculate how much manure N_FRV a crop receives
      crop_manure_frv[, 4] <- round(manure_frv[, 4] * crop_weight, 0)
      write_annual_data(module_name = 'Fertilization_module',
                        subfolder_name = 'Manure_crop_distribution',
                        file = manure_frv,
                        year = year,
                        filename = paste0(j, '_', manure_type),
                        subfolder_nameX2 = i)
    }
  }
}


manure_crop_distribution_mechanism <- function(year) {
  # this function acts as a mechanism to distribute manure_FRV N to all the crops
  # based on the animal division and manure type
  # unit: kg N yr-1
  
  animal_division <- c('ruminants', 'non_ruminants')
  manure_type <- c('solid', 'slurry')
  for (i in animal_division) {
    for (j in manure_type) {
      compute_manure_FRV_crop_dist(year = year, animal_division = i, manure_type = j)
    }
  }
}


compute_sum_manure_crop_distribution <- function(year) {
  # sums the crop manure N_FRV for both slurry and solid manure
  # unit: kg N yr-1
  
  main_crops <- append(get_fodder_main.crops(), get_non.fodder_main.crops())
  
  for (i in main_crops) {
    crops <- get_fertilization_crops(main_crop = i)
    for (j in crops) {
      
      ## get slurry and solid crop manure N_FRV
      crop_solid_FRV <- get_module_subfolder_output(module = 'Fertilization_module', 
                                                    submodule = 'Manure_crop_distribution', 
                                                    file_pattern = paste0(j, '_solid'),
                                                    submoduleX2 = i,  
                                                    submoduleX3 = year)
      crop_slurry_FRV <- get_module_subfolder_output(module = 'Fertilization_module', 
                                                    submodule = 'Manure_crop_distribution', 
                                                    file_pattern = paste0(j, '_slurry'),
                                                    submoduleX2 = i,  
                                                    submoduleX3 = year)
      crop_tot_FRV <- crop_slurry_FRV
      crop_tot_FRV[, 4] <- crop_slurry_FRV[, 4] + crop_solid_FRV[, 4]
      write_annual_data(module_name = 'Fertilization_module',
                        subfolder_name = 'Manure_crop_distribution',
                        file = crop_tot_FRV,
                        year = year,
                        filename = paste0(j, '_total'),
                        subfolder_nameX2 = i)
    }
  }
  rm(list=c('crop_solid_FRV', 'crop_slurry_FRV', 'crop_tot_FRV'))
}


