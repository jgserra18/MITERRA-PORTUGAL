source('./Main_functions.R')
source('./CropProduction_module/Functions/crop_Nremoval_functions.R')


#// modificar parâmetros das pastagens (FRAC_renew e productividade)
#// modificar parâmetros das pastagens (FRAC_renew e productividade)
#// modificar parâmetros das pastagens (FRAC_renew e productividade)

## ----------------------- ReSIDUES DATA GETTERS ----------------------- ##
## ----------------------------------------------------------------------##

get_residues_params <- function(param) {
  # gets crop residues data for the specified param
  
  res_param <- get_activity_data(subfolder = 'Crop_production', 
                                  subfolderX2 = 'Residues_params', 
                                  file_pattern = param)
  return(res_param)
}


select_crop_res_param <- function(param, crop, param_col) {
  # gets crop residues params a given year and crop
  
  res_param <- get_residues_params(param)
  crop_idx <- which(res_param[, 2]==crop)
  col_idx <- which(names(res_param)==as.character(param_col))
  res_param <- res_param[crop_idx,col_idx]
  
  return(res_param)
  rm(list='crop_idx')
}


get_PC_tree_densities <- function(crop) {
  # get perma crops' tree densities and spatially "downscales" them to the municipality level based on agrarian region data
  # unit: NO_trees ha-1 yr-1
  
  tree_densities <- get_residues_params('PermaCrops_tree')
  crop_id <- which(names(tree_densities)==crop)
  crop_trees <- tree_densities[, c(1,2,crop_id)]
  crop_trees <- spatial_disagg_agrarian_muni(crop_trees)
  
  return(crop_trees)
  rm(list=c('tree_densities', 'crop_id'))
}


get_burnt_maincrops <- function() {
  
  main_crops <- c('cereals', 'citrus', 'dried_fruits', 'fresh_fruits', 'olive_grove', 'vineyard')
  return(main_crops)
}


## ----------------------- BURNT AREAS ----------------------- ##
## ------------------------------------------------------------##
# No need to write this output

compute_burning_areas <- function(year, main_crop, crop) {
  # computes  burning areas for a given crop
  #  Crop_A_burnt = A_crop * FRAC_burn * Cf
  # unit: ha yr-1
  
  crop_area <- get_crop_areas(year, main_crop, crop)
  burnt_frac <- select_crop_res_param('Residues_burnt', crop, year)
  cf <- select_crop_res_param('Residues_Cf', crop, 'Cf')
  
  burnt_area <- crop_area
  burnt_area[, as.character(year)] <- round ( 
    crop_area[, as.character(year)] * burnt_frac * cf, 2)
  burnt_area <- data_cleaning(burnt_area)
  
  return(burnt_area)
  rm(list=c('crop_area', 'burnt_frac', 'cf'))
}


compute_remain_areas <- function(year, main_crop, crop) {
  # computes remaining residue crop areas after burning practices for a given year and crop
  # Remaining A_crop = A_crop - (A_crop * FRAC_burn * Cf)
  # unit: ha yr-1
  
  crop_area <- get_crop_areas(year, main_crop, crop)
  burnt_area <- compute_burning_areas(year, main_crop, crop)
  
  crop_area[, as.character(year)] <- round ( 
    crop_area[, as.character(year)] - burnt_area[, as.character(year)], 2)

  return(crop_area)
  rm(list=c('crop_area', 'burnt_area'))
}




## -----------------------  DM yields ----------------------- ##
## ------------------------------------------------------------##

get_crop_DM_yields <- function(year, main_crop, crop) {
  # gets spatially disaggregated crop yields DM 
  # same is also applied for pastures
  # pasture yield are given in kg DM ha-1 yr-1 and were derived from Velthof et al (2009)
  # unit: kg DM ha-1 yr-1
  
  if (main_crop != 'pastures') {
    
    crop_yield <- get_spatially_disaggregated_cropYields(year, main_crop, crop)
    crop_DM <- select_crop_res_param('Residues_params', crop, 'DM_frac')
    crop_yield[, as.character(year)] <- round ( crop_yield[, as.character(year)] * crop_DM, 2 )
  }
  else {
    crop_yield <- create_main_csv()
    ifelse(crop=='intensive_pasture',
           crop_yield[, as.character(year)] <- 4000,
           crop_yield[, as.character(year)] <- 2000)
  }
  return(crop_yield)
  rm(list='crop_DM')
}



## ----------------------- ABOVEGROUND RESIDUE DM----------------------- ##
## --------------------------- TEMPORARY CROPS-------------------------- ##
## ----------------------------------------------------------------------##

compute_AG_biomass <- function(year, main_crop, crop) {
  # computes abovground residue biomass based on IPCC (2006) Table 11.2 parameters
  # AG_DM = ( Yield * DM / 1000 ) * slope + intercept
  # unit: kg DM ha-1 yr-1
  
  crop_yield <- get_crop_DM_yields(year, main_crop, crop)
  slope <- select_crop_res_param('Residues_params', crop, 'slope')
  intercept <- select_crop_res_param('Residues_params', crop, 'intercept')
  
  AB_biomass <- crop_yield
  
  AB_biomass[, as.character(year)] <- round ( ( crop_yield[, as.character(year)] / 1000 ) * slope + intercept , 2)
  
  return(AB_biomass)
  rm(list=c('crop_yield','slope', 'intercept'))
}


compute_ratio_R.AG_yield <- function(year, main_crop, crop) {
  # ratio of AB residues biomass to harvsted crop yield
  # R_AG = AG_DM * 1000 / (yield * DM)
  # unit: dimensionless
  
  crop_yield <- get_crop_DM_yields(year, main_crop, crop)
  
  AG_DM <- compute_AG_biomass(year, main_crop, crop)
  
  AG_DM[, as.character(year)] <- round ( 
    AG_DM[, as.character(year)] * 1000 / crop_yield[, as.character(year)] , 2)
  
  return(AG_DM)
  rm(list=c('crop_yield'))
}


## ----------------------- BELOWGROUND RESIDUE DM----------------------- ##
## ----------------------------------------------------------------------##

compute_ratio_total_AG_biomass_yield <- function(year, main_crop, crop) {
  # computes the ratio of total aboveground biomass to harvested crop yield
  # TAG_DM = ( AG_DM * 1000 + (Yield * DM) ) / (Yield * DM)
  # unit: dimensionless
  
  AG_DM <- compute_AG_biomass(year, main_crop, crop)
  crop_yield <- get_crop_DM_yields(year, main_crop, crop)
  
  AG_DM[, as.character(year)] <- round ( 
    (AG_DM[, as.character(year)] * 1000 + crop_yield[, as.character(year)]) / crop_yield[, as.character(year)], 2)
  
  return(AG_DM)
  rm(list=c('crop_yield'))
}


compute_ratio_R.BG_yield <- function(year, main_crop, crop) {
  # computes the ratio of belowground residues to harvested crop yield
  # R_BG = R_BG_BIO * TAG_SM
  # unit: dimensionless
  
  TAG_DM <- compute_ratio_total_AG_biomass_yield(year, main_crop, crop)
  R_BG_BIO <- select_crop_res_param('Residues_params', crop, 'R_BG_BIO')
  
  TAG_DM[, as.character(year)] <- round ( TAG_DM[, as.character(year)] * R_BG_BIO, 2)
  
  return(TAG_DM)
  rm(list='R_BG_BIO')
}

## ----------------------- N in crop residues----------------------- ##
## ------------------------------------------------------------------##

# Total N in crop residues, irrespective of whether it is left, removed or burnt 
# is divided into different two different sections (IPCC 2006 EQ 11.6)
# One section regards the aboveground biomass, the other the belowground biomass

compute_BG_residues_N_section <- function(year, main_crop, crop) {
  # computes the belowground section of IPCC (2006) 11.6
  # unit: kg N ha kg dm-1 yr-1
  
  R_BG <- compute_ratio_R.BG_yield(year, main_crop, crop)
  crop_area <- get_crop_areas(year, main_crop, crop)
  N_BG <- select_crop_res_param('Residues_params', crop, 'N_BG')
  
  BG_section <- crop_area
  BG_section[, as.character(year)] <- round( R_BG[, as.character(year)] * N_BG * crop_area[, as.character(year)] , 2)
  
  return(BG_section)
  rm(list=c('R_BG', 'crop_area', 'N_BG'))
}


select_residue_management_fracs <- function(crop, residue_practice) {
  # selects for a given crop, the FRAC of the residue left or removed from the field
  # residue_practice is either "removed" or "left"
  # unit: % of crop residue N
  
  residue_FRAC <- select_crop_res_param('Residues_params', crop, 'FRAC_removed')
  ifelse(residue_practice=='removed',
         residue_FRAC <- residue_FRAC,
         residue_FRAC <- 1 - residue_FRAC)
  return(residue_FRAC)
}

compute_AG_residues_N_section <- function(year, main_crop, crop, residue_practice) {
  # computes the aboveground section of IPCC (2006) 11.6
  # already includs
  # unit: kg N ha kg dm-1 yr-1
  
  R_AG <- compute_ratio_R.AG_yield(year, main_crop, crop)
  residue_area <- compute_remain_areas(year, main_crop, crop)
  N_AG <- select_crop_res_param('Residues_params', crop, 'N_AG')
  residue_frac <- select_residue_management_fracs(crop, residue_practice)
  
  AG_section <- residue_area
  AG_section[, as.character(year)] <- round( residue_frac * N_AG * R_AG[, as.character(year)] * residue_area[, as.character(year)] , 2)
  
  return(AG_section)
  rm(list=c('R_AG', 'residue_area', 'N_AG'))
}



general_funct_crop_residues_N <- function(year, main_crop, crop, residue_practice) {
  # general function that is able to compute crop residues N, either left or removed from the field
  # general function to be later implemented according to residue practice
  # unit: kg N yr-1

  # load params ------------------------------------------------------------------
  crop_yield <- get_crop_DM_yields(year, main_crop, crop)
  FRAC_renew <- select_crop_res_param('Residues_FRACrenew', crop, 'FRAC_renew')
  AG_section <- compute_AG_residues_N_section(year, main_crop, crop, residue_practice)
  BG_section <- compute_BG_residues_N_section(year, main_crop, crop)
  
  # calculations -----------------------------------------------------------------
  calc_col <- as.character(year)
  residues_N <- crop_yield
  
  residues_N[, calc_col] <- round(
    crop_yield[, calc_col] * FRAC_renew * ( AG_section[, calc_col] + BG_section[, calc_col] ), 2)
  residues_N <- data_cleaning(residues_N)
  write_annual_data(module_name = 'CropProduction_module', 
                     subfolder_name = paste0('Residues_', residue_practice), 
                     file = residues_N, 
                     filename = crop, 
                     year = year, 
                     subfolder_nameX2 = main_crop)
  rm(list=c('crop_yield', 'FRAC_renew', 'AG_section', 'BG_section'))
}


compute_crop_residues_leftN <- function(year, main_crop, crop) {
  
  general_funct_crop_residues_N(year, main_crop, crop, 'left')
}

compute_crop_residues_removedN <- function(year, main_crop, crop) {
  
  general_funct_crop_residues_N(year, main_crop, crop, 'removed')
}


compute_crop_residuesN <- function(year) {
  # crop residues N left and removed from the field computation function 
  # unit: kg N yr-1
  
  main_crops <- c('cereals', 'industry', 'potato', 'pulses', 'pastures')
  for (i in main_crops) {
    crops <- get_all_crop_names(main.crop = i)
    for (j in crops) {
      print(paste0('Computing crop residues N for ', j))
      compute_crop_residues_leftN(year = year,main_crop = i, crop = j)
      compute_crop_residues_removedN(year = year,main_crop = i, crop = j)
    }
  }
}


compute_totals_residues_N <- function(year, residue_practice) {
  # calculate the totals for each main crop and municipality for a given year
  # unit: kg N yr-1
  
  calc_df <- create_main_csv()
  main_crops <- c('cereals', 'industry', 'potato', 'pulses', 'pastures')
  
  for (i in main_crops) {
    
    main_crop_df <- create_main_csv()
    crops <- get_all_crop_names(main.crop = i) 
    
    for (j in crops) {
      res_N <- get_module_subfolder_output(module = 'CropProduction', 
                                           submodule = paste0('Residues_', residue_practice), 
                                           submoduleX2 = i, 
                                           file_pattern = j, 
                                           submoduleX3 = year)
      main_crop_df[, j] <- res_N[, as.character(year)]
    }
    # sum the total of crops for each main crop -------------------------------------------------
    ifelse(ncol(main_crop_df)>4,
           main_crop_df[, 'total'] <- rowSums(main_crop_df[, seq(4, ncol(main_crop_df))]),
           main_crop_df[, 'total'] <- main_crop_df[, 4])
    write_annual_data(module_name = 'CropProduction_module', 
                      subfolder_name = paste0('Residues_', residue_practice), 
                      file = main_crop_df, 
                      filename = paste0(i, '_total'), 
                      year = year, 
                      subfolder_nameX2 = 'MainCrop_totals')
    
    calc_df[, i] <- main_crop_df[, 'total']
  }
  # sum the total of maincrops -------------------------------------------------
  calc_df[, 'total_municipality'] <- rowSums(calc_df[, seq(4, ncol(calc_df))])
  write_annual_data(module_name = 'CropProduction_module', 
                    subfolder_name = paste0('Residues_', residue_practice), 
                    file = calc_df, 
                    filename = 'Total_municipality', 
                    year = year, 
                    subfolder_nameX2 = 'MainCrop_totals')
  rm(list=c('calc_df', 'main_crop_df', 'res_N', 'crops', 'main_crops'))
}


## ----------------------- CROP RESIDUES BURNT IN SITU----------------------- ##
## ---------------------------------------------------------------------------##

## PERMANENT CROPS ------------------------------------------------------------------------------

compute_PC_trees <- function(year, main_crop, crop) {
  # calculates the no of trees of a permanent crop at the municipality level
  # EXCEPTION ----> VINEYARDS !!!!!!!!!!!!!!!
  # NO_trees = Burnt_area * Tree_density
  # unit: No_trees yr-1
  
  crop_area <- compute_burning_areas(year, main_crop, crop)
  tree_densities <- get_PC_tree_densities(crop)
  
  NO_trees <- crop_area
  NO_trees[, as.character(year)] <- round( 
    NO_trees[, as.character(year)] * tree_densities[, crop] , 2)
  
  return(NO_trees)
  rm(list=c('crop_area', 'tree_densities'))
}


compute_PC_burnt_residues_N <- function(year, main_crop, crop) {
  # calculates N in crop residues burnt of PERMANENT CROPS, EXCEPT VINEYARDS
  # BURNT_RES_N = NO_TREES * RES_TREE * RES_N
  # Unit: kg N yr-1
  
  pc_trees <- compute_PC_trees(year, main_crop, crop)
  tree_residues <- select_crop_res_param('PermaCrops_params', crop, 'Res_trees') #kg residue / tree / yr
  N_residues <- select_crop_res_param('PermaCrops_params', crop, 'N_res') #kg N / tonnes residues / yr
  
  burnt_resN <- pc_trees
  burnt_resN[, as.character(year)] <- round( 
    (burnt_resN[, as.character(year)] * tree_residues) / 1000 * N_residues , 2)
  burnt_resN <- data_cleaning(burnt_resN)
  write_annual_data(module_name = 'CropProduction_module', 
                    subfolder_name = 'Residues_burnt', 
                    file = burnt_resN, 
                    filename = crop, 
                    year = year, 
                    subfolder_nameX2 = main_crop)
  rm(list=c('pc_trees', 'tree_residues', 'N_residues', 'burnt_resN'))
}

## vineyard --------------------------------------------------------------------------------------------

compute_vineyard_burnt_residues_N <- function(year) {
  # computes total N in vineyard residues burnt in situ
  # unit: kg N yr-1
  
  vine_area <- compute_burning_areas(year, 'vineyard', 'vineyard')
  vine_res <- 1.4 # tonnes residues ha-1 yr-1
  vine_Nres <- select_crop_res_param('PermaCrops_params', 'vineyard', 'N_res')
  
  burnt_vineN <-vine_area
  burnt_vineN[, as.character(year)] <- round( burnt_vineN[, as.character(year)] * vine_res * vine_Nres , 2)
  burnt_vineN <- data_cleaning(burnt_vineN)
  
  write_annual_data(module_name = 'CropProduction_module', 
                    subfolder_name = 'Residues_burnt', 
                    file = burnt_vineN, 
                    filename = 'vineyard', 
                    year = year, 
                    subfolder_nameX2 = 'vineyard')
  rm(list=c('vine_area', 'vine_res', 'vine_Nres', 'burnt_vineN'))
}

## cereals --------------------------------------------------------------------------------------------

compute_cereals_burnt_residues_N <- function(year, crop) {
  # computes N in cereal burnt residues
  # unit: kg N yr-1
  
  crop_yield <- get_crop_DM_yields(year, 'cereals', crop)
  crop_area <- compute_burning_areas(year, 'cereals', crop)
  R_AG <- compute_ratio_R.AG_yield(year, 'cereals', crop)
  N_AG <- select_crop_res_param('Residues_params', crop, 'N_AG')
  
  burnt_resN <- crop_area
  burnt_resN[, as.character(year)] <- round( 
    crop_area[, as.character(year)] * R_AG[, as.character(year)] * N_AG * crop_yield[, as.character(year)] , 2)
  burnt_resN <- data_cleaning(burnt_resN)
  
  write_annual_data(module_name = 'CropProduction_module', 
                    subfolder_name = 'Residues_burnt', 
                    file = burnt_resN, 
                    filename = crop, 
                    year = year, 
                    subfolder_nameX2 = 'cereals')
  rm(list=c('crop_area', 'R_AG', 'N_AG', 'burnt_resN'))
}


## TOTAL --------------------------------------------------------------------------------------------

compute_crop_residues_burnt_N <- function(year) {
  # computes N in crop residues burnt for all specified main crops
  # unit: kg N yr-1
  
  main_crops <- get_burnt_maincrops()
  for (i in main_crops) {
    crops <- get_all_crop_names(main.crop = i)
    for (j in crops) {
      if (i=='cereals') {
        compute_cereals_burnt_residues_N(year, crop = j)
      }
      else if (i=='vineyard') {
        compute_vineyard_burnt_residues_N(year)
      }
      else {
        compute_PC_burnt_residues_N(year = year, main_crop = i, crop = j)
      }
    }
  }
}


compute_totals_residue_burnt_N <- function(year) {
  # calculate the totals for each main crop and municipality for a given year
  # unit: kg N yr-1
  
  calc_df <- create_main_csv()
  main_crops <- get_burnt_maincrops()
  
  for (i in main_crops) {
    
    main_crop_df <- create_main_csv()
    crops <- get_all_crop_names(main.crop = i) 
    
    for (j in crops) {
      res_burtN <- get_module_subfolder_output(module = 'CropProduction', 
                                               submodule = 'Residues_burnt', 
                                               submoduleX2 = i, 
                                               file_pattern = j, 
                                               submoduleX3 = year)
      main_crop_df[, j] <- res_burtN[, as.character(year)]
    }
    # sum the total of crops for each main crop -------------------------------------------------
    ifelse(ncol(main_crop_df)>4,
           main_crop_df[, 'total'] <- rowSums(main_crop_df[, seq(4, ncol(main_crop_df))]),
           main_crop_df[, 'total'] <- main_crop_df[, 4])
    write_annual_data(module_name = 'CropProduction_module', 
                      subfolder_name = 'Residues_burnt', 
                      file = main_crop_df, 
                      filename = paste0(i, '_total'), 
                      year = year, 
                      subfolder_nameX2 = 'MainCrop_totals')
    
    calc_df[, i] <- main_crop_df[, 'total']
  }
  # sum the total of maincrops -------------------------------------------------
  calc_df[, 'total_municipality'] <- rowSums(calc_df[, seq(4, ncol(calc_df))])
  write_annual_data(module_name = 'CropProduction_module', 
                    subfolder_name = 'Residues_burnt', 
                    file = calc_df, 
                    filename = 'Total_municipality', 
                    year = year, 
                    subfolder_nameX2 = 'MainCrop_totals')
  rm(list=c('calc_df', 'main_crop_df', 'res_burtN', 'crops', 'main_crops'))
}


