source('./Main_functions.R')
source('./CropProduction_module/Functions/crop_Nremoval_functions.R')

# BNF module based on the methodology presented 
# in Future Legumes Report by Baddeley et al (2014)


## ----------------------- GETTERS ----------------------- ##
## --------------------------------------------------------##

get_BNF_params <- function(legume_category) {
  # gets the necessary params to compute BNF for legumes
  # legume category is either Forage or Grain
  
  atmN_file <- get_activity_data(subfolder = 'BNF', 
                                 subfolderX2 = legume_category, 
                                 file_pattern = legume_category)
  return(atmN_file)
}

select_pulse_BNF_param <- function(param, crop) {
  
  pulse_param <- get_BNF_params('Grain')
  param_id <- which(grepl(param, pulse_param[, 1])==TRUE)
  pulse_param <- pulse_param[param_id, as.character(crop)]
  return(pulse_param)
}



## ----------------------- FORAGE LEGUMES BNF CALCULATION ----------------------- ##
## -------------------------------------------------------------------------------##
compute_forage_BNF <- function(year) {
  # Forage N fixation = Area * N_retention_kgNha-1 * N_fixation_coeff ,, 
  # where N_fixed_kgNha-1 == N_retention_kgNha-1 * N_fixation_coeff 
  # unit: kg N yr-1
  
  main_crop <- 'pastures'
  crop <- get_all_crop_names(main.crop = main_crop)
  forage_params <- get_BNF_params('Forage')
  
  for (i in crop) {
    crop_area <- get_crop_areas(year = year, main_crop = main_crop, crop = i)
    Nfixed_kgNha <- forage_params[which(forage_params[, 1]=='N_fixed_kgNha-1'), i]
    crop_area[, as.character(year)] <- crop_area[, as.character(year)] * Nfixed_kgNha
    write_annual_data(module_name = 'BNF_module',
                           subfolder_name = 'Forage_BNF', 
                           file = crop_area, 
                           year = year,
                           filename = paste0(i, '_kgN'))
  }
}


compute_total_forage_Nfixation <- function(year) {
  # computes the total N fixed by legumes in intensive and extensive pastures
  # unit: kg N yr-1
  
  forage_legumes <- c('intensive_pasture', 'extensive_pasture')
  calc_df <- create_main_csv()
  
  for (i in forage_legumes) {
    forage_BNF <- get_module_subfolder_output(module = 'BNF', submodule = 'Forage', file_pattern = i, submoduleX2 = year)
    calc_df[, i] <- forage_BNF[, as.character(year)]
  }

  calc_df[, 'forage_Nfixed'] <- calc_df[, forage_legumes[1]] + calc_df[, forage_legumes[2]]
  calc_df <- calc_df[, c(1, 2,3, 6)]
  colnames(calc_df)[4] <- as.character(year)
  
  write_annual_data(module_name = 'BNF_module', 
                    subfolder_name = 'Total_BNF',
                    year = year, 
                    file =  calc_df,
                    filename = 'Forage_BNF')
  rm(list=c('forage_legumes', 'forage_BNF'))
}



## ----------------------- GRAIN LEGUMES BNF CALCULATION ----------------------- ##
## ------------------------------------------------------------------------------##


# 1 - calculate DM production (tonnes dm)
# 2 - Calculate aboveground biomass
# 3 - Calculate aboveground N content
# 4 - calculate root biomass and N content
# 5 - Calculate rhizodeposition
# sum steps 3, 4 and 5
# apply Ndfa


compute_pulses_DM_production <- function(year, crop) {
  # step 1: calculate crop_Nremoval_function to compute pulses' DM production
  # unit: kg dm yr-1
  
  pulse_DMprod <- compute_crop_DM_production(year = year, main_crop = 'pulses', crop = crop)
  return(pulse_DMprod)
}


compute_pulses_aboveground_production <- function(year, crop) {
  # step 2: calculate aboveground biomass
  # AB_biomass = DMproduction / Harvest Index
  # unit: kg dm yr-1
  
  pulse_DMprod <- compute_pulses_DM_production(year, crop)
  pulse_HI <- select_pulse_BNF_param(param = 'harvest_index', crop)
  pulse_DMprod[, as.character(year)] <-  round( pulse_DMprod[, as.character(year)] / pulse_HI , 2)
  return(pulse_DMprod)
}


compute_pulses_aboveground_N <- function(year, crop) {
  # step 3: calculate abouground N content
  # AB_N = ( AB_biomass * protein_frac / protein_toN ) / HI_N
  # unit: kg N yr-1
  
  pulses_ABprod <- compute_pulses_aboveground_production(year, crop)
  
  # params
  pulse_protein_frac <- select_pulse_BNF_param(param = 'protein_content', crop)
  pulse_proteiN_conversion <- select_pulse_BNF_param(param = 'protein_to_N', crop)
  pulse_HI_N <- select_pulse_BNF_param(param = 'harvest_N_index', crop)
  
  # computation
  pulses_ABprod[, as.character(year)] <- round( 
    (pulses_ABprod[, as.character(year)] * pulse_protein_frac / pulse_proteiN_conversion) / pulse_HI_N  , 2)
  return(pulses_ABprod)
}


compute_pulses_belowground_production <- function(year, crop) {
  # step 4: calculate belowground biomass 
  # BG prod = AB prod * BG/AB ratio
  # unit: kg dm yr-1
  
  pulses_ABprod <- compute_pulses_aboveground_production(year, crop)
  pulse_BG_frac <- select_pulse_BNF_param(param = 'root_shoot', crop)
  pulses_ABprod[, as.character(year)] <- round( pulses_ABprod[, as.character(year)] * pulse_BG_frac, 2)
  return(pulses_ABprod)
}

compute_pulses_belowground_N <- function(year, crop) {
  # step 5: calculate belowground N 
  # BG N = BG prod * BG_Ncontent
  # unit: kg N yr-1
  
  pulses_BGprod <- compute_pulses_belowground_production(year, crop)
  pulse_BG_N <- select_pulse_BNF_param(param = 'root_N_content', crop)
  pulses_BGprod[, as.character(year)] <- round( pulses_BGprod[, as.character(year)] * pulse_BG_N, 2)
  return(pulses_BGprod)
}

compute_pulses_rhizodepositio_N <- function(year, crop) {
  # step 6: calculate rhizodeposition 
  # Rhizo_N = (AB_N + BG_N) + rhizo_frac
  # unit: kg N yr-1
  
  pulses_AB_N <- compute_pulses_aboveground_N(year, crop)
  pulses_BG_N <- compute_pulses_belowground_N(year, crop)
  pulses_rhizo_frac <- select_pulse_BNF_param(param = 'rhizodeposition', crop)
  
  # total N biomass
  pulses_AB_N[, as.character(year)] <- round(  pulses_AB_N[, as.character(year)] +  pulses_BG_N[, as.character(year)], 0)
  
  # calculate rhizodeposition 
  pulses_AB_N[, as.character(year)] <- round( pulses_AB_N[, as.character(year)] * pulses_rhizo_frac, 0)
  return(pulses_AB_N)
  rm(list=c('pulses_BG_N', 'pulses_rhizo_frac'))
}


compute_pulses_total_Nprod <- function(year, crop, write) {
  # step 7: calculate total N from the pulse
  # total N = AB_N + BG_N + rhizo_N
  # unit: kg N yr-1
  
  pulses_rhizo_N <- compute_pulses_rhizodepositio_N(year, crop)
  pulses_AB_N <- compute_pulses_aboveground_N(year, crop)
  pulses_BG_N <- compute_pulses_belowground_N(year, crop)
  
  # compute total pulse N production
  pulses_AB_N[, as.character(year)] <- round(
    pulses_AB_N[, as.character(year)] + pulses_rhizo_N[, as.character(year)] + pulses_BG_N[, as.character(year)]
  )
  
  if (missing(write)==FALSE) {
    write_list <- list(pulses_rhizo_N, pulses_AB_N, pulses_BG_N)
    sub_name <- c('Rhizodeposition_N', 'Aboveground_N', 'Belowground_N')
    
    for (i in 1:3) {
      write_annual_data(module_name = 'BNF_module', 
                        subfolder_name = 'Grains_BNF',
                        year = year, 
                        file = write_list[[i]], 
                        filename = crop, 
                        subfolder_nameX2 = sub_name[i]) 
    }
  }
  else {
    return(pulses_AB_N)
  }
}


compute_pulses_Nfixation <- function(year, crop, write) {
  # step 8: calculate biological N fixation from legumes
  # BNF = total N * Ndfa
  # unit: kg N yr-1
  
  pulse_total_N <- compute_pulses_total_Nprod(year, crop)
  pulse_Nfda <- select_pulse_BNF_param(param = 'Ndfa', crop)
  
  pulse_total_N[, as.character(year)] <- round( pulse_total_N[, as.character(year)] * pulse_Nfda, 0 )
  
  if (missing(write)==FALSE) {
    write_annual_data(module_name = 'BNF_module', 
                      subfolder_name = 'Grains_BNF',
                      year = year, 
                      file =  pulse_total_N,
                      filename = crop, 
                      subfolder_nameX2 ='Biological N fixation')
  }
  else {
    return(pulse_total_N)
  }
}

compute_grain_Nfixation <- function(year) {
  # compute AB N, BG N, rhizo N, total N and N fixed by all pulse  crops
  # unit: kg N yr-1
  
  pulse_crops <- get_all_crop_names(main.crop = 'pulses')
  for (i in pulse_crops) {
    compute_pulses_total_Nprod(year = year, crop = i, write = TRUE)
    compute_pulses_Nfixation(year = year, crop = i, write = TRUE)
  }
}


compute_total_grain_Nfixation <- function(year) {
  # computes total grain legumes N fixation 
  # unit: kg N yr-1
  
  pulse_crops <- get_all_crop_names(main.crop = 'pulses')
  calc_df <- create_main_csv()
  
  for (i in pulse_crops) {
    grain_BNF <- get_module_subfolder_output(module = 'BNF', 
                                             submodule = 'Grains', 
                                             submoduleX2 = 'Biological N fixation', 
                                             file_pattern = i, 
                                             submoduleX3 = year)
    calc_df[, i] <- grain_BNF[, as.character(year)]
  }
  calc_df[, 'grains_Nfixed'] <- rowSums(calc_df[, seq(4, ncol(calc_df))])
  calc_df <- calc_df[, c(1, 2,3, ncol(calc_df))]
  colnames(calc_df)[4] <- as.character(year)
  
  write_annual_data(module_name = 'BNF_module', 
                    subfolder_name = 'Total_BNF',
                    year = year, 
                    file =  calc_df,
                    filename = 'Grains_BNF')
  rm(list=c('pulse_crops', 'grain_BNF', 'calc_df'))
}

## ----------------------- TOTAL BNF CALCULATION ----------------------- ##
## ----------------------------------------------------------------------##

compute_total_BNF <- function(year) {
  # compute total N fixation by forage and grain legumes
  # unit: kg N yr-1
  
  grains_BNF <- get_module_subfolder_output(module = 'BNF', 
                                            submodule = 'Total', 
                                            submoduleX2 = year, 
                                            file_pattern = 'Grains')
  forage_BNF <- get_module_subfolder_output(module = 'BNF', 
                                            submodule = 'Total', 
                                            submoduleX2 = year, 
                                            file_pattern = 'Forage')
  # compute total N fixed
  grains_BNF[, as.character(year)] <- round( grains_BNF[, as.character(year)] + forage_BNF[, as.character(year)], 2)
  write_annual_data(module_name = 'BNF_module', 
                    subfolder_name = 'Total_BNF',
                    year = year, 
                    file =  grains_BNF,
                    filename = 'Total_BNF')
}


## ----------------------- BNF CROP ACREAGE ----------------------- ##
## -----------------------------------------------------------------##

compute_acreage_Nfixation <- function(year) {
  # compute crop N fixed per crop acreage
  # unit: kg N (crop area)-1 yr-1
  
  main_crops <- c('pulses', 'pastures')
  for (i in main_crops) {
    pulse_crops <- get_all_crop_names(main.crop = i)
    
    for (j in pulse_crops) {
      crop_area <- get_crop_areas(year = year, main_crop = i, crop = j)
      # get leguminous N fixed -------------------------------------------------
      ifelse(i=='pulses',
             Nfixed <- compute_pulses_Nfixation(year = year, crop = j),
             Nfixed <- get_module_subfolder_output(module = 'BNF_module', 
                                                   submodule = 'Forage_BNF', 
                                                   submoduleX2 = year, 
                                                   file_pattern = j))
      # calculate N fixed per legume area ---------------------------------------
      Nfixed[, as.character(year)] <- round(
        Nfixed[, as.character(year)] / crop_area[, as.character(year)], 2)
      Nfixed <- data_cleaning(Nfixed)
      
      write_annual_data(module_name = 'BNF_module', 
                        subfolder_name = 'Crop_acreage_BNF',
                        year = year, 
                        file =  Nfixed,
                        filename = j, 
                        subfolder_nameX2 = i)
    }
  }
  unit_path <- './BNF_module/Output/Crop_acreage_BNF/'
  writeLines('...', paste0(file.path(unit_path, 'Unit_kgN_cropha'), '.txt'))
  rm(list=c('pulse_crops', 'crop_area', 'Nfixed'))
}


