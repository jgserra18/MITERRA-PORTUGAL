source('./Irrigation_module/Functions/Global_irrigation_functions.R')

#loads the template irrigation df
get_irrig_template <- function() {
  read_template <- read_folder_files("Template", "Template")
  return(read_template)
}

get_irrig_total_areas <- function(year) {
  read_tot_areas <- read_folder_files("Total_irrigated_areas", as.character(year))
  return(read_tot_areas)
}

get_irrig_1999 <- function() {
  #gets irrigated areas for 1999 @ municipality

  path <- get_folder_files('Irrigated_areas')
  file_path <- disag_folders_year(path, 1999)
  file_path <- substr(file_path, 1, nchar(file_path)-1) #correct the last "/"
  
  read_file <- read.csv(file_path)
}


modify_get_crop_water_volume <- function(year, a, b) {
  #modifies get_crop_water_volume function to similar parameters of get_irrig_area
  #this is ought to be used in the general function to apply these subfunctions

  df <- get_crop_water_volume(a, b, year)
  return(df)
}

compute_irrig_sys_volumes <- function(year, write, function_x, output_name) {
  #NOTE: function_x can be a general function, such as modify_get_crop_water_volume OR get_irrig_areas

  main_crops <- get_maincrops_names(year)
  df <- get_irrig_template()
  
  for (a in main_crops) {
    crop_names <- get_crop_names(year, a)
    
    for (b in crop_names) {
      b <- as.character(b)
      crop_files <- function_x(year, a, b)
      
      for (i in 4:ncol(crop_files))
      {
        df[, i] <- df[, i] + crop_files[, i]
      }
    }
  }
  
  if (write==TRUE) {
    print('Writing ...')
    write_irrig_output(paste0('Irrigation_systems_general/', year), df, output_name)
    print('Finished!')
  }
  return(df)
}

correct_irrig_vol_efficiency <- function(year, write, output_name) {
  #calculate corrected water volumes with efficiencies

  irrig_vols <- compute_irrig_sys_volumes(year, FALSE, modify_get_crop_water_volume, '')
  eff <- get_irrig_sys_efficiency()
  cols_name <- colnames(irrig_vols)[4:ncol(irrig_vols)]
  
  for (i in cols_name) {
    id <- which(eff[, 1]==i)
    eff_df <- eff[id, 2]
    irrig_vols[, i] <- irrig_vols[, i]*eff_df
  }
  
  if (write==TRUE) {
    print('Writing ...')
    write_irrig_output(paste0('Irrigation_systems_general/', year), irrig_vols, output_name)
    print('Finished!')
  }
  return(irrig_vols)
}

compute_irrig_syst_proportion <- function(year, write) {
  #computes the irrigation systems proportions 

  sum_irrig_areas <- compute_irrig_sys_volumes(2009, FALSE, get_irrig_areas) #irrigated areas per irrig system of 2009
  df <- sum_irrig_areas
  df$SUM <- rowSums(df[, 4:ncol(df)])

  for (i in 4:ncol(df)) {
    df[, i] <- round(df[, i]/df$SUM, 3)
  }
  
  if (write==TRUE) {
    print('Writing ...')
    write_irrig_output(paste0('Irrigation_systems_general/', year), df, 'irrig_syst_proportions')
    print('Finished!')
  }
  return(df)
}


compute_2009_m3_per_ha <- function(year, write, output_name, with_efficiency) {
  #computes the water volume per ha for each irrigation system for the year 2009
#this will be used to extrapolate data for 1999
#THIS HAS ANOTHER OPTIONAL SUBMODULE TO ACCOUNT FOR IRRIGATION EFFICIENCY OR NOT
  area_df <- compute_irrig_sys_volumes(year, FALSE, get_irrig_areas, 'irrig_areas')
  
  ifelse(with_efficiency==TRUE,
        vol_df <- correct_irrig_vol_efficiency(2009, FALSE),
        vol_df <- compute_irrig_sys_volumes(year, FALSE, modify_get_crop_water_volume, 'irrig_volumes'))
  
  for (i in 4:ncol(area_df)) {
    vol_df[, i] <- vol_df[, i]/area_df[, i]
  }
  
  vol_df <- data_cleaning(vol_df)
  
  if (write==TRUE) {
    print('Writing ...')
    write_irrig_output(paste0('Irrigation_systems_general/', year), vol_df, output_name)
    print('Finished!')
  }
  return(vol_df)
}

switch_drip_to_sprinkler <- function(df_irrig_areas99) {
  #prof rosario assumption

  df <- df_irrig_areas99
  df[, 'aspersion'] <- df[, 'aspersion'] + df[, 'drip']
  df[, 'drip'] <- 0
  
  return(df)
}

compute_1999_general <- function(year, write, output_name, function_x) {
  ##general function to calculate data for 1999

  irrig_areas99 <- get_irrig_1999()
  df <- function_x(2009, FALSE) #any specified df from function_x
  
  for (i in 4:ncol(df))
  {
    df[, i] <- df[, i]*irrig_areas99[, 4]
  }
  
  df <- switch_drip_to_sprinkler(df) #switch drip to sprinkler and set drip to 0
  
  if (write==TRUE)
  {
    print('Writing ...')
    write_irrig_output(paste0('Irrigation_systems_general/', year), df, output_name)
    print('Finished!')
  }
  return(df)
}

compute_1999_irrig_sys_vol <- function(year, write, output_name, with_efficiency) {

  irrig_areas99 <- compute_1999_general(year, FALSE, '', compute_irrig_syst_proportion) #irrig sys areas
  df_vol_ha <- compute_2009_m3_per_ha(2009, FALSE, '', with_efficiency)
  
  for (i in 4:ncol(df_vol_ha)) {
    irrig_areas99[, i] <- df_vol_ha[, i]*irrig_areas99[, i]
  }
  irrig_areas99$SUM <- rowSums(irrig_areas99[, 4:ncol(irrig_areas99)])
  irrig_areas99 <- data_cleaning(irrig_areas99) #clean the data
  
  if (write==TRUE) {
    print('Writing ...')
    write_irrig_output(paste0('Irrigation_systems_general/', year), irrig_areas99, output_name)
    print('Finished!')
  }
  return(irrig_areas99)
}
