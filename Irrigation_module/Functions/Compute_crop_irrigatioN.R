source('./Irrigation_module/Functions/Compute_1999_areas.R')
source('./Irrigation_module/Functions/no3_functions.R')

#get water volume per crop for each year
#get water source and NO3
#get irrigated areas per crop (irrespective of irrig system)
#calculate irrig N

#aggregate crops as follows: winter cereals (non-irrigated land), fruit trees (fresh, dry, citrus, olive, vine), grassland
#irrigated crops




## ----------------------- GENERAL GETTERS -----------------------##
## ---------------------------------------------------------------##

get_correct_water_source <- function(year) {
  #compute kg N per each water source
  #water_source_weight * water_volume_irrig_sys * NO3_water_source
  
  ifelse(missing(year)==TRUE,
         water_source <- compute_corrected_water_sources(2009, FALSE),
         water_source <- compute_corrected_water_sources(year, FALSE))
  
  return(water_source)
}

get_irrig_sys_volumes <- function(year, efficiency) {
  #gets the irrigation system volumes for any specified year and with or without irrigation efficiencies
  
  if (year==1999) {
    irrig_sys_vol <- compute_1999_irrig_sys_vol(year, FALSE, 'irrig_volumes_wo_efficiency', efficiency) #FALSE=IGNORES IRRIGATION EFFICIENCIES
  }
  else  {
    ifelse(efficiency==TRUE,
           irrig_sys_vol <- correct_irrig_vol_efficiency(year, FALSE, 'irrig_volumes_efficiency'),
           irrig_sys_vol <- compute_irrig_sys_volumes(year, FALSE, modify_get_crop_water_volume, 'irrig_volumes'))
  }
  return(irrig_sys_vol)
}


specify_water_source <- function(water_source) {
  #gets specified water_source column
  
  water_source_df <- get_correct_water_source()
  condition <- which(grepl(water_source, colnames(water_source_df))==TRUE)
  
  select_source <- water_source_df[, condition]
  names(select_source) <- water_source
  
  return(select_source)
}

correct_no3_ND <- function(df) {
  #corrects the NDs to 0
  
  df <- gsub('ND', 0, df)
  df <- as.numeric(df)
  
  return(df)
}

specify_no3_source <- function(year, water_source) {
  #specifies no3 source 
  
  no3_df <- aggregate_no3_source(year)
  cols <- colnames(no3_df)
  condition <- which(grepl(water_source, cols)==TRUE)
  select_source <- no3_df[,condition]
  select_source <- correct_no3_ND(select_source)
  
  return(select_source)
}
#//wrong conversion factor
conversion_no3 <- function(year, water_source) {
  #converts data in mg NO3/L to kg N-NO3/m3
  
  no3_df <- specify_no3_source(year, water_source)
  no3_df <- as.numeric(no3_df)
  n_no3_df <- (no3_df*0.2259*0.001*0.001)/0.001 #conversion to N-NO3, conversion to m3, conversion to kg 
  return(n_no3_df)
}



## ----------------------- IRRIG CROP 1999 EXTRAPOLATION -----------------------##
## -----------------------------------------------------------------------------##

compute_tot_irrig_areas09 <- function() {
  #computes total crop area (sum of irrig system areas) and compiles these into one dataset
  
  main_crops <- get_maincrops_names(2009)
  df <- create_main_csv() #create main df to populate crop water volume usage per irrigation system specified
  create_irrig_output_dir('irrigated_crop_areas', TRUE)
  
  for (a in main_crops) {
    crops <- get_crop_names(2009, a)
    print(paste0('Working on', a))
    for (b in crops) {
      print(b)
      crop_area <- get_irrig_areas(2009, a, b)
      crop_area$sum <- rowSums(crop_area[, seq(4, ncol(crop_area))])
      df[, b] <- crop_area$sum
    }
  }
  df$irrig_areas09 <- rowSums(df[, seq(4, ncol(df))])
  write_irrig_output('irrigated_crop_areas', df, 'tot_irrig_crop09', 2009)
  return(df)
}

compute_prop_crop09 <- function() {
  #computes area proportion for each area
  
  irrig_dataset09 <- get_output_file('irrigated_crop_areas', 2009, 'tot_irrig_crop')
  sum <- ncol(irrig_dataset09)
  crop_cols <- seq(4, sum)
  
  irrig_dataset09[, crop_cols] <- sapply(crop_cols, function(x) irrig_dataset09[, x] <- irrig_dataset09[, x]/irrig_dataset09[, sum])
  return(irrig_dataset09)  
}

extrapolate_1999_crop_areas <- function() {
  #calculate 1999 crop areas at the municipality level based on 2009 proportions
  
  irrig_tot99 <- get_irrig_1999()
  prop_crop09 <- compute_prop_crop09()
  
  for (i in 4:(ncol(prop_crop09)-1)) {
    prop_crop09[, i] <- round(prop_crop09[, i]*irrig_tot99[, 4], 3)       
  }
  return(prop_crop09)
}

func_compute_area_prop <- function(df) {
  df$sum <- rowSums(df[, seq(4, ncol(df))])
  nc <- ncol(df)
  
  for (i in 4:nc) {
    df[, i] <- df[, i]/df[, nc]
  }
  
  return(df)
}

create_irrig_crop_dir99 <- function(main_crop, return_path, name) {

  irrig_output_path <- select_module_output('Irrigation_module')
  dir_path <- file.path(irrig_output_path, 'irrigated_crop_areas')
  ifelse(missing(name)==TRUE, name <- '1999', name <- name)
  yr_path <- file.path(dir_path, name)
  crop <- file.path(yr_path, main_crop)
  dir.create(crop)
  #return pathfile
  if(return_path==TRUE) {
    return(crop)
  }
}


populate_output_irrig_areas99 <- function() {
  #compute irrigation areas for crop and irrigation system based on 2009 data
  
  irrig_a99 <- extrapolate_1999_crop_areas()
  colnames(irrig_a99)[which(colnames(irrig_a99)=='olive.grove')] <- 'olive grove'
  main_crops <- get_maincrops_names(2009)
  
  for (a in main_crops)
  {
    crops <- get_crop_names(2009, a)
    path <- create_irrig_crop_dir99(a, T)
    
    for (b in crops)
    {
      crop_area <- get_irrig_areas(2009, a, b)
      crop_area_prop <- func_compute_area_prop(crop_area)
      crop_df <- as.data.frame(irrig_a99[, b])
      #calculate proportion of irrig systems for 1999
      for (i in 4:ncol(crop_area_prop)) {
        crop_area_prop[, i] <- round(crop_area_prop[,i]*crop_df, 0)
        crop_area_prop <- data_cleaning(crop_area_prop)
        crop_area_prop <- switch_drip_to_sprinkler(crop_area_prop) #prof rosario assumption
      }
  
      write.csv(crop_area_prop, file.path(path, paste0(b, '.csv')))
    }
  }
}


correct_output_irrig_areas99 <- function()
{
  #correct_output_irrig_areas99()
  #correct irrigated areas 1999 based on total 1999 areas per crop
  #note: if anything wrong arises, it is because get_irrig_areas are directed from uncorrected99 subfolder
  
  main_crops <- get_maincrops_names(2009)
  
  for (a in main_crops)
  {
    crops <- get_crop_names(2009, a)
    path <- create_irrig_crop_dir99(a, T, 'corrected99')
    
    for (b in crops)
    {
      tot_crop_a <- get_raw_crop_areas(1999, a, b)
      irrig_crop_a <-get_irrig_areas(1999, a, b, '1999')
      
      irrig_crop_a <- cbind(irrig_crop_a, tot_crop_a[, ncol(tot_crop_a)])
      colnames(irrig_crop_a)[ncol(irrig_crop_a)] <- paste0('tot_', b)
      col_name <- paste0('tot_', b)
      irrig_crop_a$diff <- irrig_crop_a[, col_name]-irrig_crop_a$sum
      
      corr_id <- which(irrig_crop_a$diff<0)
      
      for (i in 4:(ncol(irrig_crop_a)-2))
      {
        irrig_crop_a[corr_id, i] <- round(irrig_crop_a[corr_id, col_name]*irrig_crop_a[corr_id, i]/irrig_crop_a[corr_id, 'sum'], 0)
      }
      irrig_crop_a$sum <- rowSums(irrig_crop_a[, seq(4,9)])
      final_df <- irrig_crop_a[, seq(1,9)]
      write.csv(final_df, file.path(path, paste0(b, '.csv')))
    }

  }
}

## ----------------------- CROP IRRIG N -----------------------##
## ------------------------------------------------------------##


general_crop_irrigatioN_func <- function(year, individual_crop) {
  # computes irrigation N for each crop as the sum of each irrig system
  # this function is useful to allocate irrigation N for each LU classes
  # note: this is the ideal function to compute irrigationN for each source
  # INDIIVIDUAL_CROP can be either TRUE or FALSE
  # unit: kg N yr-1
  
  options(warn=-1)

  compute_corrected_water_sources(year, T) #for water source
  main_crops <- get_maincrops_names(2009)
  df <- create_main_csv() #create main df to populate crop water volume usage per irrigation system specified
  water_source <- c('spring', 'superficial', 'well')
  
  #create directory for total and individual crops
  ifelse(individual_crop==TRUE, 
         folder <- 'individual', 
         folder <- 'Total')

  for (a in main_crops) {

    crops <- get_crop_names(2009, a)
    main_crop_df <- df
    
    for (b in crops) {
      #get crop water volume
      print(paste0('Computing irrigation N for ', b))
      crop_volume <- get_crop_water_volume(a, b, year)
      crop_volume$sum <- rowSums(crop_volume[, seq(4, ncol(crop_volume))]) #compute total volume
      sel_cols <- seq(4, ncol(crop_volume)) # define range of cols in calculations
      
      #create empty dataframe
      crop_df <- crop_volume
      crop_df[, sel_cols] <- sapply(crop_df[, sel_cols], function(x) x <- 0)
      calc_df <- df
      
      for (c in water_source) {
        
        #get water source and NO3
        w_source <- specify_water_source(c)
        no3_source <- conversion_no3(year, c)
        
        #N in individual crops is calculated by summing the N from each water source
        #main crops is sim
        ifelse(individual_crop==FALSE,
               calc_df[, c] <- crop_volume$sum * w_source * no3_source,
               crop_df[, sel_cols] <- crop_df[, sel_cols] + sapply(crop_volume[, sel_cols], function(x) round(x*w_source*no3_source, 0)))
      }
      
      #aggregate the datasets to export
      #no need to aggregate the inidividual crops as the dataset already contains the sum of N for each system
      if (individual_crop==FALSE){
        calc_df$sum <- rowSums(calc_df[, seq(4, ncol(calc_df))])
        main_crop_df[,b] <- calc_df$sum
      }
      else {
        write_annual_data(module_name = 'Irrigation_module', 
                          subfolder_name = 'Irrigation_crop_N', 
                          file = crop_df, 
                          filename = paste0('irrigN_', b), 
                          year = year, 
                          subfolder_nameX2 = a)
      }
    }
    
    if (individual_crop==FALSE){
      ifelse(ncol(main_crop_df)>4, 
             main_crop_df$sum <- rowSums(main_crop_df[, seq(4, ncol(main_crop_df))]), 
             colnames(main_crop_df)[4] <- 'sum')
      write_annual_data(module_name = 'Irrigation_module', 
                        subfolder_name = 'Irrigation_crop_N', 
                        file = main_crop_df, 
                        filename = paste0('irrigN_', a), 
                        year = year, 
                        subfolder_nameX2 = 'Total')
    }
  }
}

loop_through_crop_irrigatioN <- function(year) {
  ## loops through both individual and total crop irrigatioN for both years
  # unit: kg N yr-1
  
  #year <- c(1999, 2009)
  cond <- c(TRUE, FALSE)
  for (i in cond) {
    general_crop_irrigatioN_func(year = year, individual_crop = i)
  }
}


## ----------------------- COMPUTE IRRIG SYS N-INPUT -----------------------##
## -------------------------------------------------------------------------##

general_func_irrig_sys_N <- function(year, irrig_sys) {
  # general function to calculate the total irrigation N for a given irrigation system
  # unit: kg N yr-1
  
  main_crops <- get_maincrops_names(2009)
  calc_df <- create_main_csv()
  
  for (i in main_crops) {
    crops <- get_crop_names(2009, i)
    
    for (j in crops) {
      crop_irrigN <- get_module_subfolder_output(module = 'Irrigation_module', 
                                                 submodule = 'Irrigation_crop_N',
                                                 submoduleX2 = i, 
                                                 file_pattern = j,
                                                 submoduleX3 = year)
      calc_df[, j] <- crop_irrigN[, irrig_sys]
    }
  }
  calc_df[, 'total'] <- rowSums(calc_df[, seq(4, ncol(calc_df))])
  return(calc_df)
  rm(list=c('main_crops', 'crops', 'crop_irrigN'))
}


compute_irrig_sys_N <- function(year) {
  # computes the irrigation N for each irrigation system
  # unit: kg N yr-1
  
  irrig_sys <- get_irrig_sys_names()
  
  for (i in irrig_sys) {
    irrig_sysN <- general_func_irrig_sys_N(year = year, irrig_sys = i)
    write_output_subfolder(module_name = 'Irrigation_module', 
                      subfolder_name = 'Irrigation_sys_N', 
                      file_df = irrig_sysN, 
                      filename = i, 
                      subfolderX2_name = year)
  }
  rm(list=c('irrig_sys', 'irrig_sysN'))
}

