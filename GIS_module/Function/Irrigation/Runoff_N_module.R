source('G:/O meu disco/MITERRA/MITERRA-PORTUGAL/GIS_module/Function/LU_irrigation_allocation.R', echo=TRUE)
source('./GIS_module/Function/MITERRA_fractions.R')

get_GIS_soilTexture <- function() {
  #100 - sandy soils
  #75 - loamy
  #50 - clay 
  
  texture <- select_files_miterra_folder('Common', 'Lf_SoilType')
}

runoff_assumptions_df <- function() {
  
  irrig_syst_assumptions <- data.frame(main_irrig_syst = c('furrow', 'other_grav', 'sprinkler', 'sprinkler', 'sprinkler', 'localized', 'localized'),
                                       irrig_syst <- c('furrow', 'other_grav', 'aspersion', 'cannon', 'pivot', 'drip', 'microaspersion'))
  return(irrig_syst_assumptions)
}

runoff_assumptions_distribution <- function(main_irrig_system) {
  # calls the assumptions df, selects the specified main irrigation system (e.g. localized)
  # returns the associated irrigation systems (e.g. drip, microaspersion)
  
  assumptions_df <- runoff_assumptions_df()
  id <- which(assumptions_df$main_irrig_syst==main_irrig_system)
  select_irrig_systems <- as.vector(assumptions_df[id, 2])
  
  return(select_irrig_systems)
}

set_localized_runoff <- function(calc_df, main_irrig_system) {
  #sets runoff losses from localized irrigation systems to 0
  
  if (main_irrig_system=='localized') {
    calc_df[, c('drip', 'microaspersion')] <- 0
  }
  
  return(calc_df)
}

aggregate_into_main_irrig_sys <- function(crop, main_irrig_system, calc_df) {
  # note: to be implemented in runoff_activity_data_N_LU
  # calculates the sum of each irrigation system within the specified main irrigation system
  # changes the sum column to the colname also according to the crop (e.g. maize_sprinkler)
  
  set_colname <- paste0(crop, '_', main_irrig_system)
  
  if(ncol(calc_df)>4) {
    calc_df$sum <- rowSums(calc_df[, seq(4, ncol(calc_df))])
  }
  colnames(calc_df)[ncol(calc_df)] <- set_colname
  return(calc_df)
}


get_crop_runoffN <- function(year, maincrop, crop) {
  # computes the potential N available to calculate runoff N losses (i.e. irrigation N)
  # analog to get_crop_irrigatioN but with a minor modification concerning rice paddies
  # runoff losses from rice paddies, before allocation to rice or heterogeneous LU classes, are set to 0
  # unit : kg N
  
  crop_irrigN <- get_crop_irrigatioN(year = year, individual_crop = T, maincrop = maincrop, crop = crop)
  if (crop=='rice') {
  # sets the runoff from each irrigation system (i.e. furrow) to 0
    crop_irrigN[, seq(4, ncol(crop_irrigN))] <- 0
  }
  return(crop_irrigN)
}


create_runoff_subfolders <- function(year, runoff_subfolder) {
  
  # 1 - create runoff subfolder in N_irrigation
  path <- create_dir_volumes_path(year, 'N_irrigation', 'runoff') 
  # 2 - create runoff_subfolder sub-subfolder, which can be either Activity data or Calculations
  sub_path <- file.path(path, runoff_subfolder)
  dir.create(path = sub_path)
  return(sub_path)
}

write_runoff_LU <- function(year, runoff_subfolder,  rasterfile, filename) {
  
  path <- create_runoff_subfolders(year, runoff_subfolder)
  output_folder <- file.path(path, filename)
  tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
  writeRaster(rasterfile, output_folder, options=tifoptions, format='GTiff')
}

runoff_activity_data_N_LU <- function(year, LU_class, main_irrig_system) {
  # this function is used to calculate the activity data to calculate runoff N losses from irrigation systems
  # using the aforementioned assumptions
  # the user specifies a main irrigation system (e.g. localized, sprinkler, furrow) which is disaggregate into its irrigation systems
  # rice and drip runoff N losses are set to 0
  # for the remaining systems (sprinkler, furrow, other_grav) the irrigation N is calculated to each main irrigation system
  # and then it is regularly allocated to LU classes
  # returns a dataframe 
  # unit: kg N (which is the activity to later do the GIS_computations based on soil texture and LU classes)
  
  # define and choose the selected main irrigation system
  irrig_systems <- runoff_assumptions_distribution(main_irrig_system)
  
  crop_class <- LU_class_allocation(LU_class)
  cereal_condition <- LU_cereal_conditions(LU_class)
  calc_df <- create_main_csv()
  
  #correct allocation to heterogeneous from each LU class, hence 1-proportion_hetero
  allocation_df <- 1-loop_allocation_LU_routine(year)[, LU_class]
  
  for (a in crop_class) {
    ifelse(a=='cereals', crops <- cereal_condition, crops <- get_crop_names(2009, a))
    
    for (b in crops) {
      # get available N to calculate runoff N losses and set rice runoff losses to 0
      crop_irrgn <- get_crop_runoffN(year = year, maincrop = a, crop = b)
      # subset crop_irrign according to the main irrigation system specified
      crop_irrgn <- crop_irrgn[, c('Muni_ID', 'ID', 'Muni', irrig_systems)]
      # if main_irrig_system is localized, set runoff losses to 0
      crop_irrgn <- set_localized_runoff(crop_irrgn, main_irrig_system)
      
      # aggregate irrigation systems into one main irrigation system col and change its name
      crop_irrgn <- aggregate_into_main_irrig_sys(crop = b, main_irrig_system = main_irrig_system, calc_df = crop_irrgn)
      calc_df <- merge(calc_df, crop_irrgn[, c(1, ncol(crop_irrgn))], 'Muni_ID', sort=FALSE)
      
      #compute total N correct for heterogeneous allocation for the specified LU_class
      calc_df[, ncol(calc_df)] <- calc_df[, ncol(calc_df)] * allocation_df
    }
  }
  ifelse(ncol(calc_df)>4, 
         calc_df$N_runoff_act_data <- round(rowSums(calc_df[, seq(4, ncol(calc_df))]), 0),
         colnames(calc_df)[4] <- 'N_runoff_act_data')
  
  return(calc_df)
}


runoff_activity_data_N_heterogeneous <- function(year, main_irrig_system) {
  # modified version of compute_cropN_heterogeneous()
  # calculates the runoff N activity data for a later GIS computation based on soil texture and f_runoff
  # note: the runoff N losses of rice and localized systems are already set to 0
  # unit: kg N 
  
  irrig_systems <- runoff_assumptions_distribution(main_irrig_system)
  allocation_df <- loop_allocation_LU_routine(year)
  lu_class <- colnames(allocation_df[, seq(7, ncol(allocation_df))]) #select the other LU classes
  calc_df <- create_main_csv()
  
  for (i in lu_class) {
    
    crop_class <- LU_class_allocation(i)
    cereal_condition <- LU_cereal_conditions(i)
    
    for (a in crop_class) {
      ifelse(a=='cereals', crops <- cereal_condition, crops <- get_crop_names(2009, a))
      
      for (b in crops) {
        # get available N to calculate runoff N losses and set rice runoff losses to 0
        crop_irrgn <- get_crop_runoffN(year = year, maincrop = a, crop = b)
        # subset crop_irrign according to the main irrigation system specified
        crop_irrgn <- crop_irrgn[, c('Muni_ID', 'ID', 'Muni', irrig_systems)]
        # if main_irrig_system is localized, set runoff losses to 0
        crop_irrgn <- set_localized_runoff(crop_irrgn, main_irrig_system)
        
        # aggregate irrigation systems into one main irrigation system col and change its name
        crop_irrgn <- aggregate_into_main_irrig_sys(crop = b, main_irrig_system = main_irrig_system, calc_df = crop_irrgn)
        calc_df <- merge(calc_df, crop_irrgn[, c(1, ncol(crop_irrgn))], 'Muni_ID', sort=F)
        
        #compute total N correct for heterogeneous allocation for the specified LU_class
        calc_df[, ncol(calc_df)] <- calc_df[, ncol(calc_df)] * allocation_df[, i]
      }
    }
  }
  calc_df$N_runoff_act_data <- round(rowSums(calc_df[, seq(4, ncol(calc_df))]), 0)
  return(calc_df)
}


spatial_allocation_runoffN_LU <- function(year, all) {
  # this function allocates the available N to compute runoff N losses for each LU besides heterogeneous
  # note that LU classes where runoff losses was automatically set to 0 are not rasterized (i.e. rice, localized)
  # unit : kg N/LU ha
  
  LU_classes <- loop_LU_classes()
  LU_classes <- LU_classes[seq(1, (length(LU_classes)-1))] #disregard heterogeneous
  
  ifelse(all==TRUE, 
         main_irrig <- c('localized', 'sprinkler', 'other_grav', 'furrow'),
         main_irrig <- c('sprinkler', 'other_grav', 'furrow'))
  
  for (i in main_irrig) {
    
    calc_df <- create_main_csv()
    
    for (j in LU_classes) {
      
      path <- create_runoff_subfolders(year = year, runoff_subfolder = 'Activity_data')
      # sum and aggregate into one dataframe each main irrig systems of a LU
      # unit: kg N/LU ha
      rf_N_data <- runoff_activity_data_N_LU(year = year, LU_class = j, main_irrig_system = i)
      
      # if the sum of all N at the mainland level is 0, then do not rasterize all this crap
      if (sum(rf_N_data$N_runoff_act_data>0)){
        lu_area <- get_correction_LU_statistical_df(j, year)[, 4] #select only CLC_LU_area
        rf_N_data$N_runoff_act_data_nha <- rf_N_data$N_runoff_act_data/lu_area
        
        r_lu <- rasterize_data_muni(rf_N_data, 'Muni_ID', 'N_runoff_act_data_nha')
        LU_GIS <- get_LU_class_raster(j, year)
        LU_irrigN <- LU_GIS*r_lu
        write_runoff_LU(year = year, runoff_subfolder = 'Activity_data', rasterfile = LU_irrigN, 
                        filename = paste0(j, '_', i))
      }
    }
  }
}


spatial_allocation_runoffN_heterogeneous <- function(year, all) {
  # this function allocates the available N to compute runoff N losses for each LU besides heterogeneous
  # note that LU classes where runoff losses was automatically set to 0 are not rasterized (i.e. rice, localized)
  # unit : kg N/LU ha
  
  LU_classes <- loop_LU_classes()
  LU_classes <- LU_classes[seq(1, (length(LU_classes)-1))] #disregard heterogeneous
  
  ifelse(all==TRUE, 
         main_irrig <- c('localized', 'sprinkler', 'other_grav', 'furrow'),
         main_irrig <- c('sprinkler', 'other_grav', 'furrow'))
  
  for (i in main_irrig) {
    
    calc_df <- create_main_csv()
    
      path <- create_runoff_subfolders(year = year, runoff_subfolder = 'Activity_data')
      # sum and aggregate into one dataframe each main irrig systems of a LU
      # unit: kg N/LU ha
      rf_N_data <- runoff_activity_data_N_heterogeneous(year = year, main_irrig_system = i)
      
      # if the sum of all N at the mainland level is 0, then do not rasterize all this crap
      if (sum(rf_N_data$N_runoff_act_data>0)){
        lu_area <- get_correction_LU_statistical_df('heterogeneous', year)[, 4] #select only CLC_LU_area
        rf_N_data$N_runoff_act_data_nha <- rf_N_data$N_runoff_act_data/lu_area
        
        r_lu <- rasterize_data_muni(rf_N_data, 'Muni_ID', 'N_runoff_act_data_nha')
        LU_GIS <- get_LU_class_raster('heterogeneous', year)
        LU_irrigN <- LU_GIS*r_lu
        write_runoff_LU(year = year, runoff_subfolder = 'Activity_data', rasterfile = LU_irrigN, 
                        filename = paste0('heterogeneous_', i))
    }
  }
}


mosaic_runoff_main_irrig_systems <- function(year) {
  # the starting point of this function is the available N for runoff calculations for each LU class 
  # creates a raster mosaic for each main irrigation system with possible runoff losses
  # unit : kg N/LU ha
  
  act_data <- create_runoff_subfolders(year = year, runoff_subfolder = 'Activity_data')
  caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  
  # runoff_rice == 0 ;; runoff_localized == 0
  irrig_systems <- c('sprinkler', 'other_grav', 'furrow')
  r_list <- list()
  
  for (i in irrig_systems) {
    print(paste0('Working with ', i))
    select_irrig_sys_file <- list.files(path = act_data, pattern = i, full.names = T)
    
    if(length(select_irrig_sys_file)==1) {
      write_runoff_LU(year = year, runoff_subfolder = 'Activity_data', rasterfile = r_mosaic, 
                      filename = paste0('MOSAIC_', i))
    }
    else {
    r_list <- lapply(select_irrig_sys_file, function(x) raster(x))
    r_list$fun <- sum
    r_mosaic <- do.call(raster::mosaic,r_list)
    r_mosaic <- r_mosaic*caa
    
    write_runoff_LU(year = year, runoff_subfolder = 'Activity_data', rasterfile = r_mosaic, 
                    filename = paste0('MOSAIC_', i))
    }
  }
  rm(list=c('caa', 'r_mosaic', 'r_list'))
}


soilTexture_reclass_matrix <- function(main_irrig_system) {
  # define the reclassification range for application regarding soil texture 
  # reclassification values are reduction factors, where rf_fct == 1, no reduction factor is applied
  
  sprinkler <- c(0, 99, 1, 99, 100, 0)
  furrow <- c(0, 49, 0, 49, 99, 1, 99, 100, 0.5) #reduction factor of 50% for f_runoff 
  
  rcl_list <- list(sprinkler, furrow)
  names(rcl_list) <- c('sprinkler', 'furrow')
  
  return(rcl_list[[main_irrig_system]])
}

soilTexture_reclass_irrig_system <- function(main_irrig_system) {
  #reclassifies soil texture based on main_irrig_system and soilTexture_reclass_matrix
  
  soil_txt <- get_GIS_soilTexture()
  rcl_matrix <- soilTexture_reclass_matrix(main_irrig_system)
  rc_soil_txt <- reclassify(soil_txt, rcl = rcl_matrix)
  
  return(rc_soil_txt)
}


furrow_f_runoff <- function(ovr_f_runoff, year) {
  # calculate the f_runoff of furrow based on different soil textures
  
  rc_soil_txt <- soilTexture_reclass_irrig_system('furrow')
  
  #modify f_runoff in sandy soil, reduction factor of 50%
  furrow_sandy <- rc_soil_txt[[1]]==0.5
  furrow_sandy <- furrow_sandy*ovr_f_runoff
  
  # set the f_runoff for clay/loamy soils of 20% (1999) and 10% (2009)
  ifelse(year==2009, f_runoff <- 0.10, f_runoff <- 0.2)
  furrow_other_soils <- rc_soil_txt[[1]]==1
  furrow_other_soils <- furrow_other_soils*f_runoff
  
  #join furrow runoff fraction rasters
  furrow_frunoff <- furrow_sandy+furrow_other_soils
  rm(list = c('furrow_sandy', 'furrow_other_soils', 'rc_soil_txt'))
  return(furrow_frunoff)
}

sprinkler_f_runoff <- function(ovr_f_runoff) {
  # calculate the f_runoff of sprinkler based on different soil textures
  
  rc_soil_txt <- soilTexture_reclass_irrig_system('sprinkler')
  sprinkler_frunoff <- rc_soil_txt*ovr_f_runoff
  return(sprinkler_frunoff)
  rm(list=c('rc_soil_txt'))
}


call_runoff_fractions_irrig_systems <- function(year, main_irrig_system) {
  # this is used to calculate runoff fractions of different irrigation systems based on soil texture and the aforementioned assumptions
  
  mit_f_runoff <- get_GIS_file(file_pattern = paste0('Rf', year_prefix(year)), folder_name = 'MITERRA_fractions')
  
  if (main_irrig_system=='furrow') {
    ifelse(year==2009, f_runoff <- 0.10, f_runoff <- 0.2)
    frunoff <- furrow_f_runoff(ovr_f_runoff = mit_f_runoff, year = year)
  }
  else if (main_irrig_system=='other_grav') {
    frunoff <- mit_f_runoff
  }
  else if (main_irrig_system=='sprinkler') {
    frunoff <- sprinkler_f_runoff(ovr_f_runoff = mit_f_runoff)
  }
  return(frunoff)
}

compute_irrig_runoff_nha_mosaic <- function(year, main_irrig_system, write) {
  # gets the raster mosaics for the specified main irrig system regarding the available irrig N to calculate runoff losses
  # calls  main irrig system-specific runoff fractions based on soil texture
  # computes runoff N losses for the specified main irrig system
  # user can select to either write to the subfolder "Runoff_N" or to return those data for plotting
  # unit: kg N/LU ha
  
  # get mosaic data
  act_data <- create_runoff_subfolders(year = year, runoff_subfolder = 'Activity_data')
  mosaic_irrig_sys <- raster(
    list.files(path = act_data, pattern = paste0('MOSAIC_', main_irrig_system), full.names = TRUE))
  
  # call f_runoof for the irrig sys
  f_runoff <- call_runoff_fractions_irrig_systems(year, main_irrig_system)
  
  # compute runoff losses for the specified irrig sys
  # unit: kg N/LU ha
  r_runoffN_irrig_sys <- f_runoff*mosaic_irrig_sys
  
  #create sub-directory for runoffN
  runoffN_dir <- create_runoff_subfolders(year = year, runoff_subfolder = 'Runoff_N')
  
  
  if(write==TRUE) {
    write_runoff_LU(year = year, runoff_subfolder = 'Runoff_N', rasterfile = r_runoffN_irrig_sys, 
                    filename = paste0('Runoff_nha_', main_irrig_system))
  }
  else {return(r_runoffN_irrig_sys) }
}

loop_runoff_nha <- function(year) {
  # loops around the specified main irrigation systems
  # calls compute_irrig_runoff_nha_mosaic and writes the runoff N losses to the subfolder "Runoff_N"
  # unit: kg N/LU ha
  
  options(warning=-1)
  main_irrig <- c('sprinkler', 'furrow', 'other_grav')
  sapply(main_irrig, function(x) {
    print(paste0('Working with ', x))
    compute_irrig_runoff_nha_mosaic(year, x, write = TRUE)
    beepr::beep(sound='wilhelm')
  }
  )
}

compute_total_LU_runoff_nha <- function(year, write) {
  
  runoffNha_folder <- create_runoff_subfolders(year = year, runoff_subfolder = 'Runoff_N')
  runoffNha_files <- list.files(path = runoffNha_folder, pattern = 'Runoff_nha', full.names = TRUE)
  list_runoffNha <- lapply(runoffNha_files, raster)
  
  runoffNha_sum <- do.call('sum', list_runoffNha)
  
  if(write==TRUE) {
    write_runoff_LU(year = year, runoff_subfolder = 'Runoff_N', rasterfile = runoffNha_sum, 
                  filename = 'MOSAIC_Runoff')
  } else {return(runoffNha_sum) }
  
  rm(list=c('list_runoffNha'))
}

compute_runoff_nha_muni <-function(year, write) {
  # this function sums the total runoff losses (kg N/LU ha) for each municipality
  # because the total N is the same as in the statistical calculations, there is no neeed to apply an adjustment factor
  # output: returns a dataframe with the total runoff N losses, both in kg N and kg N/UAA ha
  
  library(foreach)
  library(doParallel)
  # call runoff_nha mosaic
  runoffNha_folder <- create_runoff_subfolders(year = year, runoff_subfolder = 'Runoff_N')
  runoff_mosaic <- raster(list.files(path = runoffNha_folder, pattern = 'MOSAIC', full.names = TRUE))
  
  muni_df <- create_main_csv()
  muni_shp <- get_muni_shp()
  
  registerDoParallel(cores=3)
  #getDoParWorkers()
  
  print('Starting parallel processing ...')
  irrigN <- foreach(i=1:278, .packages=c('raster', 'rgdal'), .combine=rbind) %dopar% {
    j <- muni_df[i,1]
    print(paste0('Working with the municipality ID ', j))
    sb <- subset(muni_shp, Muni_ID==j)
    r_mosaic <- crop(runoff_mosaic, sb)
    r_mask <- mask(r_mosaic, sb)
    #compute total N in runoff N losses for a municipality
    runoff_kgN <- round(cellStats(r_mask, 'sum'), 0)
    
    muni_id <- which(muni_df$Muni_ID==j)
    muni_df[muni_id, 'irrig_runoff_kgN'] <- runoff_kgN
    data.frame(muni_df[i, seq(1,3)], runoff_kgN)
  }
  #unregister parallel processing
  registerDoSEQ()
  
  uaa <- load_uaa(year)
  irrigN$runoff_kgNhh <- round(irrigN$runoff_kgN/uaa, 2)
  
  if (write==TRUE) {
    runoff_path <- create_runoff_subfolders(year = year, runoff_subfolder = 'Runoff_N')
    filename <- file.path(runoff_path, paste0('Runoff_dataset_municipality', year_prefix(year)))
    write.csv(irrigN, paste0(filename, '.csv'))
  }
  return(irrigN)
  rm(list=c('sb', 'r_mosaic', 'r_mask', 'muni_shp', 'runoff_mosaic'))
}
