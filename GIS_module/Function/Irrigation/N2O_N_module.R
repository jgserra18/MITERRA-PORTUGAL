source('./Irrigation_module/Functions/Global_irrigation_functions.R')
source('./GIS_module/Function/General_GIS_functions.R')
source('./Irrigation_module/Functions/Compute_crop_irrigatioN.R')
source('./GIS_module/Function/GW_computation_functions.R')
source('./GIS_module/Function/Irrigation/Runoff_N_module.R')


# define climatic regions and export in raster format
# disaggregate gross irrigation N into all the different irrigation systems 
  # standard allocation procedure
  # create mosaic for each irrigation system
# call Cayuela et al (2017) direct N2O EF for the Mediterranean region and IPCC (2006) for the remaining areas

get_climatic_region_folder <- function() {
  
  climatic_folder <- select_maindata_pattern('Climatic')
  climatic_regions_folder <- list.files(path = climatic_folder, pattern='Climatic_regions', full.names=TRUE)
  return(climatic_regions_folder)
}

get_climatic_regions <- function() {
  #gets the dataframe with the climatic regions and respective municipality
  
  climatic_regions_folder <- get_climatic_region_folder()
  climatic_regions_file <- read.csv(
    list.files(path = climatic_regions_folder, pattern='Climate_muni', full.names=TRUE)
  )
  return(climatic_regions_file)
}

GIS_climatic_regions <- function() {
  # rasterizes the climatic regions and writes the output to get_climatic_region_folder
  
  #get output path
  out_path <- get_climatic_region_folder()
  filename <- file.path(out_path, 'Climatic_regions')
  
  #rasterize climatic region
  climatic_id <- get_climatic_regions()
  GIS_climatic_region <- rasterize_data_muni( df_to_rasterize = climatic_id, merged_col = 'Muni_ID', field_rasterize = 'Climatic_ID')
  
  #write raster
  tifoptions <- c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
  writeRaster(GIS_climatic_region, filename, format='GTiff', options=tifoptions, overwrite=TRUE)
}

get_GIS_climatic_region <- function() {
  # get GIS file of climatic region IPCC (2006)
  
  climatic_regions_folder <- get_climatic_region_folder()
  r_climatic_regions <- raster(
    list.files(path = climatic_regions_folder, pattern='.tif', full.names=TRUE)
  )
  return(r_climatic_regions)
}

## -----------------------------------------------------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------------------------------------------------

get_mediterranean_EFs <- function() {
  # get MEditerranean EFs determined in Cayuela et al (2017)
  
  ef_df <- data.frame(ef_irrig_sys = c('gravity', 'sprinkler', 'localized'),
                      ef = c(0.0047, 0.0091, 0.0051))
  return(ef_df)
}

irrig_sys_assumptions_df <- function() {
  
  irrig_sys_df <- data.frame(ef_irrig_sys = c('gravity', 'gravity', 'sprinkler', 'sprinkler', 'sprinkler', 'localized', 'localized'),
                             irrig_sys = c('furrow', 'other_grav', 'aspersion', 'pivot', 'cannon', 'drip', 'microaspersion'))
}

complete_mediterranean_EFs <- function() {
  # create the complete mediterranean EF dataframe
  # with all the main irrig as well as the individual irrig systems
  
  main_df <- irrig_sys_assumptions_df()
  main_ef <- get_mediterranean_EFs()
  
  main_df <- merge(main_df, main_ef, 'ef_irrig_sys')
  return(main_df)
}


irrig_sys_assumptions_distribution <- function(main_irrig_system) {
  # calls the irrig_sys_assumptions_df, selects the specified main irrigation system (e.g. localized)
  # returns the associated irrigation systems (e.g. drip, microaspersion)
  
  assumptions_df <- irrig_sys_assumptions_df()
  id <- which(assumptions_df$ef_irrig_sys==main_irrig_system)
  select_irrig_systems <- as.vector(assumptions_df[id, 2])
  
  return(select_irrig_systems)
}

## -----------------------------------------------------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------------------------------------------------

create_n2o_subfolders <- function(year, n2o_subfolder) {
  
  # 1 - create n2o subfolder in N_irrigation
  path <- create_dir_volumes_path(year, 'N_irrigation', 'n2o') 
  # 2 - create n2o_subfolder sub-subfolder, which can be either Activity data or Calculations
  sub_path <- file.path(path, n2o_subfolder)
  dir.create(path = sub_path)
  return(sub_path)
}


write_n2o_LU <- function(year, n2o_subfolder,  rasterfile, filename) {
  
  path <- create_n2o_subfolders(year, n2o_subfolder)
  output_folder <- file.path(path, filename)
  tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
  writeRaster(rasterfile, output_folder, options=tifoptions, format='GTiff')
}

## -----------------------------------------------------------------------------------------------------------------------------------------
## ------------------------------ FUNCTIONS TO AGGREGATE DATA BASED ON THE MAIN IRRIGATION SYSTEMS -------------------------------------- ##
## -----------------------------------------------------------------------------------------------------------------------------------------

reference_irrig_system <- function(main_irrig_system, individual_irrig_sys) {
  # this function is the baseline reference unit regarding irrigation systems for all calculations
  # if individual_irrig_sys is missing, then the system calculates based on the main irrig systems (localized, sprinkler, gravity)
  # otherwise individual irrig systems (e.g. aspersion, drip, furrow) are calculated and exported to the act data folder
  
  if(missing(individual_irrig_sys)==TRUE) {
    irrig_sys <- irrig_sys_assumptions_distribution(main_irrig_system)
  } else if (missing(main_irrig_system)==TRUE) {
    irrig_sys <- individual_irrig_sys
  }
  return(irrig_sys)
}


n2o_activity_data_N_LU <- function(year, LU_class, main_irrig_system, individual_irrig_sys) {
  # this function is used to calculate the activity data to calculatedirect N2O emissions from irrigation systems
  # using the aforementioned assumptions
  # the user specifies a main irrigation system (e.g. localized, sprinkler, furrow) which is disaggregate into its irrigation systems
  # for the remaining systems (sprinkler, furrow, other_grav) the irrigation N is calculated to each main irrigation system
  # and then it is regularly allocated to LU classes
  # returns a dataframe 
  # unit: kg N 
  
  # define and choose the selected main irrigation system
  # if the user specifies an individual systen 
  main_irrig_system <- reference_irrig_system(main_irrig_system, individual_irrig_sys)

  crop_class <- LU_class_allocation(LU_class)
  cereal_condition <- LU_cereal_conditions(LU_class)
  calc_df <- create_main_csv()
  
  #correct allocation to heterogeneous from each LU class, hence 1-proportion_hetero
  allocation_df <- 1-loop_allocation_LU_routine(year)[, LU_class]
  
  for (a in crop_class) {
    ifelse(a=='cereals', crops <- cereal_condition, crops <- get_crop_names(2009, a))
    
    for (b in crops) {
      # get gross irrigation N
      crop_irrgn <- get_crop_irrigatioN(year = year, individual_crop = TRUE, maincrop = a, crop = b)
      
      # subset crop_irrign according to the main irrigation system specified
      crop_irrgn <- crop_irrgn[, c('Muni_ID', 'ID', 'Muni', main_irrig_system)]
      
      # aggregate irrigation systems into one main irrigation system col and change its name
      crop_irrgn <- aggregate_into_main_irrig_sys(crop = b, main_irrig_system = main_irrig_system, calc_df = crop_irrgn)
      calc_df <- merge(calc_df, crop_irrgn[, c(1, ncol(crop_irrgn))], 'Muni_ID', sort=FALSE)
      
      #compute total N correct for heterogeneous allocation for the specified LU_class
      calc_df[, ncol(calc_df)] <- calc_df[, ncol(calc_df)] * allocation_df
    }
  }
  ifelse(ncol(calc_df)>4, 
         calc_df$N_n2o_act_data <- round(rowSums(calc_df[, seq(4, ncol(calc_df))]), 0),
         colnames(calc_df)[4] <- 'N_n2o_act_data')
  
  return(calc_df)
}

n2o_activity_data_N_heterogeneous <- function(year, main_irrig_system, individual_irrig_sys) {
  # modified version of compute_cropN_heterogeneous()
  # calculates the direct N2O activity data for a later GIS computation 
  # unit: kg N 
  
  options(warnings=-1)
  main_irrig_system <- reference_irrig_system(main_irrig_system, individual_irrig_sys)
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
        crop_irrgn <- get_crop_irrigatioN(year = year, individual_crop = TRUE, maincrop = a, crop = b)
        # subset crop_irrign according to the main irrigation system specified
        crop_irrgn <- crop_irrgn[, c('Muni_ID', 'ID', 'Muni', main_irrig_system)]
        
        # aggregate irrigation systems into one main irrigation system col and change its name
        crop_irrgn <- aggregate_into_main_irrig_sys(crop = b, main_irrig_system = main_irrig_system, calc_df = crop_irrgn)
        calc_df <- merge(calc_df, crop_irrgn[, c(1, ncol(crop_irrgn))], 'Muni_ID', sort=F)
        
        #compute total N correct for heterogeneous allocation for the specified LU_class
        calc_df[, ncol(calc_df)] <- calc_df[, ncol(calc_df)] * allocation_df[, i]
      }
    }
  }
  calc_df$N_n2o_act_data <- round(rowSums(calc_df[, seq(4, ncol(calc_df))]), 0)
  return(calc_df)
}

spatial_allocation_n2o_LU <- function(year, irrig_param) {
  # this function allocates the available N to compute direct N2O emissions for each LU besides heterogeneous
  # the user can specify whether the spatial allocation is for all main irrig systems (irrig_param=='all_main')
  # or all specific irrigation systems (e.g. gun, cannon, etc) (irrig_param=='all_individual')
  # unit : kg N/LU ha
  
  LU_classes <- loop_LU_classes()
  LU_classes <- LU_classes[seq(1, (length(LU_classes)-1))] #disregard heterogeneous
  
  ifelse(irrig_param=='all_main',
    main_irrig <- get_mediterranean_EFs()[, 1],
    main_irrig <- irrig_sys_assumptions_df()[, 2])
  
  for (i in main_irrig) {
    
    calc_df <- create_main_csv()
    
    for (j in LU_classes) {
      
      path <- create_n2o_subfolders(year = year, n2o_subfolder  = 'Activity_data')
      # sum and aggregate into one dataframe each main irrig systems of a LU
      # this should be according to the specified irrig_param
      # unit: kg N/LU ha
      ifelse(irrig_param=='all_main',
        n2o_N_data <- n2o_activity_data_N_LU(year = year, LU_class = j, main_irrig_system = i),
        n2o_N_data <- n2o_activity_data_N_LU(year = year, LU_class = j, individual_irrig_sys = i))
      
      # if the sum of all N at the mainland level is 0, then do not rasterize all this crap
      if (sum(n2o_N_data$N_n2o_act_data>0)){
        lu_area <- get_correction_LU_statistical_df(j, year)[, 4] #select only CLC_LU_area
        n2o_N_data$N_n2o_act_data_nha <- n2o_N_data$N_n2o_act_data/lu_area
        
        r_lu <- rasterize_data_muni(n2o_N_data, 'Muni_ID', 'N_n2o_act_data_nha')
        LU_GIS <- get_LU_class_raster(j, year)
        LU_irrigN <- LU_GIS*r_lu
        write_n2o_LU(year = year, n2o_subfolder = 'Activity_data', rasterfile = LU_irrigN, 
                        filename = paste0(j, '_', i))
      }
    }
  }
}

spatial_allocation_n2o_heterogeneous <- function(year, irrig_param) {
  # this function allocates the available N to compute runoff N losses for each LU besides heterogeneous
  # note that LU classes where runoff losses was automatically set to 0 are not rasterized (i.e. rice, localized)
  # the user can specify whether the spatial allocation is for all main irrig systems (irrig_param=='all_main')
  # or all specific irrigation systems (e.g. gun, cannon, etc) (irrig_param=='all_individual')
  # unit : kg N/LU ha
  
  LU_classes <- loop_LU_classes()
  LU_classes <- LU_classes[seq(1, (length(LU_classes)-1))] #disregard heterogeneous
  
  ifelse(irrig_param=='all_main',
         main_irrig <- get_mediterranean_EFs()[, 1],
         main_irrig <- irrig_sys_assumptions_df()[, 2])
  
  for (i in main_irrig) {
    
    calc_df <- create_main_csv()
    
    path <- create_n2o_subfolders(year = year, n2o_subfolder  = 'Activity_data')
    # sum and aggregate into one dataframe each main irrig systems of a LU
    # this should be according to the specified irrig_param
    # unit: kg N/LU ha
    ifelse(irrig_param=='all_main',
           n2o_N_data <- n2o_activity_data_N_heterogeneous(year = year, main_irrig_system = i),
           n2o_N_data <- n2o_activity_data_N_heterogeneous(year = year, individual_irrig_sys = i)
    )

    # if the sum of all N at the mainland level is 0, then do not rasterize all this crap
    if (sum(n2o_N_data$N_n2o_act_data>0)){
      lu_area <- get_correction_LU_statistical_df('heterogeneous', year)[, 4] #select only CLC_LU_area
      n2o_N_data$N_n2o_act_data_nha <- n2o_N_data$N_n2o_act_data/lu_area
      
      r_lu <- rasterize_data_muni(n2o_N_data, 'Muni_ID', 'N_n2o_act_data_nha')
      LU_GIS <- get_LU_class_raster('heterogeneous', year)
      LU_irrigN <- LU_GIS*r_lu
      write_n2o_LU(year = year, n2o_subfolder = 'Activity_data', rasterfile = LU_irrigN, 
                   filename = paste0('heterogeneous_', i))
    }
  }
}

mosaic_n2o_main_irrig_systems <- function(year, irrig_param) {
  # the starting point of this function is the available N fordirect n2o emissions for each LU class 
  # creates a raster mosaic for each main irrigation system with possible direct n2o emissions
  # the user can specify whether the spatial allocation is for all main irrig systems (irrig_param=='all_main')
  # or all specific irrigation systems (e.g. gun, cannon, etc) (irrig_param=='all_individual')
  # unit : kg N/LU ha
  
  act_data <- create_n2o_subfolders(year = year, n2o_subfolder =  'Activity_data')
  caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  
  ifelse(irrig_param=='all_main',
         main_irrig <- get_mediterranean_EFs()[, 1],
         main_irrig <- irrig_sys_assumptions_df()[, 2])

  r_list <- list()
  
  for (i in main_irrig) {
    print(paste0('Working with ', i))
    select_irrig_sys_file <- list.files(path = act_data, pattern = i, full.names = T)
    
    if(length(select_irrig_sys_file)==1) {
      write_n2o_LU(year = year, n2o_subfolder = 'Activity_data', rasterfile = r_mosaic, 
                      filename = paste0('MOSAIC_', i))
    }
    else {
      r_list <- lapply(select_irrig_sys_file, function(x) raster(x))
      r_list$fun <- sum
      r_mosaic <- do.call(raster::mosaic,r_list)
      r_mosaic <- r_mosaic*caa
      
      write_n2o_LU(year = year, n2o_subfolder = 'Activity_data', rasterfile = r_mosaic, 
                      filename = paste0('MOSAIC_', i))
    }
  }
  rm(list=c('caa', 'r_mosaic', 'r_list'))
}

## -----------------------------------------------------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------------------------------------------------

compute_n.n2o_temperate <- function(gis_climatic, GIS_lu_irrig) {
  # general function to GIS compute the direct N-N2O emissions from a given irrigation system raster 
  # temperate climatic regions
  # unit: kg N-N2O/LU ha
  
  # define IPCC (2006) emission factor for direct N2O emissions
  ef_ipcc <- 0.01
  
  # reclassify temperate regions 
  gis_climatic <- reclassify(gis_climatic, rcl=c(0, 1.9, 0, 1.9, 2, 1) )
  irrigN_temperate <- mask(crop(GIS_lu_irrig, extent(gis_climatic)), gis_climatic)
  n.n2o_temperate <-  irrigN_temperate * ef_ipcc
  return(n.n2o_temperate)
}

identify_irrig_sys_EF <- function(main_irrig_system, ef_df) {
  # receives as parameters the specified irrig system, which can be either the main irrig systems or individual
  # as well as ef_df
  # output: the row number (or ID) to specify the associated ef_med
  
  main_irrig <- c('localized', 'gravity', 'sprinker')
  if(main_irrig_system %in% main_irrig) {
    id <- which(ef_df[,1]==main_irrig_system)
  }
  else {
    id <- which(ef_df[, 2]==main_irrig_system)
  }
  return(id)
}

compute_n.n2o_mediterranean <- function(gis_climatic, GIS_lu_irrig, main_irrig_system) {
  # general function to GIS compute the direct N-N2O emissions from a given irrigation system raster 
  # mediterrnaean climatic regions
  # unit: kg N-N2O/LU ha
  
  # call mediterranean EFs (Cayuela et al 2017)
  # select the appropriate EF based on the specified main irrig system
  ef_med <- complete_mediterranean_EFs()
  id <- identify_irrig_sys_EF(main_irrig_system = main_irrig_system, ef_med)
  ef_med <- ef_med[id, 3]
  
  # reclassify temperate regions 
  gis_climatic <- reclassify(gis_climatic, rcl=c(0, 1.9, 1, 1.9, 2, 0) )
  irrigN_med <- mask(crop(GIS_lu_irrig, extent(gis_climatic)), gis_climatic)
  n.n2o_med <-  irrigN_med * ef_med
  return(n.n2o_med)
}

compute_aggregated_n.n2o <- function(gis_climatic, GIS_lu_irrig, main_irrig_system) {
  # computes the sum of the N-N2O emissions from both different climatic regions
  # to be used in compute_irrig_n2o_nha_mosaic as a general function
  
  print('Computing N-N2O emissions in the Mediterranean regions...')
  n.n2o_med <- compute_n.n2o_mediterranean(gis_climatic, GIS_lu_irrig, main_irrig_system)
  
  print('Computing N-N2O emissions in the temperate regions...')
  n.n2o_temp <- compute_n.n2o_temperate(gis_climatic, GIS_lu_irrig)
  
  total_n.n2o <- n.n2o_temp+n.n2o_med
  return(total_n.n2o)
  rm(list=c('n.n2o_med', 'n.n2o_temp'))
}

compute_irrig_n2o_nha_mosaic <- function(year, main_irrig_system, write) {
  # gets the raster mosaics for the specified main irrig system regarding the available irrig N to calculate runoff losses
  # calls the aggregated N-N2O emissions computation function, which accounts for both the mediterranean and temperate regions
  # computes direct N-N2O losses for the specified main irrig system
  # user can select to either write to the subfolder "N2O_N" or to return those data for plotting
  # unit: kg N-N2O/LU ha
  
  # get gis clmatic
  GIS_climatic_reg <- get_GIS_climatic_region()
  # get mosaic data
  act_data <- create_n2o_subfolders(year = year, n2o_subfolder = 'Activity_data')
  mosaic_irrig_sys <- raster(
    list.files(path = act_data, pattern = paste0('MOSAIC_', main_irrig_system), full.names = TRUE))
  
  total_n.n2o_irrig_sys <- compute_aggregated_n.n2o(gis_climatic = GIS_climatic_reg, GIS_lu_irrig = mosaic_irrig_sys, 
                                                    main_irrig_system = main_irrig_system)
  #create sub-directory for runoffN
  n2o_dir <- create_n2o_subfolders(year = year, n2o_subfolder =  'N2O_N')
  
  if(write==TRUE) {
    write_n2o_LU(year = year, n2o_subfolder =  'N2O_N', rasterfile = total_n.n2o_irrig_sys, 
                    filename = paste0('N2O_nha_', main_irrig_system))
  }
  else {
    return(total_n.n2o_irrig_sys) 
  }
}


loop_n2o_nha <- function(year, irrig_param) {
  # loops around the specified main irrigation systems
  # calls compute_irrig_n2o_nha_mosaic and writes the direct N2O emissions to the subfolder "N2O_N"
  # unit: kg N-N2O/LU ha
  
  options(warning=-1)
  # select either the main or individual irrig systems to loop
  ifelse(irrig_param=='all_main',
         main_irrig <- get_mediterranean_EFs()[, 1],
         main_irrig <- irrig_sys_assumptions_df()[, 2])
  
    sapply(main_irrig, function(x) {
    print(paste0('Working with ', x))
      compute_irrig_n2o_nha_mosaic(year, x, write = TRUE)
    beepr::beep(sound='sword')
  }
  )
}


compute_total_LU_n2o_nha <- function(year, write) {
  # this function sums the mosaics of direct N-N2O emissions of the different irrigation systems
  # unit: kg N-N2O/LU ha
  
  n2oNha_folder <- create_n2o_subfolders(year = year, n2o_subfolder =  'N2O_N')
  n2oNha_files <- list.files(path = n2oNha_folder, pattern = 'N2O_nha', full.names = TRUE)
  list_n2oNha <- lapply(n2oNha_files, raster)
  
  n2oNha_sum <- do.call('sum', list_n2oNha)
  
  if(write==TRUE) {
    write_n2o_LU(year = year, n2o_subfolder = 'N2O_N', rasterfile = n2oNha_sum, 
                    filename = 'MOSAIC_N2O.N')
  } else {return(n2oNha_sum) }
  
  rm(list=c('list_n2oNha'))
}


compute_n2o_nha_muni <-function(year, write, irrig_param, file_name) {
  # this function sums the total runoff losses (kg N/LU ha) for each municipality
  # because the total N is the same as in the statistical calculations, there is no neeed to apply an adjustment factor
  # output: returns a dataframe with the total runoff N losses, both in kg N and kg N/UAA ha
  
  library(foreach)
  library(doParallel)
  # call runoff_nha mosaic
  
  if(missing(irrig_param)==TRUE) {
         irrig_param <- 'MOSAIC'
  }
  n2oNha_folder <- create_n2o_subfolders(year = year, n2o_subfolder =  'N2O_N')
  n2o_mosaic <- raster(list.files(path = n2oNha_folder, pattern = irrig_param, full.names = TRUE))
  
  muni_df <- create_main_csv()
  muni_shp <- get_muni_shp()
  
  registerDoParallel(cores=3)
  #getDoParWorkers()
  
  print('Starting parallel processing ...')
  n2o_irrig <- foreach(i=1:278, .packages=c('raster', 'rgdal'), .combine=rbind) %dopar% {
    j <- muni_df[i,1]
    print(paste0('Working with the municipality ID ', j))
    sb <- subset(muni_shp, Muni_ID==j)
    r_mosaic <- crop(n2o_mosaic, sb)
    r_mask <- mask(r_mosaic, sb)
    #compute total N in runoff N losses for a municipality
    n2o_kgN <- round(cellStats(r_mask, 'sum'), 0)
    
    muni_id <- which(muni_df$Muni_ID==j)
    muni_df[muni_id, 'n2o_kgN'] <- n2o_kgN
    data.frame(muni_df[i, seq(1,3)], n2o_kgN)
  }
  #unregister parallel processing
  registerDoSEQ()
  
  uaa <- load_uaa(year)
  n2o_irrig$n2o_kgNha <- round(n2o_irrig$n2o_kgN/uaa, 2)
  
  if (write==TRUE) {
    ifelse(missing(file_name)==TRUE,
           file_name <- 'N2O.N_dataset_municipality',
           file_name <- file_name)
    n2o_path <- create_n2o_subfolders(year = year, n2o_subfolder = 'N2O_N')
    filename <- file.path(n2o_path, paste0(file_name, year_prefix(year)))
    write.csv(n2o_irrig, paste0(filename, '.csv'))
  }
  return(n2o_irrig)
  rm(list=c('sb', 'r_mosaic', 'r_mask', 'muni_shp', 'n2o_mosaic'))
}

ind_irrig_N2O_dataset <- function(year) {
  
  main_df <- create_main_csv()
  main_irrig <- irrig_sys_assumptions_df()[, 2]

  main_df <- sapply(main_irrig, function(x) {
    compute_n2o_nha_muni(year = year, write = F, irrig_param = paste0('nha_',x))
    colnames(main_df)[ncol(main_df)] <- x
  })
  return(main_df)
}


