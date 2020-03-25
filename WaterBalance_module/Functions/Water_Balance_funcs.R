source('./GIS_module/Function/LU_irrigation_allocation.R')
source('./GIS_module/Function/General_GIS_functions.R')
source('./GIS_module/Function/PreProcessing/Precipitation_idw.R')

dir.create(path = 'WaterBalance_module')

get_WaterBalance_folder <- function() {
  
  wb_folder <- get_climatic_subfolder('WaterBalance')
  return(wb_folder)
}

get_crop_kc <- function() {
  # gets from the WaterBalance subfolder, the crop-Kc dataframe and reads it
  # source: Chapter 6, FAO 56, Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56
  
  wb_folder <- get_WaterBalance_folder()
  crop_kc <- list.files(wb_folder, pattern='crop_Kc', full.names=TRUE)
  r_crop_kc <- read.csv(crop_kc, sep = ';')
  return(r_crop_kc)
}

## getters ------------------------------------------------------------------------------ 

select_WB_out_folder <- function(subfolder) {
  out_path <- select_module_output(pattern = 'WaterBalance')
  select_subfolder <- list.files(out_path, pattern = subfolder, full.names = TRUE)
  return(select_subfolder)
}

select_WB_subfolder_file <- function(subfolder, file, year) {
  
  out_subfolder <- select_WB_out_folder(subfolder)
  select_file <- list.files(out_subfolder, pattern=file, full.names=T)
  select_year <-select_file[which(grepl(year_prefix(year), select_file)==TRUE)]
  return(select_year)
}

## ------------------------------------------------------------------------------------- 


set_wb_output_path <- function(output_folder) {
  
  wb_path <- './WaterBalance_module/'
  out_path <- file.path(wb_path, 'Output')
  dir.create(path = out_path, showWarnings = FALSE)
  select_out_path <- file.path(out_path, output_folder)
  dir.create(path = select_out_path, showWarnings = FALSE)
  return(select_out_path)
}

write_wb_output <- function(file, filename, output_folder) {
  
  out <- set_wb_output_path(output_folder = output_folder)
  filename <- file.path(out, filename)
  
  if (class(file)=='RasterLayer') {
    name <- paste0(filename, '.tif')
    tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
    writeRaster(x = file, filename = name, options=tifoptions)  
  } else if (class(file)=='data.frame') {
    filename <- paste0(filename, '.csv')
    write.csv(file, filename, row.names = F)
  }
}


## -----------------------------------------------------------------------------------------------------
## COMPUTE NEW CROP KC BASED ON WEIGHTED AVERAGE
## -----------------------------------------------------------------------------------------------------

correct_avg_wght_allocation <- function() {
  #weighted average Kc: weight allocation 40% kc_ini, 30% kc_mid, 30% kc_end
  # reconstructs the allocation weights to calculate the weighted average Kc
  # if kc_ini is NA (ie doesn't exist) recalculate weights
  # output: crop_kc dataframe with corrected weights
  
  allocation_kc <- data.frame(kc=c('kc_ini', 'kc_mid', 'kc_end'),
                              wghts = c(0.4, 0.3, 0.3))
  crop_kc <- get_crop_kc()
  
  #identify rows where kc_ini does not exist
  na_idx <- which(is.na(crop_kc[, 'kc_ini'])==TRUE)
  
  crop_kc[na_idx, c('kc_mid', 'kc_end')] <- 0.5
  crop_kc[-na_idx, 'kc_ini'] <- 0.4
  crop_kc[-na_idx, c('kc_mid', 'kc_end')] <- 0.3
  
  return(crop_kc)
}

compute_avg_wght_crop_kc <- function() {
  # calculates the weighted average Nc
  # this is used to calculate the potential evapotranspiration (ET_kc) during the summer
  
  allocation_kc <- correct_avg_wght_allocation()
  crop_kc <- get_crop_kc()
  
  for (i in 3:ncol(crop_kc)) {
    crop_kc[, i] <- crop_kc[, i] * allocation_kc[, i]
  }
  crop_kc <- data_cleaning(crop_kc)
  crop_kc$new_kc <- rowSums(crop_kc[, c(3,4,5)])
  return(crop_kc)
}

## -----------------------------------------------------------------------------------------------------
## COMPUTE SEASONAL REFERENCE POTENTIAL EVAPOTRANSPIRATION (ET0) BASED ON PROF ROSÁRIO CALCULATIONS 
## -----------------------------------------------------------------------------------------------------

get_r_evapo <- function(year) {
  
  et0_folder <- evapo_folder()
  r_evapo <- raster(
    list.files(et0_folder, pattern=paste0('rast_e', year_prefix(year)), full.names = TRUE))
  return(r_evapo)
}

compute_seasonal_ET0 <- function(year) {
  #Jan - Mar 11% ;; Apr - Set 77% ;; Oct-Dec 12% (annual)
  
  season_et0 <- data.frame(season=c('winter', 'summer', 'autumn'), prop = c(0.11, 0.77, 0.12))
  r_evapo <- get_r_evapo(year)
  
  for (i in 1:nrow(season_et0)) {
    r_season <- season_et0[i, 2]*r_evapo
    write_wb_output(file = r_season, filename = paste0('ET0_', season_et0[i, 1], year_prefix(year)), output_folder = 'Seasonal_ET0')
  }
}

loop_seasonal_ET0 <- function() {
  year <- c(1999, 2009)
  sapply(year, compute_seasonal_ET0)
}

## -----------------------------------------------------------------------------------------------------
## COMPUTE SEASONAL PRECIPITATION BASED ON ET0 SEASONS
## -----------------------------------------------------------------------------------------------------

# winter - Jan - Mar ;; spring/summer - Apr - Sept ;; autumn - Oct - Dec 

seasonal_prec_division <- function() {
  season_prec <- data.frame(
    season=c('winter', 'winter', 'winter', 'summer', 'summer','summer', 'summer', 'summer', 'autumn', 'autumn', 'autumn'),
    month = c('Jan', 'Feb', 'Mar', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'))
  return(season_prec)
}

compute_seasonal_prec_df <- function(year) {
  # computes seasonal sum of precipitation 
  
  prec_season_df <- seasonal_prec_division()
  season <- c('winter', 'summer', 'autumn')
  prec_data <- get_idw_stations(year = year, idw_source = 'precipitation')
  
  for (i in season) {
    print(paste0('Working with prec data from ', i))
    #gives month to loop
    select_months <- prec_season_df[which(prec_season_df$season==i), 2]
    idx <- sapply(seq(1,3), function(x) which(names(prec_data)==select_months[x]))
    prec_df <- prec_data[, c(1,2,3, idx)]
    prec_df$sum <- rowSums(prec_df[, seq(4, ncol(prec_df))])
    write_wb_output(file = prec_df, filename = paste0('prec_', i, year_prefix(year)), output_folder = 'Seasonal_prec_df')
  }
}

loop_seasonal_prec_df <- function() {
  year <- c(1999, 2009)
  sapply(year, function(x) compute_seasonal_prec_df(x))
}

georeference_seasonal_stations <- function(year, season) {
  
  seasonal_stations <- read.csv(select_WB_subfolder_file(subfolder = 'Seasonal_prec_df', file = season, year=year))
  SPdf_seasonal <- georeference_idw_stations(year = year, source = 'naah', df = seasonal_stations)
  return(SPdf_seasonal)
}

interpolate_seasonal_stations <- function(year, season) {
  
  caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  SPdf_seasonal <- georeference_seasonal_stations(year, season)
  idw_seasonal <- interpolate_idw(year = year, source = 'precipitation', SPdf_station = SPdf_seasonal)
  idw_seasonal <- idw_seasonal*caa
  write_wb_output(file = idw_seasonal, filename = paste0('idw_prec_', season, year_prefix(year)), output_folder = 'Seasonal_prec_idw')
  rm(list = c('idw_seasonal', 'caa'))
}


loop_idw_seasonal <- function() {
  year <- 2009
  season <- c('summer', 'autumn', 'winter')
  for (i in year) { 
    for (j in season) {
      interpolate_seasonal_stations(i, j)
    }
  }
  beepr::beep(sound=1)
}

## -----------------------------------------------------------------------------------------------------
## COMPUTE MUNICIPALITY CROP KC AND ALLOCATE IT TO LU
## NOTE: THIS METHOD DISREGARDS MUNICIPALITY CROP PROPORTIONS
## -----------------------------------------------------------------------------------------------------


## ------- COMPUTE IRRIGATED CROP ACREAGE PROPORTION
compute_LU_area <- function(LU_class, year, irrig_eff)  {
  #computes total irrigated areas and water volume of each crop for a LU class
  #returns a list where id =1 concerns the volume and id=2 the areas
  #irrig_eff can be either static, temporal or missing and computes crop water requirements
  #d <- compute_LU_class_volume('olive_groves', 1999, 'static)
  # d <- compute_LU_class_volume('orchards', 2009)
  
  select_LU_crops <- LU_class_allocation(LU_class)
  cereal_condition <- LU_cereal_conditions(LU_class)
  
  df_vol <- create_main_csv()
  df_irrig <- df_vol
  
  for (a in select_LU_crops)  {
    ifelse(a=='cereals', crops <- cereal_condition, crops <- get_crop_names(2009, a))
    
    for (b in crops)  {
      #load irrig area
      df_irrig <- general_func_volume_area_crop(crop_func = get_irrig_areas, year=year, main_crop=a, crop=b, calc_df=df_irrig, LU_class = LU_class)
    }
  }
  #sum every col or change name incase only 4 cols are available (e..g olive grove, vineyard)
  ifelse(ncol(df_irrig)>4, df_irrig$sum <- rowSums(df_irrig[, seq(4, ncol(df_irrig))]), colnames(df_irrig)[4] <- 'sum')
  return(df_irrig)
}

correct_orchards_area <- function(year) {
  # this aggregates each maincrop crops' within the orchards datafrmae
  # i.e. dried fruits are summed and aggregated into only one maincrop cat; same applies to citrus and fresh fruits

  orchards_crop_df <- compute_LU_area('orchards', year)
  orchards_maincrops <- LU_class_allocation('orchards')
  calc_df <- create_main_csv()
  
  for (i in orchards_maincrops) {
    crops <- get_crop_names(2009, i)
    sum_maincrop <- rowSums(orchards_crop_df[, crops])
    calc_df <- cbind(calc_df, sum_maincrop)
    colnames(calc_df)[ncol(calc_df)] <- i
  }
  calc_df$sum <- rowSums(calc_df[, seq(4, ncol(calc_df))])
  return(calc_df)
}

correct_heterogeneous_vineyard.olive <- function(df) {
  # lazy function --> corrects the colnames of the hterogeneous allocation df in accordance to crop_Kc
  olive_id <- which(names(df)=='olive grove')
  vine_id <- which(names(df)=='vineyard')
  
  colnames(df)[olive_id] <- 'olive_groves'
  colnames(df)[vine_id] <- 'vineyards'
  return(df)
}

compute_LU_area_proportions <- function(LU_class, year) {
  # this computes crop proportion comparatively to total LU area (statistics)
  # crop names are defined accordingly to crop kc dataframe
  
  ifelse(LU_class=='orchards',
    lu_crops <- correct_orchards_area(year),
    lu_crops <- compute_LU_area(LU_class, year))
  
  if (ncol(lu_crops)==4) {
    lu_crops$sum  <- lu_crops$sum/lu_crops$sum
  }
  else {
    for (i in 4:(ncol(lu_crops)-1)) {
      lu_crops[, i] <- round(lu_crops[, i] / lu_crops[, ncol(lu_crops)], 2)
    }
  }
  
  if (LU_class=='heterogeneous') {
    lu_crops <- correct_heterogeneous_vineyard.olive(lu_crops)
  }
  
  lu_crops <- data_cleaning(lu_crops)
  ifelse(ncol(lu_crops)==4,
         return(lu_crops),
         return(lu_crops[, -ncol(lu_crops)]))
}


compute_LU_crop_kc <- function(year) {
  # generates a new kc for each LU based on its crop acreage proportion and respective ET0_kc
  # output is a dataframe for each LU with the respective crop kc corrected by its area proportion
  
  crop_kc <- compute_avg_wght_crop_kc()
  lu <- loop_LU_classes()
  for (i in lu) {
    
    print(paste0('Working with ', i))
    lu_prop <- compute_LU_area_proportions(LU_class = i, year = year)
    
    if (ncol(lu_prop)==4) {
      set_crop_kc <- crop_kc[which(crop_kc$crop==i), ncol(crop_kc)]
      lu_prop[, 4] <- lu_prop[, 4] * set_crop_kc
      colnames(lu_prop)[4] <- 'new_lu_kc'
      write_wb_output(file = lu_prop, filename = paste0(i, '_ETkc', year_prefix(year)), output_folder = 'LU_ET_kc_df')
    }
    else {
      for (j in 4:ncol(lu_prop)) {
        crop_name <- colnames(lu_prop)[j]
        set_crop_kc <- crop_kc[which(crop_kc$crop==crop_name), ncol(crop_kc)]
        lu_prop[, j] <- lu_prop[, j] * set_crop_kc
      }
      lu_prop$new_lu_kc <- rowSums(lu_prop[, seq(4, ncol(lu_prop))])
      write_wb_output(file = lu_prop, filename = paste0(i, '_ETkc', year_prefix(year)), output_folder = 'LU_ET_kc_df')
    }
  }
}

spatial_allocation_LU_kc <- function(year) {
  # LU-specific kc are rasterized at the municipality level and geo-referenced against the respective LU GIS map
  
  lu <- loop_LU_classes()
  for (i in lu) {
    get_lu_kc <- read.csv(select_WB_subfolder_file(subfolder = 'LU_ET_kc_df', file = i, year = year))
    get_lu_kc <- get_lu_kc[, c('Muni_ID', 'new_lu_kc')]
    r_lu_kc <- rasterize_data_muni(get_lu_kc, 'Muni_ID', 'new_lu_kc')
    LU_GIS <- get_LU_class_raster(i, year)
    spatial_lu_kc <- LU_GIS*r_lu_kc
    write_wb_output(file = spatial_lu_kc, filename = paste0(i, '_ETkc', year_prefix(year)), output_folder = 'LU_ET_kc_GIS')
  }
}

create_mosaic_LU_kc <- function(year) {
  # creates a raster mosaic based on the output for each LU kc of spatial_allocation_LU_kc
  
  lu <- loop_LU_classes()
  r_lu_list <- lapply(lu, function(x) raster(select_WB_subfolder_file('LU_ET_kc_GIS', x, year)))
  r_lu_list$fun <- sum
  r_mosaic <- do.call(mosaic, r_lu_list)
  write_wb_output(file = r_mosaic, filename = paste0('MOSAIC_ETkc', year_prefix(year)), output_folder = 'LU_ET_kc_GIS')
}

compute_summer_ETkc_LU <- function(year) {
  
  mosaic_lu_kc <- raster(select_WB_subfolder_file('LU_ET_kc_GIS', 'MOSAIC', year))
  summer_et0 <- raster(select_WB_subfolder_file('Seasonal_ET0', 'summer', year))
  
  summer_etkc <-  summer_et0 * mosaic_lu_kc
  write_wb_output(file = summer_etkc, filename = paste0('summer_ET_kc', year_prefix(year)), output_folder = 'summer_ET_kc')
  rm(list=c('mosaic_lu_kc', 'summer_et0'))
}

export_gross_irrig_vol <- function(year) {
  # exports from GIS irrigation submodule to WB
  # unit: m3/ha
  g_irrig <- raster(get_irrig_LU_data(year = year, folder_name = 'Volumes', path = 'gross_irrigation', 'mosaic'))
  write_wb_output(file = g_irrig, filename = paste0('Gross_irrig_vol', year_prefix(year)), output_folder = 'Irrigation')
}


compute_runoff_vol <- function(year, season, r_prec_surplus) {
  # computes runoff water volume losses derived from the precipitation surplus
  # unit: r_prec_surplus must be in m3/ha/yr so runoff losses are in the same unit
  
  f_runoff <- get_GIS_file(file_pattern = paste0('Rf', year_prefix(year)), folder_name = 'MITERRA_fractions')
  runoff_vol <- f_runoff*r_prec_surplus
  write_wb_output(file = runoff_vol, filename = paste0('Runoff_', season, year_prefix(year)), output_folder = 'Water_surplus')
  rm(list='f_runoff')
  return(runoff_vol)
}

compute_seasonal_water_surplus <- function(year, season) {
  
  #ET and prec data are on mm/year ;; the conversion to m3/ha is 10x mm/year
  #unit: m3/ha(yr)
  
  if (season=='summer') {
    # compute precipitation surplus
    ET_kc <- raster(select_WB_subfolder_file('summer_ET_kc', 'summer_ET_kc', year)) 
    prec <- raster(select_WB_subfolder_file('Seasonal_prec_idw', 'summer', year))
    irrig <- raster(select_WB_subfolder_file('Irrigation', 'Gross_irrig_vol', year))
    prec_surplus <- (prec-ET_kc)*10+irrig
    rm(list=c('ET_kc', 'prec'))
    prec_surplus[prec_surplus<0] <- 0 
    # compute and export runoff losses
    runoff <- compute_runoff_vol(year = year, season = season, r_prec_surplus = prec_surplus)
    # compute and export water balance
    water_surplus <- prec_surplus-runoff
  } else {
    # compute precipitation surplus
    ET0 <- raster(select_WB_subfolder_file('Seasonal_ET0', season, year)) 
    prec <- raster(select_WB_subfolder_file('Seasonal_prec_idw', season, year))
    prec_surplus <- (prec-ET0)*10
    rm(list=c('ET0', 'prec'))
    prec_surplus[prec_surplus<0] <- 0 
    # compute and export runoff losses
    runoff <- compute_runoff_vol(year = year, season = season, r_prec_surplus = prec_surplus)
    # compute and export water balance
    water_surplus <- prec_surplus-runoff
  }
  write_wb_output(file = prec_surplus, filename = paste0('PrecSurplus_',season,year_prefix(year)), output_folder = 'Water_surplus')
  write_wb_output(file = water_surplus, filename = paste0('WaterSurplus_',season,year_prefix(year)), output_folder = 'Water_surplus')
  rm(list=c('runoff', 'water_surplus'))
}

loop_seasonal_water_surplus <- function() {
  year <- c(1999, 2009)
  season <- c('winter', 'summer', 'autumn')
  for (i in year) {
    for (j in season) {
      compute_seasonal_water_surplus(i, j)
    }
  }
}

construct_WaterBalance_mosaic_annual <- function(year) {
  # unit: m3/ha
  
  caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  r_WS_list <- lapply(select_WB_subfolder_file(subfolder = 'Water_surplus', file = 'WaterSurplus', year = year), function(x) raster(x))
  r_WS_list$fun <- sum
  r_mosaic <- do.call(mosaic, r_WS_list)
  r_mosaic <- r_mosaic * caa
  write_wb_output(file = r_mosaic, filename = paste0('MOSAIC_WS',year_prefix(year)), output_folder = 'Water_surplus')
}
