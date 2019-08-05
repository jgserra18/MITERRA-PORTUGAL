source('./Irrigation_module/Functions/Global_irrigation_functions.R')
source('./GIS_module/Function/General_GIS_functions.R')
source('./Irrigation_module/Functions/Compute_crop_irrigatioN.R')
source('./GIS_module/Function/GW_computation_functions.R')

loop_LU_classes <- function()
{
  classes <- c('perma_irrigated', 'non', 'olive_groves', 'orchards', 'pastures', 'rice', 'vineyards', 'heterogeneous')
  return(classes)
}

LU_crop_list <- function () {
  
  df <- list(winter_cereals = c('cereals'),
             pastures = c('pastures'),
             orchards = c('dried fruits', 'fresh fruits', 'citrus'),
             olive_groves = c('olive grove'),
             vineyards = c('vineyard'),
             rice = c('cereals'),
             irrigated_crops = c('forage', 'pulses', 'industry', 'horticulture', 'potato', 'cereals'),
             heterogeneous = c('forage', 'pulses', 'industry', 'horticulture', 'potato', 
                               'cereals', 'olive grove', 'vineyard', 'dried fruits', 'fresh fruits',
                               'citrus'))
  return(df)
}

#define LU categories
LU_classes <- function(crop_class){
  #define LU categories
  
  df <- LU_crop_list()
  return(df[[crop_class]])
}

get_LU_class_raster <- function(LU_class, year)
{
  #gets LU class from CLC
  # d <- get_LU_class_raster('rice', 1999)
  
  LU_folder <- select_GIS_output_submodule('LandUse')
  LU_year <- file.path(LU_folder, year)
  LU_select <- file.path(LU_year, list.files(LU_year, pattern =LU_class))
  
  LU_raster <- raster(LU_select)
  return(LU_raster)
}


LU_class_allocation <- function(LU_class)
{
  #allocates LU class to different crops
  
  df <- data.frame(perma_irrigated='irrigated_crops',
             non = 'winter_cereals',
             olive_groves = 'olive_groves',
             orchards = 'orchards',
             pastures = 'pastures',
             rice = 'rice',
             vineyards = 'vineyards',
             heterogeneous = 'heterogeneous')
  
  crop_class <- df[[LU_class]]
  crop_classes <- LU_classes(as.character(crop_class))
  return(crop_classes)
}


LU_cereal_conditions <- function(LU_class)
{
  condition <- list(non=c('rye', 'wheat', 'barley', 'triticale'),
                    perma_irrigated=c('oat', 'maize'),
                    rice='rice',
                    heterogeneous=c('oat', 'maize'))
  return(condition[[LU_class]])
}

## --------------------------------------------------------------------------------
## DEFINE ASSUMPTIONS FOR HETEROGENEOUS AREAS -------------------------------------
## --------------------------------------------------------------------------------

set_assumptions_heterogeneous <- function() {
  ### MAIN ASSUMPTIONS PER NUTS 2 -----------------------------------------------------------------------------------------------
  ## AML --> 20% PERMANENT CROPS ;; 15% IRRIGATED TEMPORARY CROPS
  ## ALENTEJO --> ALL RAINFED AS THESE ARE NOT PREPONDERANTE
  ## ALGARVE --> 25% ORCHARDS ;; 25% HORTICULTURE/VEGETABLES
  ## CENTRO --> SAME DISTRIBUTION AS AML
  ## NORTH --> 20% IRRIGATED TEMPORARY CROPS ;; 20% PERMANENT CROPS ;; 30% VINEYARDS
  
  nuts2_ID <- c(15, 16, 11, 17, 18)
  nuts2_names <- c('Algarve', 'Centro', 'Norte', 'AML', 'Alentejo')
  perma_irrigated <- c(0.25, 0.15, 0.20, 0.15, 0)
  orchards <- c(0.25, 0.2, 0.2, 0.2, 0)
  vineyards <- c(0, 0.2, 0.3, 0.2, 0)
  olive_groves <- c(0, 0.2, 0.2, 0.2, 0)
  
  assumption_df <- data.frame(nuts2_ID, nuts2_names, perma_irrigated, orchards, vineyards, olive_groves)
  return(assumption_df)
}


create_LU_distribution_df <- function() {
  # creates a dataframe with the different municiaplities but disaggregated per NUTS2
  # used to allocate different LU areas to heterogeneous areas
  # to be applied only in perma_irrigated or pemrnanet crops
  template_df <- create_main_csv()
  main_df <- call_spatial_disaggregation()[, -2]
  assumption_df <- set_assumptions_heterogeneous()[, -2]
  main_df <- merge(main_df, assumption_df, 'nuts2_ID', sort=F)
  main_df <- merge(template_df, main_df, 'Muni_ID', sort=F)
  #main_df <- main_df[order(main_df$Full_ID), ]
  return(main_df[, -2])
}


condition_LU_heterogeneous_df <- function(LU_class) {
  # modifies create_LU_distribution_df
  # to be applied only in heterogeneous areas
  
  df <- create_LU_distribution_df()
  
  if(LU_class == 'perma_irrigated' |LU_class == 'orchards' |LU_class == 'olive_groves' |LU_class == 'vineyards') {
    df <- 1-df[, seq(7, ncol(df))]
    df <- df[, LU_class]
  } 
  else if (LU_class == 'heterogeneous') {
    df <- df
  }
  else {
    df <- 1
  }
  return(df)
}


LU_heterogeneous_crop_allocation <- function (main_crop) {
  # this function is to be applied when calculating anything related to main crops FOR HETEROGENEOUS
  # it checks whether a main crop has to be allocated to heterogeneous areas
  # if so, it uses the allocation factor and  distributes it to the crop
  
  main_df <- create_LU_distribution_df()
  lu_list_heterogeneous <- LU_crop_list()[c('irrigated_crops','orchards', 'vineyards', 'olive_groves')]
  
  id_condition_lu <- which(grepl(main_crop, lu_list_heterogeneous)==TRUE)+6 #this is to match the columns from condition_LU_heterogeneous_df
  df <- main_df[, id_condition_lu]
  return(df)
}

#IMPORTANT
compute_LU_condition <- function(computation_df, LU_class, main_crop) {
  # this function is the general function to allocate anything (e.g. areas, volumes) to heterogeneous areas
  # while it modifies the affected LU classes based on the allocation value
  
  ifelse(LU_class!='heterogeneous', modify_df <- condition_LU_heterogeneous_df(LU_class),
         modify_df <- LU_heterogeneous_crop_allocation(main_crop))
  
  ifelse(ncol(computation_df)>4, 
         computation_df[, seq(4, ncol(computation_df))] <-  sapply(computation_df[, seq(4, ncol(computation_df))], function(x) x*modify_df),
         computation_df[, 4] <- sapply(computation_df[, 4], function(x) x*modify_df)
         )
  
  return(computation_df)
}

## ----------------------------------------------------------------------------------------------------------------------------

compute_net_crop_water_reqs <- function(crop_func_df, irrig_eff, year, LU_class) {
  #this function is ought to be implemented in general_func_volume_area_crop
  #irrig eff can be either static (default) or temporal
  
  #load conditions ----------------------------------------------------------
  if (irrig_eff=='static')
  {
    irrig_eff <- get_irrig_sys_efficiency()
  }
  else if (irrig_eff=='temporal')
  {
    irrig_eff <- get_temporal_irrig_sys_efficiency()
    #subset temporal eff based on year
    ifelse(year==1999, irrig_eff <- irrig_eff[, -3], irrig_eff <- irrig_eff[,-2])
  }
  
  #computation
  irrig_sys_names <- colnames(crop_func_df)[seq(4, ncol(crop_func_df))]
  
  for (i in irrig_sys_names)
  {
    irrig_effs <- irrig_eff[which(irrig_eff$irrig_system==i), 2]
    crop_func_df[, i] <- round(crop_func_df[, i]*irrig_effs, 0)
  }
  return(crop_func_df)
}


general_func_volume_area_crop <- function(crop_func, year, main_crop, crop, calc_df, irrig_eff, LU_class)
{
  #note crop_func is either get_crop_water_volume or get_irrig_areas
  
    crop_df <- crop_func(main_crop=main_crop, crop=crop, year=year) 
    new_crop_df <- compute_LU_condition(computation_df = crop_df, LU_class = LU_class, main_crop = main_crop)
    
    if (missing(irrig_eff)==FALSE)
    {
      new_crop_df <- compute_net_crop_water_reqs(new_crop_df, irrig_eff, year)
    }
    
    new_crop_df$sum <- rowSums(new_crop_df[, seq(4, ncol(new_crop_df))])
    calc_df <- cbind(calc_df, new_crop_df$sum)
    colnames(calc_df)[ncol(calc_df)] <- as.character(crop)   

    return(calc_df)
}

compute_LU_class_volume <- function(LU_class, year, irrig_eff)
{
  #computes total irrigated areas and water volume of each crop for a LU class
  #returns a list where id =1 concerns the volume and id=2 the areas
  #irrig_eff can be either static, temporal or missing and computes crop water requirements
  #d <- compute_LU_class_volume('olive_groves', 1999, 'static)
  # d <- compute_LU_class_volume('orchards', 2009)
  select_LU_crops <- LU_class_allocation(LU_class)
  cereal_condition <- LU_cereal_conditions(LU_class)
  
  df_vol <- create_main_csv()
  df_irrig <- df_vol
  
  for (a in select_LU_crops)
  {
    ifelse(a=='cereals', crops <- cereal_condition, crops <- get_crop_names(2009, a))
    
    for (b in crops)
    {
      #load volume
      
      df_vol <- general_func_volume_area_crop(crop_func = get_crop_water_volume, year=year, main_crop=a, crop=b, calc_df=df_vol, 
                                              irrig_eff = irrig_eff, LU_class = LU_class)
      #load irrig area
      df_irrig <- general_func_volume_area_crop(crop_func = get_irrig_areas, year=year, main_crop=a, crop=b, calc_df=df_irrig, LU_class = LU_class)
    }
  }
  #sum every col or change name incase only 4 cols are available (e..g olive grove, vineyard)
  ifelse(ncol(df_vol)>4, df_vol$sum <- rowSums(df_vol[, seq(4, ncol(df_vol))]), colnames(df_vol)[4] <- 'sum')
  ifelse(ncol(df_irrig)>4, df_irrig$sum <- rowSums(df_irrig[, seq(4, ncol(df_irrig))]),colnames(df_irrig)[4] <- 'sum')
  return(list(df_vol, df_irrig))
}

compute_tot_temporary_irrig_areas <- function(year)
{
  #compute total sum of temporary irrigated crop areas (minus winter cereals)
  
  temp_crops <- c('cereals', 'forage', 'horticulture', 'industry', 'potato', 'pulses')
  calc_df <- create_main_csv()
  df <- create_main_csv()
  
  for (a in temp_crops)
  {
    crops <- get_crop_names(2009, a)
    
    ifelse(a=='cereals', crops <- subset(crops, crops=='maize' | crops=='oat'), crops <- get_crop_names(2009, a))
    
    for (b in crops)
    {
      crop_sum <- general_func_volume_area_crop(crop_func = get_irrig_areas, year=year, 
                                               main_crop=a, crop=b, calc_df=df)
      calc_df <- cbind(calc_df, crop_sum[, 4]) #last col with total crop area
      colnames(calc_df)[ncol(calc_df)] <- b
    }
    
  }
 calc_df$sum <- round(rowSums(calc_df[, seq(4, ncol(calc_df))]), 0)
  return(calc_df)
}


compute_LU_m3_ha <- function(LU_class, year, irrig_eff)
{
  #aggregates into one dataset the sum of volume, irrigated areas and m3/ha for a LU class
  #d <- compute_LU_m3_ha('olive_groves', 1999)
  #note for the future: this can be generalized
  
  crop_vol <- compute_LU_class_volume(LU_class, year, irrig_eff)[[1]]
  crop_area <- compute_LU_class_volume(LU_class, year)[[2]]
  
  summary_df <- create_main_csv()
  summary_df$sum_vol <- crop_vol$sum
  summary_df$sum_area <- crop_area$sum
  summary_df$m3_ha <- round(summary_df$sum_vol/summary_df$sum_area, 0)
  summary_df  <- data_cleaning(summary_df)
  return(summary_df)
}


#this function computes the m3/ha of irrigation volume per total area of LU 
compute_LU_m3_ha_allocation <- function(LU_class, year, irrig_eff)
{
  crop_vol <- compute_LU_class_volume(LU_class, year, irrig_eff)[[1]]
  crop_class <- LU_class_allocation(LU_class)
  cereal_condition <- LU_cereal_conditions(LU_class)
  
  calc_df <- create_main_csv()
  area_df <- create_main_csv()
  
  for (a in crop_class)
  {
    ifelse(a=='cereals', crops <- cereal_condition, crops <- get_crop_names(2009, a))
    
    for (b in crops)
    {
      tot_crop_a <- get_raw_crop_areas(year, a, b)
      area_df <- cbind(area_df, tot_crop_a[, ncol(tot_crop_a)])
      colnames(area_df)[ncol(area_df)]  <- b
    }
  }

  ifelse(ncol(area_df)>4, area_df$sum <- rowSums(area_df[, seq(4, ncol(area_df))]), colnames(area_df)[4] <- 'sum')
  area_df$sum <- as.integer(area_df$sum)
  calc_df$m3_tot_ha <- round(crop_vol$sum/area_df$sum, 0)
  calc_df <- data_cleaning(calc_df)
  return(calc_df)
}


### GET AND WRITE FUNCTIONS ----------------------------------------------------------------------------------
get_irrig_LU_outputfolder <- function(year, folder_name)
{
  irrig_folder <- raster_modelling_subfolders('Irrigation')
  select_yr <- file.path(irrig_folder, year)
  select_folder <- file.path(select_yr, list.files(select_yr, pattern=folder_name))
  return(select_folder)
}

get_irrig_LU_data <- function(year, folder_name, path, file_pattern) {
  #gets data from subfolders
  #get_irrig_LU_data(1999, 'Volumes', 'gross_irrigation', 'vineyards')
  
  irrig_output <- get_irrig_LU_outputfolder(year, folder_name)
  irrig_path <- list.files(irrig_output, full.names = T, pattern = path)
  
  ifelse(missing(file_pattern)==FALSE,
         file_path <- list.files(irrig_path, full.names = T, pattern = file_pattern),
         file_path <- irrig_path)

  return(file_path)
}

general_LU_irrig_dir <- function(year, folder_name) {
  # this function checks whether a directory named 'folder_name' exists
  # if not it creates it
  
  irrig_output <- get_irrig_LU_outputfolder(year, folder_name)
  
  if(identical(irrig_output, character(0))==TRUE) {
    
    irrig_folder <- raster_modelling_subfolders('Irrigation')
    select_yr <- file.path(irrig_folder, year)
    fullpath <- file.path(select_yr, folder_name)
    dir.create(path = fullpath)
  }
}

create_dir_volumes_path <- function(year, folder_name, path) {
  # if missing, path is gross irrigation, if static or temporal path is net irrigation, 
  # if runoff path is runoff, if leaching path is leaching
  
  volume_folder <- get_irrig_LU_outputfolder(year, folder_name)
  
  #load conditions
  df_conditions <- data.frame(path=c('missing', 'static', 'temporal', 'runoff', 'leaching'),
                              path_name = c('gross_irrigation', 'net_irrigation', 'net_irrigation', 'runoff', 'leaching'))
  #path_name conditions
  ifelse(missing(path)==TRUE,
    path.name <- df_conditions[which(df_conditions$path=='missing'), 2],
    path.name <- df_conditions[which(df_conditions$path==path), 2])
  
  path <- file.path(volume_folder, path.name)
  #path <- paste0(volume_folder, path.name)
  
  dir.create(path=path)
  return(path)
}

write_irrig_LU <- function(year, folder_name, irrig_eff, rasterfile, filename) {
  
  path <- create_dir_volumes_path(year, folder_name, irrig_eff)
  output_folder <- file.path(path, filename)
  tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
  writeRaster(rasterfile, output_folder, options=tifoptions, format='GTiff')
}
### ----------------------------------------------------------------------------------------------------------

allocate_LU_volume_GIS <- function(year, irrig_eff) {
  #allocates LU volumes in m3/ha of each LU class to CLC class
  
  LU_classes <- loop_LU_classes()
  if(missing(irrig_eff)==TRUE) {
    path <- create_dir_volumes_path(year, 'Volumes', 'missing')
    name <- 'LU_vol_'
  } 
  else if (irrig_eff=='static' || irrig_eff =='temporal'){
    path <- create_dir_volumes_path(year, 'Volumes', irrig_eff)
    name <- paste0(irrig_eff, '_LU_vol_')
  }
  
  for (i in LU_classes)
  {
    print(paste0('Working with ', i))
    df_lu_vol <- compute_LU_m3_ha_allocation(i, year, irrig_eff)
    r_lu <- rasterize_data_muni(df_lu_vol, 'Muni_ID', 'm3_tot_ha')
    LU_GIS <- get_LU_class_raster(i, year)
    LU_vol <- LU_GIS*r_lu
    print(paste0('Writing ....', i))
    write_irrig_LU(year, 'Volumes', irrig_eff, LU_vol, paste0(name, i))
  }
}


list_LU_GIS <- function(year, folder_name, subfolder, file_pattern) {
  #this creates a list with every LU raster for a given year
  #e.g. list_LU_GIS(1999, 'Volumes', 'gross_irrigation', 'LU_vol_')
  
  irrig_data <- get_irrig_LU_data(year, folder_name, path=subfolder, file_pattern = file_pattern)
  r_list <- list()
  
  for (i in irrig_data) {
     lu_raster <- raster(i)
     #createe raster list
     r_list <- append(r_list, lu_raster)
  }
  return(r_list)
}


map_LU_GIS <- function(year, folder_name, subfolder, file_pattern, write) {
  #creates a mosaic raster for a specified irrigation path (e.g. runoff, missing, static)
  #output is a raster mosaic for different LU classes for the mainland
  #map_LU_GIS(1999, 'Volumes', 'runoff', 'LU_vol', TRUE)
  caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  if (subfolder=='missing') {subfolder <- 'gross_irrigation'}
  r_list <- list_LU_GIS(year, folder_name, subfolder, file_pattern)
  r_list$fun <- sum

  r_mosaic <- do.call(raster::mosaic,r_list)
  r_mosaic_caa <- r_mosaic*caa

  if(write==TRUE) {
    if (subfolder=='gross_irrigation') {subfolder <- 'missing'}
    write_irrig_LU(year = year, folder_name = folder_name, 
                   irrig_eff = file_pattern, rasterfile = r_mosaic_caa, filename = paste0('LU_mosaic_', file_pattern))
  }
}


loop_man_gross_vol_LU_GIS <- function() {
  
  year <- c(1999, 2009)
  for (i in year) {
    print(paste0('Printing ', j, ' for ', i))
    map_LU_GIS(i, 'Volumes', 'gross_irrigation', 'LU_mosaic_', T)
    
  }
}

loop_map_net_vol_LU_GIS <- function() {
  #loops in creating a mosaic of all LU_class crop water requirements 
  #note: only applied to net 
  
  year <- c(1999, 2009)
  cond <- c('static', 'temporal')
  
  for (i in year)
  {
    for (j in cond)
    {
      print(paste0('Printing ', j, ' for ', i))
      map_LU_GIS(i, 'Volumes', 'net_irrigation', j, T)
    }
  }
}

## ---------------------------------------------------------------------------------------------------------
## Compute runoff volume per LU class
## ---------------------------------------------------------------------------------------------------------

compute_LU_runoff_vol <- function(year) {
  #Computes generic runoff losses without taking account different irrig systems
  #The formula used in this approac is the generic formula:
  #Runoff_irrig_vol = f_runoff * Gross_irrig_vol
  
  gross_irrig_files <- get_irrig_LU_data(year, 'Volumes', 'gross_irrigation', 'LU_vol_')
  f_runoff <- get_GIS_file(file_pattern = paste0('Rf', year_prefix(year)), folder_name = 'MITERRA_fractions')
  create_dir_volumes_path(year = year, folder_name = 'Volumes', path = 'runoff')
  
  for (i in gross_irrig_files)
  {
    name <- gsub(paste0('./GIS_module/Output//Modelling/Irrigation/', as.character(year), '/Volumes/gross_irrigation/'), '', i)
    name <- gsub('.tif', '', name)
    print(paste0('Working with ', name))
    
    gross_irrig <- raster(i)
    irrig_runoff <- f_runoff*gross_irrig
    print('Writing ....')
    write_irrig_LU(year = year, folder_name = 'Volumes', irrig_eff = 'runoff', rasterfile = irrig_runoff, filename = name)
  }
}

LU_runoff_vol_mosaic <- function(year) {
  #creates a mosaic for runoff water volume losses
  map_LU_GIS(year, 'Volumes', 'runoff', 'LU_vol', write = T)
}

## ---------------------------------------------------------------------------------------------------------
## Compute water lost through percolation
## ---------------------------------------------------------------------------------------------------------

LU_percolation_net_condition <- function(mosaic_path, irrig_eff) {
  #this is a specific condition for LU net water requirements related to irrigation efficiencies
  #by default the irrigation efficiencies are static, but the user can specify otherwise
  #e.g. LU_percolation_net_condition(mosaic_path, 'temporal')
  # NOTE: to be applied in compute_LU_percolation_vol
  
  ifelse(missing(irrig_eff)==TRUE, irrig_eff <- 'static', irrig_eff <- irri_eff)
  
  id_irrig_eff <- which(grepl(irrig_eff, mosaic_path)==TRUE)
  mosaic_path <- mosaic_path[id_irrig_eff]
  return(mosaic_path)
}


compute_LU_percolation_vol <- function(year, irrig_eff) {
  #computes water lost through percolation for each LU class
  #perc = Gross irrig - runoff - net requirement
  #this will be converted to meters as to be used in the leaching risk index
  #1 cell = 1 ha = 10,000 m2
  
  water_path <- c('gross_irrigation', 'runoff', 'net_irrigation')
  r_stack <- raster::stack()
  
  #populate raster stack with gross irrig, runoff and net LU requirements
  for (i in water_path)
  {
    irrig_file <- get_irrig_LU_data(year, 'Volumes', i, 'mosaic')
    if (i==water_path[3]){
      irrig_file <- LU_percolation_net_condition(irrig_file) #default: static
    }
    r_irrig <- raster(irrig_file)
    r_stack <- raster::stack(r_stack, r_irrig)
  }
  #compute water lost through percolation
  perc_m3 <- r_stack[[1]]-r_stack[[2]]-r_stack[[3]]
  perc_m3 <- reclassify(perc_m3, rcl = c(-Inf, 0, 0))#correct minor negative numbers like -0.001
  perc_meter <- perc_m3/10000 
  
  write_irrig_LU(year = year, folder_name = 'Volumes', irrig_eff = 'leaching', rasterfile = perc_m3, filename ='LU_percolation_vol_m3')
  write_irrig_LU(year = year, folder_name = 'Volumes', irrig_eff = 'leaching', rasterfile = perc_meter, filename ='LU_percolation_vol_meter')
}


## ---------------------------------------------------------------------------------------------------------
## Compute irrigation N per LU class
## ---------------------------------------------------------------------------------------------------------

compute_cropN_LU <- function(year, LU_class) {
  ## this function computes the total N from irrigation for each crop and aggregates it
  ## into each LU class e.g. allocate_cropN_LU(1999, 'rice')
  
  crop_class <- LU_class_allocation(LU_class)
  cereal_condition <- LU_cereal_conditions(LU_class)
  calc_df <- create_main_csv()
  
  for (a in crop_class)
  {
    ifelse(a=='cereals', crops <- cereal_condition, crops <- get_crop_names(2009, a))
    
    for (b in crops) {
      crop_irrgn <- get_crop_irrigatioN(year = year, individual_crop = FALSE, maincrop = a, crop = b)
      ifelse(ncol(crop_irrgn)>4, calc_df$add <- crop_irrgn[, b], calc_df$add <- crop_irrgn[, 4])
      colnames(calc_df)[ncol(calc_df)] <- b
    }
  }
  ifelse(ncol(calc_df)>4, 
         calc_df$total_N <- round(rowSums(calc_df[, seq(4, ncol(calc_df))]), 0),
         colnames(calc_df)[4] <- 'total_N')
         
  return(calc_df)
}

compute_maincropN_hierarchy <- function(year)
{
  ##computes the total main crop N irrigation N and aggregates into one dataset
  ## this function is used to assess which crop is associated with irrigation N flows
  
  main_crop <- get_maincrops_names(2009)
  calc_df <- create_main_csv()
  
  for (a in main_crop) {
    
    crop <- get_crop_names(2009, a)
      crop_N <- get_crop_irrigatioN(year = year, individual_crop = FALSE, maincrop = as.character(a))
      calc_df <- cbind(calc_df, crop_N$sum)
      colnames(calc_df)[ncol(calc_df)] <- a
  }
  return(calc_df)
}

compute_total_irrigN_muni <- function(year) {
  
  ##computes the total crop irrigation N and aggregates into one dataset
  ## this function is used to assess which crop is associated with irrigation N flows
  main_crop <- get_maincrops_names(2009)
  calc_df <- create_main_csv()
  
  for (a in main_crop) {
    
    crop <- get_crop_names(2009, a)
    
    for (b in crop) {

      crop_N <- get_crop_irrigatioN(year = year, individual_crop = TRUE, maincrop = as.character(a), crop = as.character(b))
      calc_df <- cbind(calc_df, crop_N$sum)
      colnames(calc_df)[ncol(calc_df)] <- b
    }
  }
  return(calc_df)
}

compute_cropN_hierarchy <- function(year) {
  ##computes the total crop irrigation N and aggregates into one dataset
  ## this function is used to assess which crop is associated with irrigation N flows
  
  main_crop <- get_maincrops_names(2009)
  calc_df <- data.frame(crop_name='crop_name', crop_N=c(1))
  calc_df$crop_name <- as.character(calc_df$crop_name)
  ctr <- 0
  
  for (a in main_crop) {
    
    crop <- get_crop_names(2009, a)
    
    for (b in crop) {
      
      ctr <- ctr + 1
      crop_N <- get_crop_irrigatioN(year = year, individual_crop = TRUE, maincrop = as.character(a), crop = as.character(b))
      sum <- sum(crop_N$sum)
      calc_df[ctr, 1] <- b
      calc_df[ctr, 2] <- sum
    }
  }
  return(calc_df)
}

compute_cropNha_LU <- function(year, LU_class) {
  # uses allocate_cropN_LU and compute_LU_class_volume to compute 
  # N-input from irrigation in kg N/ha based on Portugal Statistics (1999, 2009)
  
  lu_irrigN <- compute_cropN_LU(year, LU_class)
  lu_area <- compute_LU_class_volume(LU_class, year)[[2]]
  
  lu_irrigN[, ncol(lu_irrigN)] <- round(lu_irrigN[, ncol(lu_irrigN)]/lu_area[, ncol(lu_area)], 0)
  lu_irrigN <- data_cleaning(lu_irrigN)
  return(lu_irrigN)
}

allocate_LU_irrigN <- function(year) {
  # calls compute_cropNha_LU [irrig in kg N/ha]
  # rasterizes these inputs for each LU
  # output is a raster for each LU
  
  LU_classes <- loop_LU_classes()
  general_LU_irrig_dir(year, 'N_irrigation')
  path <- create_dir_volumes_path(year, 'N_irrigation') #by default it creates 'gross irrigation' folder
  
  for (i in LU_classes) {
    
    print(paste0('Working with ', i))
    df_irrign_lu <- compute_cropNha_LU(year, i)
    r_lu <- rasterize_data_muni(df_irrign_lu, 'Muni_ID', 'total_N')
    LU_GIS <- get_LU_class_raster(i, year)
    LU_irrigN <- LU_GIS*r_lu
    print(paste0('Writing ....', i))
    write_irrig_LU(year, 'N_irrigation', 'missing', LU_irrigN, paste0('IrrigN_', i))
  }
}

LU_gross_irrigN_mosaic <- function(year)
{
  #creates a mosaic for gross irrigation N for each LU class
  map_LU_GIS(year = year, folder_name = 'N_irrigation', subfolder = 'missing', file_pattern = 'IrrigN_', write = TRUE)
}


## ---------------------------------------------------------------------------------------------------------
## COMPUTE ADJUSTMENT FACTORS !!!!!!!!!!!
## ---------------------------------------------------------------------------------------------------------

compute_total_CLC_LU_area_muni <-function(LU_class, year) {
  # computes the CLC LU area for each municipality
  # used to calculate the appropriate adjustment factor at the municipality scale
  # computes adjustment factors for each LU class
  # this is achieved by comparing national statistics of total crop areas of each LU class 
  # and by summing the corresponding CLC LU pixels
  
  r_LU <- get_LU_class_raster(LU_class, year)
  muni_df <- create_main_csv()
  muni_shp <- get_muni_shp()
  
  for (i in muni_shp$Muni_ID) {
    print(paste0('Working with the municipality ID ', i))
    sb <- subset(muni_shp, Muni_ID==i)
    r_LU_crop <- crop(r_LU, sb)
    r_LU_mask <- mask(r_LU_crop, sb)
    area <- cellStats(r_LU_mask, 'sum') #compute area
    
    muni_id <- which(muni_df$Muni_ID==i)
    muni_df[muni_id, 'CLC_LU_area[ha]'] <- area
  }
  return(muni_df)
}

compute_total_statistics_LU_area_muni <- function(LU_class, year) {
  # computes the total statistics area for each LU_class
  # basically after allocating different crops to different LU classes
  # these are summed
  
  area_statistics_lu <- compute_LU_class_volume(LU_class, year)[[2]]
  area_statistics_lu <- area_statistics_lu[, c(1,2,3, ncol(area_statistics_lu))] #subset ID and sum cols
  return(area_statistics_lu)
}

create_LU_adj_factor_dir <- function(year) {
  # creates year and LU subfolders within the also created LU_adj_factor folder in GIS module "modelling"
  
  folder_modelling <- select_GIS_output_submodule('Modelling')
  lu_adj_factor_folder <- file.path(folder_modelling, 'LU_adj_factor')
  dir.create(path = lu_adj_factor_folder)
  
  lu_adj_factor_folder <- file.path(lu_adj_factor_folder, as.character(year))
  dir.create(path = lu_adj_factor_folder)
  return(lu_adj_factor_folder)
}

compute_LU_class_adj_factor <- function(year) {
  # computes LU adjustment factor absed on CLC adj and statistic adj factors
  
  lu_classes <- loop_LU_classes()
  path <- create_LU_adj_factor_dir(year) #create directories
  
  for (i in lu_classes) {
    print(paste0('Working with data from ', i))
    
    clc_lu <- compute_total_CLC_LU_area_muni(i, year)
    statistics_lu <- compute_total_statistics_LU_area_muni(i, year)
    clc_lu$sum <- clc_lu$`CLC_LU_area[ha]`/statistics_lu$sum
    r_lu <- rasterize_data_muni(clc_lu, 'Muni_ID', 'sum')
  
    #write adj_factor raster
    filename <- file.path(path, paste0(i, '_adj_factor.tif'))
    tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
    writeRaster(r_lu, filename, options=tifoptions) 
  }
}


get_LU_adj_factor <- function(LU_class, year) {
  
  get_adj_factor<- get_modelling_files('LU_adj_factor', LU_class, as.character(year))
  return(get_adj_factor)
}

create_LU_adj_factor_mosaic <- function(year) {
  # loads for each LU class:
    # municipality adjustment factor, which has to be corrected by = 1/adj_factor
    # LU raster
  # calculates spatially explicit adjustment factor for each LU class
  # stores this raster into a list
  # creates a raster mosaic, with spatially explicit information about the adjustment factor for each LU clas
  # exports those data into the specified folder
  
  LU_class <- loop_LU_classes()
  r_list <- list()
  caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  path <- create_LU_adj_factor_dir(year) #create directories
  
  for (i in LU_class) {
    print(paste0('Working with LU class --> ', i))
    muni_adj_fact <- 1/get_lu_adj_factor(i, year) #correct the dam adjustment factor
    raster_lu <- get_LU_class_raster(i, year)
    
    spatial_lu_adj_fact <- muni_adj_fact*raster_lu 
    r_list <- append(r_list, spatial_lu_adj_fact)
  }
  r_list$fun <- sum
  r_mosaic <- do.call(raster::mosaic,r_list)
  r_mosaic_caa <- r_mosaic*caa
  
  filename <- file.path(path, 'MOSAIC_adj_factor.tif')
  tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
  writeRaster(r_mosaic_caa, filename, options=tifoptions) 
}

correct_irrig_percolation <- function(year) {
  # corrects the percolation losses in irrigation (m/yr)
  # by applying the mosaic adjustment factor
  
  mosaic_adj_fact <- get_LU_adj_factor('MOSAIC', year)
  irrig_perco <-  raster(get_irrig_LU_data(year, 'Volumes', 'leaching', 'meter'))
  
  irrig_perco <- irrig_perco*mosaic_adj_fact
  return(irrig_perco)
}

export_irrig_percolation_drainage_output <- function(year) {
  # exports irrigation losses through percolation to drainage subfolder
  # this will be used to compute total drainage = irrig_leaching + aquifer_recharge
  
  irrig_perco <- correct_irrig_percolation(year)
  write_gw_rasters(write = TRUE, rasterfile = irrig_perco, filename = paste0('irrig_perco', year_prefix(year)), 
                   irrig_mode = TRUE, main_subfolder = 'Drainage')
}

