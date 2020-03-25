source('./Main_functions.R')


## ----------------------- ACTIVITY DATA GETTERS ----------------------- ##
## ----------------------------------------------------------------------##

get_irrig_activity_data <- function(folder_pattern) {
  #general function to specify irrigation subdata folders
  #it is used as baseline for other functions to get the data
  
  irrig_path <- select_maindata_pattern(pattern = 'Irrigation')
  full_path <- list.files(path = irrig_path, pattern = folder_pattern, full.names = TRUE)
  files_folder <- list.files(full_path, full.names = TRUE)
}


read_folder_files <- function(folder_pattern, file_pattern) {
  #this function reads files within the get_folder_files
  #e.g. Reference_volumes, reference_volume.csv
  
  folder_files <- get_irrig_activity_data(folder_pattern)
  id_file <- which(grepl(file_pattern, folder_files)==TRUE)
  selected_file <- folder_files[id_file]
  ifelse(folder_pattern == 'NO3 stations',
         read_file <- read.csv(selected_file, stringsAsFactors = FALSE),
         read_file <- read.csv(selected_file))
  
  return(read_file)
  rm(list=c('folder_files', 'id_file', 'selected_file'))
}


disag_folders_year <- function(folder_path, year, pattern) {
  #this allows to disaggregate folder into subfolders per year and subsequently disaggregate according to a patter
  
  options(warn=-1) #i dont want this stupid warning about /
  folder_path <- check_folder(folder_path)
  options(warn=1)
  
  ifelse(missing(pattern)==TRUE, pattern <- '', pattern <- pattern)
  
  folder_path <- disaggregate_year(folder_path, year)
  new_path <- list.files(folder_path, pattern = pattern, full.names = TRUE)

  return(new_path)
  rm(list=c('folder_path'))
}


## ----------------------- EFFICIENCY GETTERS ----------------------- ##
## -------------------------------------------------------------------##


get_irrig_sys_efficiency <- function() {
  #gets simplified sys efficiency per irrig system irrespective of year
  
  eff_file <- read_folder_files('Efficiency', 'Irrig_sys_eff')
  return(eff_file)
}


get_temporal_irrig_sys_efficiency <- function() {
  #gets sys efficiency changing over time
  #irrig sys eff were calculated based on average values (i.e. between min and max) and  1 stdp deviation of the mean
  
  eff_file <- read_folder_files('Efficiency', 'Temporal_irrig_sys_eff')
  return(eff_file)
}



## ----------------------- CROP NAMES FUNCTIONS ----------------------- ##
## ---------------------------------------------------------------------##

get_maincrops_names <- function(year) {
  #get main crop names
  
  irrig_areas <- get_irrig_activity_data('Irrigated_areas')
  irrig_years <- disaggregate_year(irrig_areas, year)
  list_main_crops <- list.files(irrig_years)
  return(list_main_crops)
}

get_crop_names <- function(year, main_crop) {
  #gets the name of each crop within each main category
  #this will be used when looping each main crop to calculate water volumnes
  
  irrig_years <- get_irrig_activity_data('Irrigated_areas')
  select_year_main_crop <- disag_folders_year(irrig_years, year, main_crop)
  subcrops_list <- list.files(select_year_main_crop)
  subcrops_list <- gsub(pattern = '.csv', replacement = '', x = subcrops_list)
  
  return(subcrops_list)
  rm(list=c('irrig_years', 'select_year_main_crop'))
}


get_irrig_areas <- function(year, main_crop, crop) {
  # this specifies the main crop and associated crop to a folder
  # output is a specified csv file, already read onto memory
  # unit: ha yr-1
  
  irrig_years <- get_irrig_activity_data('Irrigated_areas')
  select_year_main_crop <- disag_folders_year(irrig_years, year, main_crop)
  select_crop <- list.files(select_year_main_crop, pattern =crop, full.names = TRUE)
  select_crop <- read.csv(select_crop)
  colnames(select_crop) <- gsub(pattern = 'X', replacement = '', x = names(select_crop))
  
  return(select_crop)
  rm(list=c('irrig_years', 'select_year_main_crop'))
}


## ----------------------- IRRIGIATION SYSTEMS VOLUMES----------------------- ##
## ---------------------------------------------------------------------------##

microaspersion_correct <- function(crop_file) {
  #checks if last col is microaspersion, if not it adds a new col with 0s
  
  last_col <- ncol(crop_file)
  
  if (colnames(crop_file)[last_col]!='microaspersion') {
    crop_file$microaspersion <- 0
  }
  crop_file$microaspersion <- as.integer(crop_file$microaspersion)
  return(crop_file)
}

get_vol_region <- function() {
  #loads volume_regions and municipality
  
  vol_region <- read_folder_files('Reference_volumes', 'reference_volume')
  short_vol_region <- vol_region[, c('Muni_ID', 'volume_region')]
  
  return(short_vol_region)
}

cross_ref_vol_region <- function() {
  #cross_references the lines of each vol_region
  #to be applied to for loop
  #returns a list with all 3 vol_regions and respective lines of Muni_ID
  
  vol_region <- get_vol_region()
  vol_regions <- c('Littoral_north', 'Interior_north', 'South')
  list <- list()
  
  for (i in vol_regions) {
    df <- data.frame(Muni_ID=which(vol_region[, 2]==i))
    colnames(df)[1] <- i
    list <- c(df, list)
  }
  
  return(list)
  rm(list=c('vol_region', 'vol_regions', 'df'))
}

cross_ref_crop <- function(crop_name) {
  #cross_refs crop line of reference volumes of any region with cropname
  #returns crop line
  
  read_vol_region <- read_folder_files(folder_pattern = 'Reference_volumes', 
                                       file_pattern = 'South') #same crop lines irrespective of file
  crop_line <- which(read_vol_region$irrigated_crop==crop_name)
  return(crop_line)
}

cross_ref_irrig_systems <- function(vol_region, crop_file, crop_col_id) {
  
  find_crop <- colnames(crop_file)[crop_col_id]
  condition <- which(colnames(vol_region)==find_crop)
  cross_ref_id <- vol_region[, c(1, condition)]
  
  return(cross_ref_id)
  rm(list=c('find_crop', 'condition'))
}


select_irrig_output_module <- function(submodule) {
  #this selects the sub-module of irrigationt output data
  #e.g. water volumes, or template reference volumes
  
  irrig_output_path <- select_module_output('Irrigation_module')
  subfolders <- list.files(irrig_output_path, pattern = submodule)
  
  full_path <- check_folder(paste0(irrig_output_path, subfolders))
  
  return(full_path)
}


## ----------------------- IRRIGIATION VOLUME COMPUTATIONS -----------------------##
## -------------------------------------------------------------------------------##

## ----------------------- CROP VOLUMES -----------------------##


populate_ref_volumes_crop <- function(year) {
  #this populates the reference volumes per irrigation system (m3/ha/yr) for each crop using the same template as the acreage
  #UNIT: m3/ha/yr
  
  main_crops <- get_maincrops_names(year)
  vol_region <- c('Littoral_north', 'Interior_north', 'South')

    for (a in main_crops) {
    crop_names <- get_crop_names(year, a)
    
    for (b in crop_names) {
      crop_files <- get_irrig_areas(year, a, b)
      crop_files <- microaspersion_correct(crop_files)
      crop_line <- cross_ref_crop(b) #crop_line_in_vol_region
      
      for (i in vol_region) {
        read_vol_region2 <- read_folder_files('Reference_volumes',i)
        lines2 <- cross_ref_vol_region()[i]
        
        for (c in lines2) {
          
          for (z in 4:ncol(crop_files)) {
            d <- cross_ref_irrig_systems(read_vol_region2, crop_files, z)
            crop_files[c, z] <- d[crop_line, 2]
          }
        }
      }
      write_output_subfolder(module_name = 'Irrigation_module', 
                        subfolder_name = 'Template_ref_volumes', 
                        file = crop_files, 
                        filename = b, 
                        subfolderX2_name = a)
    }
    }
  rm(list=c('main_crops', 'vol_region', 'crop_names', 'crop_files', 'crop_line', 'read_vol_region2', 'lines2', 'd'))
}


get_template_ref_vol <- function(main_crop, crop) {
  # gets the standard irrigation water volumes for a given crop
  # Those data are collated from DGADR (2019)
  # unit: m3 ha-1 yr-1
  
  crop_ref_vol <- get_module_subfolder_output(module = 'Irrigation_module', 
                                              submodule = 'Template_ref_volumes', 
                                              submoduleX2 = main_crop, 
                                              file_pattern = crop)
  return(crop_ref_vol)
}


get_irrig_sys_names <- function() {
  
  df <- get_irrig_areas(2009, 'horticulture', 'intensive')
  df <- df[, 4:ncol(df)]
  cols <- colnames(df)
  return(cols)
  rm(list='df')
}


compute_crop_volume <- function(year) {
  #computes total water volume of each irrigation system for each crop
  #UNIT: m3 yr-1
  ## NOTE: IRRIGATION VOLUMES ARE ONLY CORRECTED PER CROP AND NOT PER TOTAL IRRIGATION SYSTEMS EMPLOYED
  
  main_crops <- get_maincrops_names(2009)
  
  for (a in main_crops) {
    crop_names <- get_crop_names(year = year, main_crop = a)
    print(paste0('Writing water volumes of each crop of ', a))
    
    for (b in crop_names) {
      df <- create_main_csv()
      crop_areas <- get_irrig_areas(year = year, main_crop = a, crop = b)
      crop_areas <- microaspersion_correct(crop_areas)
      
      crop_ref_volumes <- get_template_ref_vol(a, b)
      
      compute_vol <- round(
        crop_areas[, seq(4, ncol(crop_areas))] * crop_ref_volumes[, seq(4, ncol(crop_ref_volumes))], 2)
      df <- cbind(df, compute_vol)
      write_annual_data(module_name = 'Irrigation_module', 
                        subfolder_name = 'Water volumes', 
                        file = df, filename = b, 
                        year = year, 
                        subfolder_nameX2 = a)
    }
  }
  rm(list=c('crop_names', 'df', 'crop_areas', 'crop_ref_volumes', 'compute_vol'))
}


get_crop_water_volume <- function(main_crop, crop, year) {
  #this gets crop water volume usage in irrigation output, water volumes sub-module
  # unit: m3 yr-1
  
  crop_vol <- get_module_subfolder_output(module = 'Irrigation_module', 
                                           submodule = 'Water volumes', 
                                           submoduleX2 = main_crop, 
                                           file_pattern = crop, 
                                           submoduleX3 = year)
  return(crop_vol)
}


