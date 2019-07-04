source('./Main_functions.R')

#general function to specify irrigation subdata folders
#it is used as baseline for other functions to get the data
get_folder_files <- function(folder_pattern)
{
  irrig_path <- check_folder(select_maindata_pattern(pattern = 'Irrigation'))
  folder_pattern <- list.files(irrig_path, pattern = folder_pattern)
  full_path <- check_folder(paste0(irrig_path, folder_pattern))
  
  ifelse(folder_pattern=='Irrigated_areas' | folder_pattern=='NO3 sampling data' | folder_pattern=='Irrigation_N',
         files_folder <- paste0(full_path, list.files(full_path)),
         files_folder <- store_folder_files(full_path))

  return(files_folder)
}

#this function reads files within the get_folder_files
#e.g. Reference_volumes, reference_volume.csv
read_folder_files <- function(folder_pattern, file_pattern)
{
  folder_files <- get_folder_files(folder_pattern)
  selected_file <- folder_files[which(grepl(file_pattern, folder_files)==TRUE)]
  read_file <- read.csv(selected_file)
}

#this allows to disaggregate folder into subfolders per year and subsequently disaggregate according to a patter
disag_folders_year <- function(folder_path, year, pattern)
{
  options(warn=-1) #i dont want this stupid warning about /
  folder_path <- check_folder(folder_path)
  options(warn=1)
  
  ifelse(missing(pattern)==TRUE, pattern <- '', pattern <- pattern)
  
  folder_path <- disaggregate_year(folder_path, year)
  folder_files <- list.files(folder_path, pattern = pattern)
  new_path <- paste0(folder_path, check_folder(folder_files))
  
  return(new_path)
}

get_irrig_sys_efficiency <- function()
{
  eff_file <- read_folder_files('Efficiency', 'Irrig_sys_eff')
  return(eff_file)
}

#get main crop names
get_maincrops_names <- function(year)
{
  irrig_areas <- get_folder_files('Irrigated_areas')
  irrig_years <- disaggregate_year(irrig_areas, year)
  list_main_crops <- list.files(irrig_years)
  
  return(list_main_crops)
}

#gets the name of each crop within each main category
#this will be used when looping each main crop to calculate water volumnes
get_crop_names <- function(year, main_crop)
{
  irrig_years <- get_folder_files('Irrigated_areas')
  select_year_main_crop <- disag_folders_year(irrig_years, year, main_crop)
  subcrops_list <- list.files(select_year_main_crop)
  subcrops_list <- gsub(pattern = '.csv', replacement = '', x = subcrops_list)
  
  return(subcrops_list)
}

#this specifies the main crop and associated crop to a folder
#uses the function disag_folders_year 
#output is a specified csv file, already read onto memory
## AREAS GIVEN IN HA
get_irrig_areas <- function(year, main_crop, crop)
{
  irrig_years <- get_folder_files('Irrigated_areas')
  select_year_main_crop <- disag_folders_year(irrig_years, year, main_crop)
  select_crop <- list.files(select_year_main_crop, pattern = crop)

  crop_path <- paste0(select_year_main_crop, select_crop)
  read_crop <- read.csv(crop_path)
}
 
get_subcrops <- function(year, main_crop)
{
  irrig_years <- get_folder_files('Irrigated_areas')
  select_year_main_crop <- disag_folders_year(irrig_years, year, main_crop)
  subcrops_list <- list.files(select_year_main_crop)
  subcrops_list <- gsub(pattern = '.csv', replacement = '', x = subcrops_list)
  
  return(subcrops_list)
}

#checks if last col is microaspersion, if not it adds a new col with 0s
microaspersion_correct <- function(crop_file)
{
  last_col <- ncol(crop_file)
  
  if (colnames(crop_file)[last_col]!='microaspersion')
  {
    crop_file$microaspersion <- 0
  }
  crop_file$microaspersion <- as.integer(crop_file$microaspersion)
  
  return(crop_file)
}

#loads volume_regions and municipality
get_vol_region <- function()
{
  vol_region <- read_folder_files('Reference_volumes', 'reference_volume')
  short_vol_region <- vol_region[, c('Muni_ID', 'volume_region')]
  
  return(short_vol_region)
}

#cross_references the lines of each vol_region
#to be applied to for loop
#returns a list with all 3 vol_regions and respective lines of Muni_ID
cross_ref_vol_region <- function()
{
  vol_region <- get_vol_region()
  vol_regions <- c('Littoral_north', 'Interior_north', 'South')
  list <- list()
  
  for (i in vol_regions)
  {
    df <- data.frame(Muni_ID=which(vol_region[, 2]==i))
    colnames(df)[1] <- i
    list <- c(df, list)
  }
  return(list)
}

#cross_refs crop line of reference volumes of any region with cropname
#returns crop line
cross_ref_crop <- function(crop_name)
{
  read_vol_region <- read_folder_files('Reference_volumes', 'South') #same crop lines irrespective of file
  
  crop_line <- which(read_vol_region$irrigated_crop==crop_name)
  return(crop_line)
}

cross_ref_irrig_systems <- function(vol_region, crop_file, crop_col_id)
{
  condition <- which(colnames(vol_region)==colnames(crop_file)[crop_col_id])
  cross_ref_id <- vol_region[, c(1, condition)]
  
  return(cross_ref_id)
}

#creates the main directory to populate the template reference volume for each irrigation system per crop
main_crop_dir_create <- function(module, folder_name, year)
{
  irrig_output <- select_module_output('Irrigation')
  module_path <- check_folder(paste0(irrig_output, module))
  
  if (missing(year)==TRUE)
  {
    dir.create(path = paste0(module_path, folder_name))
  }
  else 
  {
    year_list <- list.files(module_path, pattern = as.character(year))
    module_path <- check_folder(paste0(module_path, year_list))
    dir.create(path = paste0(module_path, folder_name))
  }
  dir.create(path = paste0(module_path, folder_name))
}

#this selects the sub-module of irrigationt output data
#e.g. water volumes, or template reference volumes
select_irrig_output_module <- function(submodule)
{
  irrig_output_path <- select_module_output('Irrigation_module')
  subfolders <- list.files(irrig_output_path, pattern = submodule)
  
  full_path <- check_folder(paste0(irrig_output_path, subfolders))
  
  return(full_path)
}

#writes irrigation output data to the selected data
write_irrig_output <- function(module, writefile, filename, main_crop)
{
  irrig_output <- select_module_output('Irrigation')
  module_path <- check_folder(paste0(irrig_output, module))
  
  if (missing(main_crop)==FALSE)
  {
    module_path <- check_folder(paste0(module_path, main_crop))
  }
  
  filename <- paste0(module_path, filename, '.csv')
  fwrite(writefile, filename)
}

#writes the output of irrigation volumes per system 
write_irrig_sys_output <- function(year, writefile, filename)
{
  irrig_sys_vol_path <- check_folder(
                          paste0(
                              select_irrig_output_module('Irrigation_systems'), year)
                                    )
  filename <- paste0(irrig_sys_vol_path, filename, '.csv')
  fwrite(writefile, filename)
}

get_template_ref_vol <- function(main_crop, crop)
{
  output_path <- select_irrig_output_module('Template_ref_volumes')
  maincrop_output_path <- check_folder(paste0(output_path, main_crop))
  
  select_crop_ref_vol <- paste0(maincrop_output_path, list.files(maincrop_output_path, pattern = crop))
  read_crop_ref_vol <- read.csv(select_crop_ref_vol)
}

#this populates the reference volumes per irrigation system (m3/ha/yr) for each crop using the same template as the acreage
#UNIT: m3/ha/yr
populate_ref_volumes_crop <- function(year)
{
  main_crops <- get_maincrops_names(year)
  vol_region <- c('Littoral_north', 'Interior_north', 'South')
  
  for (a in main_crops)
  {
    crop_names <- get_crop_names(year, a)
    main_crop_dir_create(module = 'Template_ref_volumes', folder_name = a)
    
    for (b in crop_names)
    {
      b <- as.character(b)
      crop_files <- get_irrig_areas(year, a, b)
      crop_files <- microaspersion_correct(crop_files)
      crop_line <- cross_ref_crop(b) #crop_line_in_vol_region
      
      for (i in vol_region)
      {
        read_vol_region2 <- read_folder_files('Reference_volumes',i)
        lines2 <- cross_ref_vol_region()[i]
        
        for (c in lines2)
        {
          for (z in 4:ncol(crop_files))
          {
            d <- cross_ref_irrig_systems(read_vol_region2, crop_files, z)
            crop_files[c, z] <- d[crop_line, 2]
          }
        }
      }
      write_irrig_output('Template_ref_volumes', crop_files, b, a)
    }
  }
}

get_irrig_sys_names <- function()
{
  df <- get_irrig_areas(2009, 'horticulture', 'intensive')
  df <- df[, 4:ncol(df)]
  cols <- colnames(df)
  
  return(cols)
}

#computes total water volume of each irrigation system for each crop
#UNIT: m3/yr
compute_crop_volume <- function(year)
{
  main_crops <- get_maincrops_names(year)
  
  for (a in main_crops)
  {
    crop_names <- get_crop_names(year, a)
    main_crop_dir_create(module = 'Water volumes', folder_name = a, year = year)
    print(paste0('Writing water volumes of each crop of ', a))
    
    for (b in crop_names)
    {
      df <- create_main_csv()
      crop_areas <- get_irrig_areas(year, a, b)
      crop_areas <- microaspersion_correct(crop_areas)
      
      crop_ref_volumes <- get_template_ref_vol(a, b)
      
      compute_vol <- crop_areas[, seq(4, ncol(crop_areas))]*crop_ref_volumes[, seq(4, ncol(crop_ref_volumes))]
      df <- cbind(df, compute_vol)
      write_irrig_output(module = paste0('Water volumes/', year), df, b, a)
      rm(df)
    }
  }
}

#this gets crop water volume usage in irrigation output, water volumes sub-module
get_crop_water_volume <- function(main_crop, crop, year)
{
  ifelse(missing(year)==TRUE, year <- 2009, year <- year)
  
  vol_submodule_path <- select_irrig_output_module('Water volumes')
  vol_submodule_path <- check_folder(paste0(vol_submodule_path, year))
  crop_path <- paste0(vol_submodule_path, main_crop, '/', crop, '.csv')
  read_crop <- read.csv(crop_path)
}

aggregate_irrig_system_vol <- function(year, irrig_system, write)
{
  main_crops <- get_maincrops_names(year)
  df <- create_main_csv() #create main df to populate crop water volume usage per irrigation system specified
  
  for (a in main_crops)
  {
    crops <- get_crop_names(year, a)
    
    for (b in crops)
    {
      crop_volume <- get_crop_water_volume(a, b)
      cols <- colnames(crop_volume)
      id_col_irrig <- which(cols==irrig_system)
      crop_volume_irrig <- crop_volume[, id_col_irrig]
      
      df <- cbind(df, crop_volume_irrig)
      colnames(df)[ncol(df)] <- b
    }
  }
  df$SUM <- rowSums(df[, 4:ncol(df)])
  ifelse(write==TRUE,
        write_irrig_sys_output(year, df, irrig_system),
        return(df))
}

#computes the total water volume used in each irrigation system at the mainland level
compute_irrig_sys_vol <- function(year, compute_sum_irrig_sys)
{
  irrig_sys_names <- get_irrig_sys_names()
  sum_df <- data.frame(irrig_sys=irrig_sys_names, w_vol_m3=0)
  
  ctr <- 0
  
  for (i in irrig_sys_names)
  {
    ctr <- ctr +1
    print(paste('Calculating water volume of ', i))
    df <- aggregate_irrig_system_vol(year, i, FALSE)
    col_sum <- colSums(df[, ncol(df), drop=FALSE])
    sum_df[ctr, 2] <- col_sum
  }
  
  if(compute_sum_irrig_sys==T)
  {
    write_irrig_sys_output(year = year, writefile = sum_df, filename = 'w_vol_irrig_systems')
  }
  return(sum_df)
}

#other function to load output data according to year directory and file_pattern
#d <- get_output_file('Irrigation_N', 2009, 'total)
get_output_file <- function(submodule, year, file_pattern)
{
  folder_path <- select_irrig_output_module(submodule)
  folder_path <- check_folder(paste0(folder_path,year))
  files <- list.files(folder_path, pattern = file_pattern)
  
  file_path <- paste0(folder_path, files)
  read_file <- read.csv(file_path)
  return(read_file)
}

#Calculates the N-input from irrigationN to be implemented in the GNB
#d <- get_irrigatioN_gnb(1999)
get_irrigatioN_gnb <- function(year)
{
  irrigatioN_file <- get_output_file('Irrigation_N', year, 'wo_eff_total')
  cols <- c(1,2,3, ncol(irrigatioN_file))
  
  irrigatioN_file <- irrigatioN_file[,cols]
  colnames(irrigatioN_file)[ncol(irrigatioN_file)] <- 'irrig_nha'
  irrigatioN_file <- data_cleaning(irrigatioN_file)
  
  return(irrigatioN_file)
}


replace_gnb_input_irrig <- function(df, year)
{
  path <- check_folder(select_maindata_pattern('GNB'))
  filename <- paste0('Inputs', year_prefix(year), '.csv')
 
  print(paste0('Writing irrigatioN for ', year))
  fwrite(df[[1]], paste0(path, filename))
  print('Finished!')
}


