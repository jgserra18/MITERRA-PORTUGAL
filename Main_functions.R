############################## MAIN FUNCTIONS ######################################

#LIBRARIES
library(data.table)
library(dplyr)
library(tidyverse)
require("readxl")
require("readxl")
library(raster)


#this is the general function to check if the folder_path has the \\ in the last two digits
check_folder <- function(folder_path)
{
  file_rule <- substr(as.character(folder_path), 
                      nchar(folder_path)-1, 
                      nchar(folder_path))

  if (file_rule == '/')
  {
    return(folder_path)
  }
  else
  {
    folder_path_new <- paste0(folder_path, '/')
    return(folder_path_new)
  }
}

set_dir <- function()
{
  path <- readline(prompt="Directory: ")
  path <- as.character(path)
  setwd(path)
}

#this calls the activity data folder
#note: incase of error, see comment below
set_maindata_folder <- function(path)
{
	ifelse(missing(path),
		path <- './Activity data/',
		check_folder(path))
}

#this allows the user to select which activity data folder to select based on pattern
select_maindata_pattern <- function(pattern, path)
{
  ifelse(missing(path),
         path <- set_maindata_folder(),
         check_folder(path))

  select_pattern <- list.files(path=path, pattern =as.character(pattern))
  #fix_path <- check_folder(select_pattern) PLEASE LOOK AT THIS
  return(paste0(path, select_pattern))
}

#this loops prints every file within a specific subfolder
print_loop_files <- function(path)
{
	if (missing(path))
	{
		print('You need to specify a valid path, dude.')
	}
	else 
	{
		path <- check_folder(path)
		print('This prints csv files')
		folder_files <- list.files(path, pattern='*.csv')
		file_loop <- sapply(folder_files, function(x) print(paste0('', x)))
		print('===================')
		print(file_loop)
	}
}

#note: this only stores the files to further reading according to the full path
#C:\\Users\\MAMAMAM\\gnb99.csv
store_folder_files <- function(path)
{
  if (missing(path))
  {
    print('You need to specify a valid path, dude.')
  }
  else 
  {
    path <- check_folder(path)
    
    folder_files <- list.files(path, pattern = '*.csv')
    
    for (i in 1:length(folder_files))
    {
      folder_files[i] <- paste0(path, folder_files[i])
    }
    
    return(folder_files)
  }
}

#this is a subfuction needed to the main function to
#DISAGGREGATE PER YEAR
#This is only to be called in this main function
year_prefix <- function(year)
  {
    if ((is.numeric(year)==TRUE) & (nchar(as.character(year)) == 4))
      {
        yr_indx <- substr(as.character(year), 3, 4)
        return(yr_indx)
    }
    else {print('Print the year in numeric form.')}
  }

#this disaggregates the dataset based on year
#this is the basis for the calculations
#GNB requires a specific function here
disaggregate_year <- function(store_folder_files, year)
{
	yr_id <- year_prefix(year)
	db_yr <- c()

	for (i in 1:length(store_folder_files))
	{
		if(grepl(yr_id, store_folder_files[i])==TRUE)
		{
			db_yr[i] <- store_folder_files[i]	
		}
	}

	return(db_yr[!is.na(db_yr)])
}

#this should be only used when reading the csv files
#and used to store their data.frames according to their name
store_file_names <- function(path, year)
{
  if (missing(path))
  {
    print('You need to specify a valid path, dude.')
  }
  else 
  {
    path <- check_folder(path)
    folder_files <- list.files(path, pattern = '*.csv', full.names = FALSE)
  }
  filename_yr <- disaggregate_year(folder_files, year)

  return(filename_yr)
}

#subdata_path is for e.g. gnb_data (subfolder concerning GNB within the activity data)
#disagg_folder concerns the output of disaggregated per year
read_disagg_files <- function(disagg_folder, subdata_path, year)
{
  print('This returns a list with all the csv files for a specific year.')
  db_year <- disagg_folder #db_year has all the files for a specific year
  names_yr <- store_file_names(subdata_path, year) 
  #read all the files in db_year into a list
  read_files <- lapply(db_year, read.csv)
  names(read_files) <- gsub('.csv', '', names_yr, fixed=T)

  return(read_files)
}

#subset data
#this can be useful after two datasets are merged and have unwanted columns
#needs to be a bit hardcoded
#THIS IS A FUNCTION THAT IS GOING TO BE USED IN THE MAIN MERGE FUNCTION
subset_data <- function(data, input_subset)
{
  if (is.character(input_subset)==T)
  {
    print('You have chosen specific column names.\nThese are being now subset.')
    subdata <- subset(data, select=input_subset)
    return(subdata)
  }
  else if (is.numeric(input_subset)==T)
  {
    print('You have specified the column IDs to subset.\nThese are being now subset.')
    cols <- colnames(data)[input_subset]
    subdata <- subset(data, select=cols)

    return(subdata)
  }
  else 
  {
    print('Awkward. Go read the documentation.')
  }
}

#subset must be T or F
#input_subset may be either the column IDs or column names to be selected
merge_data <- function(main_file, file_to_merge, ID, subset, input_subset)
{
	merged_file <- merge(main_file, file_to_merge, by=as.character(ID))
	if (missing(subset))
	{
		print('You forgot to specify subset.\nThis is either T or F.')
	}
	ifelse(subset==T,
		subset_merged_file <- subset(merged_file, input_subset),
		print('Lunatic. Just want to do something really quick? Focus'))
}

#this is used to export the output of the calculations sub-modules to the respective
#activity data folders to then be organized itno a main dataaset
export_to_activity <- function(source, pathway, name, file)
{
  path <- select_maindata_pattern(pattern=source)

  char_store <- c('Application', 'storage')

  if ((grepl(char_store[1], pathway) & grepl(char_store[2], pathway))==F)
  {
    print('Please use a correct pathway loss, i.e. Application or storage.')
  }
  else 
  {
    cond <- grepl(pathway, char_store)
    name_loss <- char_store[cond]

    full_path <- check_folder(paste0(path, name_loss, name, '.csv'))
    fwrite(file, full_path)
  }
}


write_output <- function(pattern1, data_to_write, name, pattern2)
{
  main_path <- './'
  specify_folder <- list.files(main_path, pattern = pattern1)
  full_path <- paste0(main_path, specify_folder)
  full_path <- check_folder(full_path)

  if (missing(pattern2))
  {
    write_path <- paste0(full_path, 'Output/', name, '.csv')
    fwrite(data_to_write, write_path)
  }
  else
  {
    specify_folder_v2 <- list.files(full_path, pattern=pattern2)
    full_path_v2 <- paste0(full_path, specify_folder_v2)
    full_path_v2 <- check_folder(full_path_v2)

    write_path <- paste0(full_path_v2, 'Output/', name, '.csv')
    fwrite(data_to_write, write_path)
  }
}

#creates the main db template (Muni_ID, ID, Muni)
create_main_csv <- function()
{
    fle <- './Activity data/Main_sheet_csv/main_sheet.csv'
    file <- read.csv(fle)

    return(file)
}

#sub-function
#this selects only the last col of a certain spreadhseet
#returns a list with the selected col and with the col_name
select_ncol <- function(csv_file)
{
  file <- read.csv(csv_file)
  col_name <- colnames(file)[ncol(file)]
  file <- file[, ncol(file)]

  return(list(file, col_name))

}

#loads the specified UAA column to fasten the calculations
load_uaa <- function(year)
{
  uaa <- select_maindata_pattern('UAA')
  uaa_files <- store_folder_files(uaa)
  uaa_yr <- disaggregate_year(uaa_files, year)
  uaa_yr <- select_ncol(uaa_yr)[[1]]

}

#identifies apattern
identify_pattern <- function(file_to_id, pattern)
{
  cond_path <- grepl(pattern, file_to_id)
  cond_index <- which(cond_path, T)

  store <- file_to_id[cond_index]
  
  return(store)
}

print_colnames_and_id <- function(main_df)
{
  col_names <- colnames(main_df)
  cols_id <- seq(1, ncol(main_df))

  for (i in 1:length(cols_id))
  {
    full_paste <- paste0("Colname is ", col_names[i], "|| col id is ", cols_id[i], ".")
    print(full_paste)
    print('------------------------------------')
  }
}

#this is used to select the output folders of a specific module
#e.g. GNB, NH3
#GOAL: Calculate the NS and SSNB
select_module_output <- function(pattern, path)
{
  ifelse(missing(path),
         path <- './',
         check_folder(path))
  
  full_path <- path
  
  for (i in pattern) {
    path <- list.files(path=full_path, pattern=as.character(i))
    path <- check_folder(path)
    full_path <- paste0(full_path, path)
  }
  
  output_path <- paste0(full_path, "Output/")
  
  return(output_path)
}


data_cleaning <- function(dataset) {
  #this cleans up the data
  #Inf, NA or NaN
  options(warn=-1)
  dataset <- do.call(data.frame, lapply(dataset, function(x)
  {
    replace(x, is.infinite(x) | is.na(x), 0)
  }
  ))
  options(warn=0)
  colnames(dataset) <- gsub('X', '', names(dataset))
  return(dataset)
}

#searchs for pattern and returns col id
col_id_by_name <- function(name_to_find, df)
{
  id <- which(grepl(name_to_find, colnames(df))==TRUE)

  return(id)
}

#sums all the cols by a specific id
#this df must only contain the rows you want to sum apart from col_id
sumif <- function(df, id_pattern, col_id_sum_pattern)
{
  id_pattern <- col_id_by_name(id_pattern, df)
  id_sum <- col_id_by_name(col_id_sum_pattern, df)

  sum_rows <- rowsum(df[, id_sum], 
                    group=df[, id_pattern])

  colnames(sum_rows)[1] <- col_id_sum_pattern
  
  return(sum_rows)
}

average_if <- function(df, col_id)
{
  df <- stats::aggregate(df, by=list(df[, col_id]), FUN = mean)
  
  return(df)
}

#populate gnb input data with irrigation
#df is the output of read_disagg_files [[1]]
#output is the same as read_disagg_files where [[1]] are the inputs and [[2]] the outputs
populate_gng_input_irrig <- function(year, function_get_irrigN, df, overwrite)
{
  irrig_data <- function_get_irrigN(year)

  df[[1]][, 'irrigation'] <- irrig_data[, ncol(irrig_data)]

  return(df)
}

create_caa_df <- function(year)
{
  folder_path <- './Activity data/Main_sheet_csv/'
  names_yr <- store_file_names(folder_path, year) 
  file_path <- file.path(folder_path, names_yr)
  file <- data.table::fread(file_path)

  return(file)
}

write_results <- function(folder, file, filename)
{
  res_folder <- './ExploratoryAnalysis_module/Results/Paper#2/'
  folders <- list.files(res_folder, pattern=folder, full.names=T)

  filename <- paste0(filename, '.csv')
  file_path <- file.path(folders, filename)
  write.csv(file, file_path)
}

get_results <- function(folder, filename)
{
  res_folder <- './ExploratoryAnalysis_module/Results/Paper#2/'
  folders <- list.files(res_folder, pattern=folder, full.names=T)
  file <- list.files(folders, pattern=filename, full.names=T)
  read <- read.csv(file)
}


## ----------------------- DATABASE FORMAT CONVERSION ---------------------- ##
## --------------------------------------------------------------------------##


convert_raw_xls_into_csv <- function(filepath_from, filepath_to) {
  # CONVERTS WEIRD AND OLD .XLS FILES TO TREATED .CSV

  xls_file <- read_excel(filepath_from)
  # get filename
  filename <- paste0(xls_file[6,3], '.csv')
  
  # identify columns to subset -----------------------------------------------------
  yrs <- seq(2018, 1986)
  store_yrs <- c()
  ctr <- 0
  for (i in yrs) {
    ctr <- ctr + 1
    store_yrs[ctr] <- which(xls_file[8,]==i) 
  }
  
  store_yrs <- c(1,2, store_yrs)
  xls_file <- xls_file[, store_yrs]
  
  # remove rows 1-7, 9, 17-25 
  xls_file <- xls_file[-c(1,2,3,4,5,6,7,9, 17, 18, 19, 20, 21, 22,23,24,25), ]
  
  names(xls_file) <- xls_file[1, ]
  colnames(xls_file)[1] <- 'agrarian_region_id'
  colnames(xls_file)[2] <- 'agrarian_region'
  xls_file <- xls_file[-1, ]
  
  write.csv(x = xls_file, file = file.path(filepath_to, filename), row.names = F)
}



convert_raw_xlsv_into_csv <- function(filepath_from, filepath_to) {
  # CONVERTS WEIRD .XLSX FILES TO TREATED .CSV

  xlsv_file <- read.xlsx(xlsxFile = filepath)
  # get filename
  filename <- paste0(xlsv_file[4,3], '.csv')
  # remove X4 and X6
  xlsv_file <-xlsv_file[, -c(4,6)]
  # remove rows 1-5 and 7 and 15-20
  xlsv_file <- xlsv_file[-c(1,2,3,4,5,7, 15,16,17,18,19,20), ]
  
  names(xlsv_file) <- xlsv_file[1, ]
  # now remove row 1
  xlsv_file <- xlsv_file[-1, ]
  colnames(xlsv_file)[1] <- 'agrarian_region_id'
  colnames(xlsv_file)[2] <- 'agrarian_region'

  write.csv(x = xlsv_file, file = file.path(filepath_to, filename), row.names = F)
}


## ----------------------- ACTIVITY DATA GETTERS ----------------------- ##
## ----------------------------------------------------------------------##

get_activity_data <- function(subfolder, subfolderX2, file_pattern) {
  # general function to get data from activity data 
  # e.g., get_activity_data('Climatic_data', 'Precipitation', 'rast_caa09')
  # the function automatically distinguishes between the data formats

  data_folder <- select_maindata_pattern(subfolder)
  
  if (missing(subfolderX2)==TRUE) {
    file <- list.files(data_folder, pattern = file_pattern, full.names = TRUE)
  }
  else {
    subfolder <- list.files(data_folder, pattern = subfolderX2, full.names=TRUE)
    file <- list.files(subfolder, pattern = file_pattern, full.names = TRUE)
  }
  
  if (grepl('.tif', file)==TRUE) {
    r_file <- raster(file) 
    return(r_file)
  }
  else if (grepl('.shp', file)==TRUE) {
    r_file <- readOGR(file)
      colnames(r_file) <- gsub('X', '', names(r_file))
      return(r_file)
  }
  else {
    r_file <- read.csv(file)
      colnames(r_file) <- gsub('X', '', names(r_file))
      return(r_file)
  }

  rm(list=c('data_folder', 'file', 'subfolder'))
}



load_raw_data <- function(subfolder) {
  ## loads raw data activity data subfolders
  
    raw_folder <- select_maindata_pattern('Raw')
    subfolder <- file.path(raw_folder, list.files(raw_folder, pattern=subfolder))
    return(subfolder)
}


get_crop_data <- function(year, subfolder, main_crop, crop) {
  # gets crop  data for a given crop ;; year can be specified or not
  # subfolder can be Yields or Areas
  # Fertilizer_support is not included
  # unit: kg dry-matter ha-1 yr-1 ;; ha yr-1

  crop_data_folder <- load_raw_data(subfolder = 'Crop_data')
  select_crop_param <- list.files(path = crop_data_folder, pattern = subfolder, full.names = T)
  
  # AREAS IN THE CENSUS YEARS ARE AVAILABLE AT THE MUNICIPALITY SCALE RATHER THAN THE AGRARIAN REGION
  # SO THESE HAVE TO BE SPECIFIED 
  if (subfolder=='Areas' | subfolder =='Irrigated_areas') {
        if (year==1989 | year==1999 | year==2009) {
          select_data <- list.files(path = select_crop_param, pattern = 'Census_years', full.names = T)
        } 
        else if (year!=1989 | year!=1999 | year!=2009) {
          select_data <- list.files(path = select_crop_param, pattern = 'Other_years', full.names = T)
        } 
        select_maincrop <- list.files(path = select_data, pattern = main_crop, full.names = T)
  }
  
  else {
    select_maincrop <- list.files(path = select_crop_param, pattern = main_crop, full.names = T)
  }
  
  select_crop <- list.files(path = select_maincrop, pattern = crop, full.names = T)
  
  if (missing(year)==TRUE) {
    r_crop.file <- read.csv(select_crop)
    # correct colnames
    colnames(r_crop.file) <- gsub(pattern = 'X', replacement = '', x = colnames(r_crop.file))
  }
  else if (missing(year)==FALSE & (year!=1989 | year!=1999 | year!=2009) & subfolder != 'Areas') {
    r_crop.file <- read.csv(select_crop)
    colnames(r_crop.file) <- gsub(pattern = 'X', replacement = '', x = colnames(r_crop.file))
    r_crop.file <- r_crop.file[, c(1, 2, which(names(r_crop.file)==year))]
  }
  else {
    r_crop.file <- read.csv(select_crop)
    colnames(r_crop.file) <- gsub(pattern = 'X', replacement = '', x = colnames(r_crop.file))
  }
  return(r_crop.file)  
}

# see irrigation and GIS module
# change this
get_raw_crop_areas <- function(year, main_crop, crop) {
  #gets raw crop areas 
  
  subfolder <- load_raw_data('Crop_data')
  select_yr <- file.path(subfolder, list.files(subfolder, pattern=as.character(year)))
  select_areas <- file.path(select_yr, list.files(select_yr, pattern = 'Areas'))
  select_maincrop <- list.files(path = select_areas, pattern = main_crop, full.names=T)
  select_crop <- list.files(path = select_maincrop, pattern = crop, full.names=T)

  read_crop <- read.csv(select_crop)
  return(read_crop)
}

load_raw_total_arable <- function(subfolder, year, file_pattern)
{ 
  subfolder <- load_raw_data(subfolder)
  select_yr <- file.path(subfolder, list.files(subfolder, pattern=as.character(year)))
  select_file <- file.path(select_yr, list.files(select_yr, pattern=file_pattern))

  ifelse(grepl('.csv', select_file), file <- read.csv(select_file), file <- raster(select_file))
  return(file)
}

## ----------------------- SPATIAL DISAGGREGATION FUNCTIONS ----------------------- ##
## ---------------------------------------------------------------------------------##


call_spatial_disaggregation <- function() {

  module_path <- list.files(patter='SpatialAggregation', full.names = T)
  select_disagg_file <- list.files(module_path, pattern = 'spatial_disaggregation', full.names = T)
  file <- read.csv(select_disagg_file)
}

spatial_disagg_agrarian_muni <- function(agrarian_df) {
  # spatially disaggregates the agrarian regions into municipalities
  
  muni_df <- call_spatial_disaggregation()
  muni_df <- left_join(muni_df, agrarian_df[, -2], 'agrarian_region_id')
  muni_df <- muni_df[, -seq(4,8)]
  
  return(muni_df)
}

## ----------------------- CREATE DIRECTORY AND SUBFOLDERS ----------------------- ##
## --------------------------------------------------------------------------------##

create_module_dir <- function(module.name) {
  # creates module name e.g., fertilization
  
  path <- './'
  module_folder <- file.path(path, module.name)
  dir.create(path = module_folder, showWarnings = F)
  return(module_folder)
}

create_module_output_subfolder <- function(module.name, subfolder.name) {
  # 1 - creates an OUTPUT FOLDER IF IT DOSNT EXIST
  # 2 - creates the specified subfolder within the output folder
  # returns the subfolder path
  
  create_module_dir(module.name)
  out_path <- select_module_output(module.name)
  module_out <- dir.create(path=out_path, showWarnings = F)
  add_subfolder <-  file.path(out_path, subfolder.name)
  dir.create(path = add_subfolder, showWarnings = F)
  return(add_subfolder)
}

create_module_output_subfolderX2 <- function(module.name, subfolder.name, subfolderX2.name) {
  # this function should be used to create crop folders within a specific subfolder
  # e.g., 'Fertilization_module', 'Fertilization_rates', 'cereals
  
  sub_path <- create_module_output_subfolder(module.name, subfolder.name)
  subX2_path <- file.path(sub_path, subfolderX2.name)
  dir.create(path = subX2_path, showWarnings = F)
  return(subX2_path)
}

create_module_annual_subfolder <- function(module.name, subfolder.name, year, subfolderX2.name) {
  # creates a subfolder for a given year
  
  if (missing(subfolderX2.name)==TRUE) {
    add_subfolder <- create_module_output_subfolder(module.name, subfolder.name)
    subfolder_year <- file.path(add_subfolder, year)
    dir.create(path = subfolder_year, showWarnings = F)
    return(subfolder_year)
  } 
  else {
    add_subfolder <- create_module_output_subfolderX2(module.name, subfolder.name, subfolderX2.name)
    add_subfolder_year <- file.path(add_subfolder, year)
    dir.create(path = add_subfolder_year, showWarnings = F)
    return(add_subfolder_year)
  }
}


## ----------------------- OUTPUT WRITING ----------------------- ##
## ---------------------------------------------------------------##

write_output_subfolder <- function(module_name, subfolder_name, file, filename) {
  # general function to write the output to a specific subfolder
  
  folder_path <- create_module_output_subfolder(module_name, subfolder_name)
  file_path <- file.path(folder_path, paste0(filename, '.csv'))
  write.csv(file, file_path, row.names = F)
}


write_output_subfolder <- function(module_name, subfolder_name, file_df, filename, subfolderX2_name) {
  # general function to write the output to a specific subfolder
  
  folder_path <- create_module_output_subfolder(module_name, subfolder_name)

  if (missing(subfolderX2_name)==TRUE) {
    if (class(file_df)=='RasterLayer') {
       file_path <- file.path(folder_path, paste0(filename, '.tif'))
       tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
       writeRaster(file_df, file_path, options=tifoptions)
    } else {
      file_path <- file.path(folder_path, paste0(filename, '.csv'))
      write.csv(x = file_df, file = file_path, row.names = F)
    }
  } 

  else {
    subfolder_path <- file.path(folder_path, subfolderX2_name)
    dir.create(path = subfolder_path, showWarnings = F)

    if (class(file_df)=='RasterLayer') {
       subfile_path <- file.path(subfolder_path, paste0(filename, '.tif'))
       tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
       writeRaster(file_df, subfile_path, options=tifoptions)
    } else {
      subfile_path <- file.path(subfolder_path, paste0(filename, '.csv'))
      write.csv(x = file_df, file = subfile_path, row.names = F)
    }
  }
}


write_annual_data <- function(module_name, subfolder_name, file, filename, year, subfolder_nameX2) {
  # e.g., write_annual_data('Fertilization_module', 'Fertilization_rates', d, 'looool', 699999)

  if(missing(subfolder_nameX2)==TRUE) {
             export_path <- create_module_annual_subfolder(module_name, subfolder_name, year)
  } else {
             export_path <- create_module_annual_subfolder(module_name, subfolder_name, year, subfolder_nameX2)
  }
  if (class(file)=='RasterLayer') {
    name <- paste0(filename, '.tif')
    file_path <- file.path(export_path, name)
    tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
    writeRaster(file, file_path, options=tifoptions)
  }
  else {
    name <- paste0(filename, '.csv')
    file_path <- file.path(export_path, name)
    write.csv(x = file, file = file_path, row.names = F)
  }
}



## ----------------------- MODULE OUTPUT GETTERS ---------------------- ##
## ---------------------------------------------------------------------##


get_module_output <- function(module, file_pattern, year) {
  #generic function to get data from modules
  
  module_path <- select_module_output(module)
  files <- list.files(module_path, pattern = file_pattern)
  files <- disaggregate_year(files, year)
  
  full_path <- file.path(module_path, files)
  select_file <- read.csv(full_path)
}

get_module_subfolder_output <- function(module, submodule, submoduleX2, file_pattern, submoduleX3) {
  # function to get the outputs of a certain module
  # only applicable to data.frames
  # e.g., get_module_subfolder_output('Fertilization_module', 'Manure_crop_distribution', 'cereals', 'rye_slurry', '2009')
  
  module_path <- select_module_output(module)
  submodule_path <- list.files(module_path, pattern=submodule, full.names=TRUE)
  
  if (missing(submoduleX2)==TRUE && missing(submoduleX3)==TRUE) {
    file <- list.files(path = submodule_path, pattern = file_pattern, full.names = TRUE)
    if(grepl('.tif', file)==TRUE) {
      file <- raster(file)
    } else if (grepl('.grd', file)==TRUE) {
      file <- stack(file)
    } else {
      file <- read.csv(file)
    }

  }
  else if (missing(submoduleX2)==FALSE && missing(submoduleX3)==TRUE) {
    submoduleX2_path <- list.files(submodule_path, pattern=as.character(submoduleX2), full.names=TRUE)
      file <- list.files(path = submoduleX2_path, pattern = file_pattern, full.names = TRUE)
    if(grepl('.tif', file)==TRUE) {
      file <- raster(file)
    } else if (grepl('.grd', file)==TRUE) {
      file <- stack(file)
    } else {
      file <- read.csv(file)
    }

  }
  else {
    submoduleX2_path <- list.files(submodule_path, pattern=as.character(submoduleX2), full.names=TRUE)
    submoduleX3_path <- list.files(submoduleX2_path, pattern=as.character(submoduleX3), full.names=TRUE)
    file <- list.files(path = submoduleX3_path, pattern = file_pattern, full.names = TRUE)
    if(grepl('.tif', file)==TRUE) {
      file <- raster(file)
    } else if (grepl('.grd', file)==TRUE) {
      file <- stack(file)
    } else {
      file <- read.csv(file)
    }

  }
  if (class(file)=='data.frame') {
      colnames(file) <- gsub(pattern = 'X', replacement = '', x = colnames(file))
  }
  
  return(file)
  rm(list = c('module_path', 'submodule_path', 'submoduleX2_path', 'submoduleX3_path'))
}

