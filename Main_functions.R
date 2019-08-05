############################## MAIN FUNCTIONS ######################################

#LIBRARIES
library(data.table)
library(dplyr)
library(tidyverse)

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
  
  for (i in pattern)
  {
    path <- list.files(path=full_path, pattern=as.character(i))
    path <- check_folder(path)
    full_path <- paste0(full_path, path)
  }
  
  output_path <- paste0(full_path, "Output/")
  
  return(output_path)
}

#this cleans up the data
#Inf, NA or NaN
data_cleaning <- function(dataset)
{
  dataset <- do.call(data.frame, lapply(dataset, function(x)
  {
    replace(x, is.infinite(x) | is.na(x), 0)
  }
  ))

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

#generic function to get data from modules
get_module_output <- function(module, file_pattern, year)
{
  module_path <- select_module_output(module)
  files <- list.files(module_path, pattern = file_pattern)
  files <- disaggregate_year(files, year)
  
  full_path <- file.path(module_path, files)
  select_file <- read.csv(full_path)
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


## loads raw data activity data subfolders
load_raw_data <- function(subfolder)
{
    raw_folder <- select_maindata_pattern('Raw')
    subfolder <- file.path(raw_folder, list.files(raw_folder, pattern=subfolder))
    return(subfolder)
}

#gets raw crop areas
get_raw_crop_areas <- function(year, main_crop, crop)
{
  subfolder <- load_raw_data('Crop_data')
  select_yr <- file.path(subfolder, list.files(subfolder, pattern=as.character(year)))
  select_maincrop <- file.path(select_yr, list.files(select_yr, pattern = main_crop))
  select_crop <- file.path(select_maincrop, list.files(select_maincrop, pattern = crop))

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


call_spatial_disaggregation <- function() {

  module_path <- list.files(patter='SpatialAggregation', full.names = T)
  select_disagg_file <- list.files(module_path, pattern = 'spatial_disaggregation', full.names = T)
  file <- read.csv(select_disagg_file)
}