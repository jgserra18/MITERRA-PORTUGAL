############################## GENERAL FUNCTIONS ##################################
#=================================================================================#
###################################################################################

##===============================================================================#
################################## START SCRIPT ##################################



##read subfolders and select which one
#Importance to select which type of data you want
read_folders <- function(path, data_pattern)
  {
    data_path <- check_folder(as.character(path))
    select_db <- list.files(data_path, pattern = data_pattern)
    return(paste0(data_path, select_db))
}

# call activity data main folder
call_activity_data <- function()
{
  path <- check_folder(define_main_path())
  data_folder <- read_folders(path, 'Activity')
}



#This function prints all the existing subfolders in each main activity data folder
# E.g. in N-NH3 folder you have Application, Manure_housing, Manure_storage
list_subfolders <- function(data_folder)
  {
    return(list.files(data_folder, pattern = '*.csv'))
  }

#this prints the containing subfolders or files
#loop_files(list_subfolders(nh3))
loop_files <- function(folder_to_loop)
  {
    print('Folder contains: ')
    print('=================')
    file_store <- sapply(folder_to_loop, function(x) print(paste0('', x)))
  }

#==================================================================#
#This has to go to N-NH3 function
#To properly use this, visualize data using list_subfolders
#subdata_pattern is the subfolders in the chosen read_folders folder
#this returns all the files within the subfolder
nh3_read_subfolders <- function(data_folder, subdata_pattern)
  {
    sub_folder <- paste0(data_folder, '\\', subdata_pattern, '\\')
    list_files <- list.files(sub_folder, pattern = '*.csv')
    loop_files(list_files)
    return(list_files)
  }

#rule folder_file_path must end with \\
#this function can either only print the files within a subfolder or
#it can returns the filenames in vector form, which can be subsequently used to read
print_return_datafiles <- function(folder_file_path, print_return)
  {
    folder_path <- check_folder(folder_file_path)
  
    if (missing(print_return))
    {
      file_db <- list.files(path = folder_path, pattern = '*.csv')
      return(file_db)
    }
    else if (print_return=='print' | print_return=='PRINT')
    {
      file_db <- list.files(path = folder_file_path, pattern = '*.csv')
      print(file_db)
    }
    else {print('Read the documentation')}
  }

#specific year;; returns a string with the last 2 indexes of the year to identify
#collate_sub_year(2009)
collate_sub_year <- function(year)
  {
    if ((is.numeric(year)==TRUE) & (nchar(as.character(year)) == 4))
      {
        yr_indx <- substr(as.character(year), 3, 4)
        return(yr_indx)
    }
    else {print('Print the year in numeric form.')}
  }

#returns vector with files according to the specified year
#in this case, either 1999 or 2009
data_by_year <- function(folder_with_files, year, print_or_return)
  {
    if (missing(print_or_return)) 
    {
      check_year <- collate_sub_year(year)
      selected_yr_db <- subset(folder_with_files, 
                               subset = grepl(check_year, 
                                              folder_with_files)==T)
      return(selected_yr_db)
    }
    else if (print_or_return=="print") 
    {
      check_year <- collate_sub_year(year)
      selected_yr_db <- subset(folder_with_files, 
                               subset = grepl(check_year, 
                                              folder_with_files)==T)
      loop_files(selected_yr_db)
    }
    else {print('Go read the documentation')}
  }

#READ csv
#activity data is for e.g. call_activity_data
#folder_file is e.g. read folders
read_files <- function(activity_path, folder_file)
{
  store_sheets <- paste0(activity_path, folder_file)
  
}





#subset data based on columns (ie 1,2,3,5, ...)
#this is not designed 
subset_data <- function(data, input_subset)
  {
    if (is.character(input_subset)==T)
    {
      subdata <- subset(data, select = input_subset)
      return(subdata)
    }
    else if (is.numeric(input_subset)==T)
    {
      cols <- colnames(data)[input_subset]
      subdata <- subset(data, select=cols)
      return(subdata)
    } 
    else {print('Go read the documentation.')}
  } 


merge_data <- function(main_file, file_to_merge, ID, subset, subset_cols)
  {
    if (subset=='Y')
    {
      file_db <- merge(main_file, file_to_merge, by=as.character(ID))
      
      if (is.numeric(subset_cols)==T)
      {
        all_cols <- paste0(1,2,3, subset_cols)
        subset_db <- subset_data(file_db, all_cols)
        return(subset_db)
      }
      else if (is.character(subset_cols)==T)
      {
        id_cols <- colnames(main_file[c(1,2,3)])
        all_id_cols <- paste(id_cols, subset_cols)
        subset_db <- subset_data(file_db, all_id_cols)
        return(subset)
      }
    }
    else if (subset != 'Y')
    {
      print('You must be out of your mind.')
      print('Please go read the documentation.')
    }
  }




