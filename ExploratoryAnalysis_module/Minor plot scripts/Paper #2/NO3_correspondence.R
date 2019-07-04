#load N-loading@muni
#load avgNO3 from activity data/irrigation

source('./Main_functions.R')
source('./NLoading_module/Function/Compute_gw_loadings.R')
source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')

call_nloading_muni <- function(year)
{
  df <- aggregate_nloading_muni(year)
  df <- df[, c(1,2,3,6)]
  return(df)
}

#e.g load_subactivity_data('Irrigation', 'NO3 sampling', 'avg', 1999)
load_subactivity_data <- function(folder_pattern, subfolder_pattern, file_pattern, year)
{
  activity_path <- check_folder(select_maindata_pattern(folder_pattern))
  search_subfolder <- list.files(activity_path, pattern=subfolder_pattern)
  sub_folder_path <- check_folder(paste0(activity_path, search_subfolder))
  
  search_file <- list.files(sub_folder_path, pattern = file_pattern)  
  file_path <- paste0(sub_folder_path, search_file)
  file_read <- read_disagg_files(file_path, sub_folder_path, year)[[1]]

  return(file_read)
}

merge_for_correspondence_no3 <- function(year)
{
  sampling_data <- load_subactivity_data('Irrigation', 'NO3 sampling', 'avg', year)
  n_loading <- call_nloading_muni(year)
  merged_df <- merge(n_loading, sampling_data, 'Muni_ID')
  merged_df <- merged_df[, c(1,2,3,4,7)]
  return(merged_df)
}

write_no3_correspondence <- function(year, name)
{
  merged_df <- merge_for_correspondence_no3(year)
  path <- "./ExploratoryAnalysis_module/Misc-data/"
  path <- paste0(path, name, '.csv')
  fwrite(merged_df, path)
  
}



