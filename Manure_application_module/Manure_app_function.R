#NOTE: THIS IS THE MAIN ACTIVITY DATA TO CALCULATE N-N2O, N-NOX, RUNOFF estimates

#this modules calculates the correct amount of manure applied to soil
#activity data: 
#---> "gross" manure application (ie without Nh3 losses)
#---> manure_application_rates per municipality
#the remaining portion of manure_application_rates equals to "manure_other_usage" --> SSNB module

source('./Main_functions.R')
source('./Gaseous_functions.R')


read_me <- function()
{
	text <- 'This first calculates the manure applied to the soil (manure b4 app * app_rates)\nand then calculates the other manure usage (i.e. manure b4 app*(1-app_rates)'

	path <- 'C:/Users/Serra/OneDrive/Major project/MITERRA-PORTUGAL/Manure_application_module/'
	write.table(text, paste0(path, 'read_me.txt'), sep='\t', row.names =F, col.names = F)
}

#this loads the UAA and template csv files
#returns list
load_app_rates <- function()
{
  #see rounding noumbers
  path <- select_maindata_pattern('Manure')
  file <- store_folder_files(path)[3]
  #file <- store_folder_files(man_app_data_path)[3]
  file <- as.data.frame(select_ncol(file)[[1]])
  colnames(file)[1] <- 'app_rate'
  
  return(file)
}


#NOTE: NEED TO CHANGE THIS FUNCTION
#function to compute manure applied to soil
compute_soil_app <- function(man_yr)
{
  app_rates <- load_app_rates()

  for (i in 4:ncol(man_yr))
  {
    man_yr[i] <- round(app_rates$app_rate*man_yr[i], 0) #compute N applied to soil
  }
  
  return(man_yr)
}

#NOTE: NEED TO CHANGE THIS FUNCTION
#this computes other manure usage (e.g. burning)
compute_manure_usage <- function(man_yr, year)
{
  app_rates <- load_app_rates() #load manure application rates
  uaa <- load_uaa(year) #load uaa per year
  template_file <- create_main_csv()

  tot_man <- man_yr[ncol(man_yr)]
  oth_manure <- round(tot_man*(1-app_rates$app_rate), 0)
  oth_manure_nha <- round(oth_manure/uaa, 2)
  
  df_oth_manure <- cbind(template_file, c(oth_manure, oth_manure_nha))
  colnames(df_oth_manure)[4] <- 'total_N'
  colnames(df_oth_manure)[5] <- 'total_nha'
  
  df_oth_manure[is.na(df_oth_manure)] <- 0
  
  return(df_oth_manure)
}

