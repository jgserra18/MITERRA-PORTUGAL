#load manure, fertiliser, grazing, sludge (in kg N)
#load N-NH3 application for manure, fertiliser, grazing and sludge (in kg N-NH3)
#load Rf CAA db

#correct the inputs and losses per the Rf CAA db

#subtract N and N-NH3 for each source = N_activity
#multiply N_activity per Rf fraction = N_runoff
#aggregate N_runoff per the municipality
#divide N_runoff_municipality per the UAA

source('./Gaseous_N_losses/Emission function/Compute_tier1_emission.R')
source('./Main_functions.R')
source('./Gaseous_functions.R')
#source('./Runoff_module/Function/Compute_irrigation_runoff.R')


#LIBRARY
library(dplyr)

print_pseudocode <- function()
{
	print('load manure, fertiliser, grazing, sludge (in kg N)\n')
	print('load N-NH3 application for manure, fertiliser, grazing and sludge (in kg N-NH3)')
	print('load Rf CAA db')
	print('correct the inputs and losses per the Rf CAA db')
	print('subtract N and N-NH3 for each source = N_activity')
	print('multiply N_activity per Rf fraction = N_runoff')
	print('aggregate N_runoff per the municipality')
	print('divide N_runoff_municipality per the UAA')
}

##====== EXPORT TOTAL N OF ORGANIC FERTILISERS TO RUNOFF ACTIVITY DATA =====##
##############################################################################

#Sub-function of load_org_fert_activity_data
export_org_fert_data <- function(file, year, filename)
{
	yr <- year_prefix(year)
	if (missing(filename)==TRUE)
	{
	  filename <- colnames(file[ncol(file)])
	  full_name <- paste0(filename, yr)
	}
	else 
	{
	  full_name <- paste0(filename, yr)
	}

	export_to_activity('Run', 'Application', file, full_name)
}

#Sub-function of load_org_fert_activity_data
calculate_N_org_fert <- function(file, year)
{
	uaa <- load_uaa(year)
	main_df <- file
	compute_total_N <- main_df*uaa

	return(compute_total_N)
}

#main_function
load_org_fert_activity_data <- function()
{

  yr <- c(1999, 2009)
  store <- create_main_csv()
  cond_colnames <- c('sludge', 'fertiliser')
  
  for (i in 1:length(cond_colnames))
  {

  	for (j in 1:length(yr))
  	{
		input_select <- select_inputs_cols(yr[j])
		input_data <- subset_data(input_select, cond_colnames[i])

		input_data_totN <- calculate_N_org_fert(input_data, yr[j])

		main_df <- cbind(store, input_data_totN)
		filename <- colnames(input_data[ncol(input_data)])

		export_org_fert_data(main_df, yr[j])
	}
  }
}

#organizes all the activity data of runoff per year
#merging all the inputs 
#returns a list whose index1 is for 1999 and index2 for 2009
list_rf_data_year <- function()
{
	rf_data_path <- gaseous_select_source(pattern = 'Runoff', source = 'Application')
	rf_folders <- store_folder_files(rf_data_path)

	yr <- c(1999, 2009)
	store <- vector(mode='list', length=2)
	names(store) <- c('store99', 'store09')


	for (i in 1:length(yr))
	{
		main_df <- create_main_csv()
		store_files_to_read <- disaggregate_year(rf_folders, yr[i]) 

		for (j in 1:length(store_files_to_read))
		{
			#read_files <- read_disagg_files(store[j], rf_data_path, yr[i]) this is also a viable option
			read_file_ncol <- select_ncol(store_files_to_read[j])[[1]]
			main_df <- cbind(main_df, read_file_ncol)
			colnames(main_df)[ncol(main_df)] <- paste0(
					select_ncol(store_files_to_read[j])[[2]],
					year_prefix(yr[i]))

		}
		print(paste0('All files were read for ', yr[i], ' were read.'))

		#hardcoded
		colnames(main_df)[5] <- 'grazing'
		colnames(main_df)[6] <- 'manure_application'

		store[[i]] <- main_df
	}

	return(store)
}	

#============================================================================#
#=============================== N-NH3 DATA =================================#
##############################################################################

#returns the fullpath of the merged n-nh3 application files
load_nh3_application_sources <- function()
{
	path <- './Gaseous_N_losses/N-NH3/Output/'
	all_files <- list.files(path, pattern='Application')
	full_path <- paste0(path, all_files)

	return(full_path)
}

#returns a df for the selected year
df_nh3_application_source_yr <- function(year)
{
	main_df <- load_nh3_application_sources()
	yr <- year_prefix(year)
	main_df_yr <- identify_pattern(main_df, yr)
	read_main_df_yr <- read.csv(main_df_yr)

	return(read_main_df_yr)
}

#computes emissions in kg n-nh3 per source
compute_nh3_emission <- function(year)
{
	select_source_yr <- df_nh3_application_source_yr(year)
	uaa <- load_uaa(year)

	main_df <- create_main_csv()

	compute_nh3 <- uaa*select_source_yr[, 4:ncol(select_source_yr)]
	main_df <- cbind(main_df, compute_nh3)

	return(main_df)
}


#=========================================================================================#
#========================= SORTING OUT THE COLS FOR ANALYSIS =============================#

#return the correct indexes for the names_cond
#used to sort all the columns
cond_col_names_index <- function(file_to_check)
{
	names_cond <- c('graz', 'fert', 'sludge', 'man')
	col_names <- colnames(file_to_check)

	col_index <- c()

	for (i in 1:length(names_cond))
	{

		for (j in 1:length(col_names))
		{

			if (grepl(names_cond[i], col_names[j])==T)
			{
				col_index[i] <- j

			}

		}

	}

	return(col_index)
}

#organizes the columns acording to cond_col_names_index
#returns a list with indx1 of the activity data and index the n-nh3 activity data
sort_col_names <- function(rf_activity_data, nh3_activity_data)
{
  list_db <- list(rf_activity_data, nh3_activity_data)
  
  for (i in 1:length(list_db))
  {
    correct_col_indexes <- cond_col_names_index(list_db[[i]])
    indx <- c(1,2,3, correct_col_indexes)
    list_db[[i]] <- list_db[[i]][indx]
  }
  
  return(list_db)
}

#computes the difference of total N in application and N-NH3 losses
compute_runoff_activity_data <- function(sort_col_names_output, year)
{
	list_db <- sort_col_names_output
	uaa <- load_uaa(year)
	main_df <- create_main_csv()

	for (i in 4:7)
	{
		compute_net_N <- (list_db[[1]][i]-list_db[[2]][i])/uaa
		main_df <- cbind(main_df, compute_net_N)
	}

	#correct sludge negative values
	#note: values are negligible (e.g. 0.02 kg N/ha, and 77kg N in a municipality)
	#due to rounding
	main_df[, 6] <- ifelse(main_df[, 6]<0, 0, main_df[, 6])

	return(main_df)	
}

#=========================== CALCULATE RUNOFF LOSSES ====================================#
##########################################################################################

#load runoff fractions sheet at the CAA_ID
load_rf_fraction <- function(year)
{
	rf_data_path <- gaseous_select_source(pattern = 'Runoff', source = 'Runoff')
	rf_store_files <- store_folder_files(rf_data_path)
	rf_yr <- disaggregate_year(rf_store_files, year)

	rf_df <- read.csv(rf_yr)

	return(rf_df)
}

#note: main_df - output of compute_runoff_activity_data
organize_rf_data_CAA <- function(main_df, year)
{
	rf_main_df <- load_rf_fraction(year)
	df_to_merge <- main_df

	cols_select <- c(seq(1,8), seq(11,14)) #hardcoded
	merged_df <- merge(rf_main_df, df_to_merge, 'Muni_ID')
	merged_df <- subset(merged_df, select=cols_select)

	return(merged_df)

}

#identifies the column with specified pattern 
#E.g. "rf" for runoff fractions or poly_corr for the CAA_polygon areas
identify_rf_col <- function(main_df, pattern)
{
	cond <- grepl(as.character(pattern), colnames(main_df))

	ifelse(cond==T,
		return(main_df[cond[TRUE]]),
		print('No such thing.')
		)

}

#computes total N for each source at the CAA
#this will be used to calculate the runoff losses
compute_runoff_losses_caa <- function(main_rf_caa_df)
{
	main_df <- main_rf_caa_df
	poly_caa_ha <- identify_rf_col(main_df, 'corr')
	rf_frac_caa <- identify_rf_col(main_df, 'rf')
	
	start_index <- ncol(main_df)-3

	for (i in start_index:ncol(main_df))
	{
		main_df[i] <- poly_caa_ha*main_df[i]*(rf_frac_caa/100) #calculate runoff losses in N per CAA
		main_df[is.na(main_df)] <- 0
	}

	return(main_df)
}

#uses to dplyr to calculate the sum of the CAA polygons per municipality ID
runoff_sum_muni <- function(runoff_losses_caa_df)
{	
	main_df <- runoff_losses_caa_df
	colnames(main_df)[seq(9, 12)] <- c('grazing', 'fertiliser', 'sludge', 'manure_application')

	sum_cols = main_df %>%
		group_by(Muni_ID) %>%
		summarise(
			sum_grazing = sum(grazing),
      		sum_fertiliser = sum(fertiliser),
      		sum_sludge = sum(sludge),
      		sum_manure = sum(manure_application)
      )

	return(sum_cols)
}

#computes the runoff losses per municipality
organize_N_runoff_muni <- function(runoff_losses_caa_df, year)
{
	main_df <- runoff_sum_muni(runoff_losses_caa_df)
	muni_df <- create_main_csv()
	df <- merge(muni_df, main_df, by='Muni_ID', sort=FALSE)
	return(df)

}

compute_runoff_losses <- function(runoff_losses_caa_df, year, irrig_mode)
{
	runoff_df <- organize_N_runoff_muni(runoff_losses_caa_df)
	uaa <- load_uaa(year)

	for (i in 4:ncol(runoff_df))
	{
		runoff_df[i] <- runoff_df[i]/uaa
	}

	colnames(runoff_df)[c(4,5,6,7)] <- c('grazing', 'fertiliser', 'sludge', 'manure')

	if (irrig_mode=='ON')
	{
	  irrig_rf <- add_runoff_irrig_dataset(year)
	  runoff_df <- cbind(runoff_df, irrig_rf)
	}
	else if (missing(irrig_mode)==TRUE | irrig_mode=='OFF')
	{
	  print('This calculates the runoff (in kg N/ha).')
	  return(runoff_df)
	}
}

#calculates the total runoff losses irrespective of source
#unit: kg N/ha
NS_runoff_total_nha <- function(runoff_losses_per_source, irrig_mode)
{
	runoff_df <- runoff_losses_per_source #output of compute_runoff_losses
	main_df <- create_main_csv()
	
	ifelse(irrig_mode=='OFF' && grepl('irrig', colnames(runoff_df)[ncol(runoff_df)])==TRUE,
	       nc <- ncol(runoff_df)-1,
	       nc <- ncol(runoff_df))
	       

	total_runoff <- rowSums(runoff_df[, 4:nc])
	main_df <- cbind(main_df, total_runoff)
	colnames(main_df)[4] <- 'runoff_nha'

	print('This calculates the TOTAL runoff losses irrespective of source (in kg N/ha).')

	return(main_df)
}


write_runoff_output <- function(data_to_write, name, year)
{
	yr <- year_prefix(year)
	fullname <- paste0(name, yr)

	write_output('Runoff', data_to_write, fullname)

}