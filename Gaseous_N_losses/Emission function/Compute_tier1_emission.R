#====================================================================================================#
#####################################  Tier 1 emission ###############################################
#====================================================================================================#"
###################### ONLY TO BE APPLIED FOR N-NOX AND N-N2O EMISSIONS ##############################

#submodules used
source('./Main_functions.R')
source('./Gaseous_functions.R')

#selects the emission factor for either NOx or N2O.
select_ef <- function(gas_name)
{
	cond <- c('NOx', 'N2O')

	if (gas_name == cond[1])
	{
		ef_no <-  0.04*14/30 #kg n-no/kg N applied (EMEP, 2016)
	}
	else if (gas_name == cond[2])
	{
		ef_no <- 0.01 #kg n-n2o/kg N applied (IPCC, 2006)
	}
	else {print('Choose either NOx or N2O.')}
}

#loads the required activity of manure application to soil
call_manure_app <- function(year)
{
	#call manure applied to the soil
	manure_path <- './Manure_application_module/Output/'
	manure_app <- disaggregate_year(store_folder_files(manure_path), year)
	manure_app <- read_disagg_files(manure_app, manure_path, year)

	manure_df <- as.data.frame(manure_app, col.names=colnames(manure_app))
	return(manure_df)
}

#loads the N-input for the specified year
call_n_input <- function(year)
{
	#call N-inputs
	gnb_path <- select_maindata_pattern('GNB')
	gnb_store <- store_folder_files(gnb_path)
	input_files <- identify_pattern(gnb_store, 'Input')
	yr_input <- disaggregate_year(input_files, year)

	n_input_yr_df <- read.csv(yr_input)

	return(n_input_yr_df)
}

#selects the specified column from nox_call_n_input
select_inputs_cols <- function(year)
{
	main_df <- call_n_input(year)
	cond <- c('sludge', 'fertiliser') 
	main_df <- subset(main_df, select=cond)

	return(main_df)
}

#computes n-nox emissions from manure applied to the soil
#and writes it down to the corresponding activity data folder
compute_manure_app <- function(year, gas_name)
{
	#calls activity data
	main_df <- call_manure_app(year)
	store <- create_main_csv()
	uaa_yr <- load_uaa(year)

	ef <- select_ef(gas_name) #call Ef

	for (i in 4:ncol(main_df))
	{
		main_df[i] <- main_df[i]*ef #calculates kg n-no per kg N applied

		emission_uaa <- main_df[i]/uaa_yr #calculates n-no/ha 
		store[i] <- emission_uaa
	}
	#prepare to export the output as kg n-no/ha
	year <- year_prefix(year)
	filename <- paste0('tier1_manure', year)
	store_output <- subset_data(store, c(1,2,3, ncol(store))) #specify
	colnames(store_output)[ncol(store_output)] <- 'm_app_nha'

	export_to_activity(gas_name, 'Application', store_output, filename)

	return(main_df)
}


#compute the n-nox emissions from sludge and fertiliser (in kg N/ha)
compute_org_fert <- function(year, gas_name)
{
	main_df <- select_inputs_cols(year)
	names_df <- colnames(main_df)
	ef <- select_ef(gas_name) #call Ef
	store <- create_main_csv() 

	for (i in 1:length(main_df))
	{
		compute_emission <- main_df[i]*ef
		export_df <- cbind(store, compute_emission)
		colnames(export_df)[ncol(export_df)] <- names_df[i]

		#export to activity data
		yr_char <- year_prefix(year)
		filename <- paste0('tier1_', names_df[i], yr_char)
		export_to_activity(gas_name, 'Application', export_df, filename)

	}

	return(store)
}

#====================================================================================================#
#####################################  NOx specific func #############################################
#====================================================================================================#"

#selects grazing data for the specified year
nox_select_graz_data <- function(year)
{
  rf_path <- gaseous_select_source(pattern = 'Run', source = 'Application')
  rf_files <- store_folder_files(rf_path)
  graz_files <- identify_pattern(rf_files, 'graz')
  graz_files_yr <- disaggregate_year(graz_files, year)

  return(graz_files_yr)
}

#computes n-nox/ha emission of grazing
nox_compute_grazing <- function(year)
{
  #load activity data
  graz_data <- nox_select_graz_data(year)
  graz_tot <- select_ncol(graz_data)[[1]]

  uaa <- load_uaa(year)
  store <- create_main_csv() 
  ef <- select_ef('NOx') #call Ef

  compute_no <- graz_tot*ef/uaa
  export_df <- cbind(store, compute_no)
  colnames(export_df)[ncol(export_df)] <- 'graz_nha'
  
  yr_char <- year_prefix(year)
  filename <- paste0('tier1_graz', yr_char)
  export_to_activity('NOx', 'Application', export_df, filename)
  
  return(export_df)
}


