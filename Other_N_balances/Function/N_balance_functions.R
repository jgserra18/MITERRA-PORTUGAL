#load used submodules
source('./Main_functions.R')
source('./Gaseous_functions.R')

#returns  data for the specified year
#e.g runoff, GNB
select_output_data <- function(pattern, year, pattern2)
{
  ifelse(missing(pattern),
         pattern <- 'GNB',
         pattern <- pattern)
  
  db_path <- select_module_output(pattern=pattern)
  db_output <- store_folder_files(db_path)
  
  if (missing(pattern2)==F)
  {
    cond <- grepl(pattern2, db_output)
    db_output <- db_output[which(cond==T)]
    
  }
  
  db_yr <- disaggregate_year(db_output, year)
  read_yr <- read.csv(db_yr)
  yr_final <- read_yr[ncol(read_yr)]#select only gnb col
  
  return(yr_final)
}

#data is the output of select_maindata_pattern (i.e. gas_final_db)
#this filteres the irrigation main db
filter_irrig_maindb <- function(data, irrig_mode)
{
  if (irrig_mode==TRUE)
  {
    id <- grepl('irrig', data)
    condition <- which(id==TRUE)
    select_data <- data[condition]
  }
  else if (irrig_mode==FALSE)
  {
    id <- grepl('irrig', data)
    condition <- which(id==FALSE)
    select_data <- data[condition]
  }
  return(select_data)
}

#returns gaseous data for the specified year
gas_output_data <- function(pattern1, year, tier, irrig_mode)
{
	print('By default this is applied for NH3.')

	pattern <- c('Gaseous_N_losses', pattern1)

	gas_db_output <- select_module_output(pattern=pattern)

	#select only the path of the total NH3 db to select
	#FILES MUST BE NAME WITH MAIN
	gas_final_db <- select_maindata_pattern(pattern='Main', path=gas_db_output) 
	
	##filter irrigation
	if (missing(irrig_mode)==TRUE){irrig_mode <- FALSE}
	gas_final_db <- filter_irrig_maindb(gas_final_db, irrig_mode)

	if (missing(tier)==FALSE)
	{
		gas_final_db  <- gas_final_db[which(grepl(tier, gas_final_db)==TRUE)]
	}

	gas_files_yr <- disaggregate_year(gas_final_db, year)
	read_gas_yr <- read.csv(gas_files_yr) #read_disagg_yr

	return(read_gas_yr)
}

tier_output_data <- function(year, tier)
{
	pattern <- c('Gaseous_N_losses', 'N2O')
	n2o_db_output <- select_module_output(pattern=pattern)

	tier_db  <- n2o_db_output[col_id_by_name(tier, n2o_db_output)]
}

#loads runoff data for the SSNB calculation
runoff_output_data <- function(pattern, year, pattern2)
{
	ifelse(pattern2=='total',
		runoff_db <- select_output_data(pattern, year,'total'),
		runoff_db <- select_output_data(pattern, year,pattern2))

	return(runoff_db)
}

#loads other manure usage for the SSNB calculation
other_manure_output_data <- function(year, pattern)
{	
	pattern_path <- c('Other_N_balances', 'SSNB')

	ifelse(pattern=='manure',
		other_manure_db <- select_output_data(pattern_path, year, pattern),
		print('Please, pattern must be "manure".'))

	other_manure_db <- data_cleaning(other_manure_db)

	return(other_manure_db)
}

#====================================================================================================#
#################################### EXPORT CSV OF N BALANCE ###########################################
#====================================================================================================#
#note: pattern2 is either NS or SSNB
write_output_n_balance <- function(data_to_write, name, pattern2)
{
	write_output("Other_N_balances", data_to_write, name, pattern2)
}

#====================================================================================================#
#################################### NITROGEN SURPLUS (NS) ###########################################
#====================================================================================================#
#returns gaseous emissions for housing + storage
#only applied for n-nh3
ns_select_gas <- function(pattern1, year)
{
	gas_data <- gas_output_data(pattern1, year)
	df <- create_main_csv()

	pathway_cond <- c('housing', 'storage')

	for (i in pathway_cond)
	{
		cond_id <- which(grepl(i, colnames(gas_data))==T)
		df <- cbind(df, gas_data[cond_id])
	}

	df$b4_app <- rowSums(df[, 4:ncol(df)])
	gas_final_df <- df[ncol(df)]

	return(gas_final_df)
}

#only for the GNB
#computes the nitrogen surplus
#NS = GNB - NH3_housing - NH3_storage
#d <- ns_compute('NH3', 2009, 'ON')
ns_compute <- function(pattern1, year, irrig_mode)
{
	main_df <- create_main_csv()
	
	ifelse(irrig_mode=='ON', irrig_mode <- '_irrig', irrig_mode <- 'default') #irrigatioN?
	
	gnb_data <- select_output_data(year=year,pattern2 = irrig_mode)
	nh3_data <- ns_select_gas(pattern1, year)

	ns_compute <- gnb_data-nh3_data
	main_df <- cbind(main_df, ns_compute)
	colnames(main_df)[4] <- paste0(irrig_mode, '_ns')

	return(main_df)
}

#====================================================================================================#
##################################### SOIL SURFACE N BALANCE #########################################
#====================================================================================================#

#function compiles all the total gaseous emissions and selects only last col (total)
#returns the template db + total of n2o, nh3 and nox
#ssnb_compile_tot_gaseous(2009, 'tier2', TRUE)
ssnb_compile_tot_gaseous <- function(year, tier, irrig_mode)
{
	gas_cond <- c('NH3', 'N2O', 'NOx')
	main_df <- create_main_csv()
	
	ifelse(irrig_mode=='ON', irrig_mode <- TRUE, irrig_mode <- FALSE) #irrigatioN?
	
	for (i in gas_cond)
	{
		if (i=='N2O')
		{
			gas_data <- gas_output_data(i, year, tier, irrig_mode)
			fullname <- paste0(tier, '_tot_', i)
		}
		else 
		{
			gas_data <- gas_output_data(i, year)
			fullname <- paste0('tot_', i)
		}
		gas_data <- gas_data[ncol(gas_data)] #select last col (total)

		main_df <- cbind(main_df, gas_data)
		colnames(main_df)[ncol(main_df)] <- fullname

	}
	return(main_df)
}

#compute the total gaseous emissions (nh3+n2o+nox)
#d <- ssnb_compute_tot_gaseous(2009, 'tier2', 'ON')
ssnb_compute_tot_gaseous <- function(year, tier, irrig_mode)
{
  ifelse(irrig_mode=='ON', irrig_mode <- 'ON', irrig_mode <- 'OFF') #irrigatioN?
  
	main_gas_df <- ssnb_compile_tot_gaseous(year, tier, irrig_mode)
	tot_gas_df <- rowSums(main_gas_df[, 4:ncol(main_gas_df)])
	
	return(tot_gas_df)
}

#computes the ssnb = GNB - tot_gas_emissions
#d <- ssnb_compute(2009, 'tier2', 'ON)
ssnb_compute <- function(year, tier, irrig_mode)
{
	main_df <- create_main_csv()
	
	if (irrig_mode=='ON')
	{
	  pattern2 <- TRUE
	  gnb_pattern <- '_irrig'
	  runoff_data <- runoff_output_data('Runoff', year, 'irrig')
	}
	else if (irrig_mode=='OFF')
	{
	  pattern2 <- FALSE
	  gnb_pattern <- 'default'
	  runoff_data <- runoff_output_data('Runoff', year, 'total_muni')
	}
	#load the data
	gnb_data <- select_output_data(year=year, pattern2 = gnb_pattern) 
	gas_data <- ssnb_compute_tot_gaseous(year, tier, irrig_mode)
	#runoff_data <- runoff_output_data('Runoff', year, 'total_muni_irrig')
	other_manure_data <- other_manure_output_data(year, 'manure')
  
	#compute the SSNB
	ssnb <- gnb_data-gas_data-runoff_data-other_manure_data
	main_df <- cbind(main_df, ssnb)
	colnames(main_df)[4] <- 'ssnb'

	return(main_df)
}

