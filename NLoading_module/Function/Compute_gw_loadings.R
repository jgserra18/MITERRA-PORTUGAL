#load leaching (in kg N)
#load prec
#calculate drainage (prec*rech_rate)
	#NOTE: leaching has to be recalculated due to fragmentation of polygons (to upscale to aquifer)
	#leaching (kg N/ha)*new_area_corrected
	#calculate N-loadings at the caa scale
	#upscale to CAA scale
	#upscale total N leached and drainage for each aquifer
	#convert caa_area ha to m2
	#compute mg N/L

source('./Main_functions.R')
source('./Gaseous_functions.R')
source('./Runoff_module/Function/Compute_runoff_application.R')
source('./Leaching_module/Function/Compute_leaching.R')
source('./Leaching_module/Function/compute_correct_surface_water_areas.R')

library(dplyr)
library(data.table)

tier_condition <- function(year, tier_leaching)
{
  ifelse(grep('irrig', tier_leaching) == T, tier <- 'source_leaching_irrig', tier <- 'source_leaching')
  tier <- paste0(tier, year_prefix(year))
  
  return(tier)
}

#load precipitation data
#select == T only 1st and last col
load_prec <- function(year, select)
{
	prec_path <- gaseous_select_source('Climatic', source='Precipitation')
	store_prec_files <- store_folder_files(prec_path)
	prec_yr <- disaggregate_year(store_prec_files, year)
	read_prec_yr <- read.csv(prec_yr)

	if(missing(select)==FALSE)
	{
		prec_df <- read_prec_yr[, c(1, ncol(read_prec_yr))]
		return(prec_df)
	}
	else 
	{
		return(read_prec_yr)
	}
}

#by default this related to the tier2 leaching calculation
#note: select_output_data  is only possible to use since it has the same structure!
#however, select_output_data could be improved by adding a parameter to specify last col or not
#tier can be either 'tier2_muni_db' or 'tier2_irrig_muni')
load_leaching_muni <- function(year, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
	
	main_df <- create_main_csv()
	leach_muni <- select_output_data('Leaching', year, tier_leaching) 

	main_df <- cbind(main_df, leach_muni)
	main_df <- main_df[, c(1,4)] #only Muni_ID and leaching

	return(main_df)
}

#loads the main gw dataset
#this contains "repeated" CAA_ID composed of even smaller polygons, whose areas
#were calculated
load_gw_db <- function(year)
{
	gw_db <- select_maindata_pattern('Groundwater')
	gw_files <- store_folder_files(gw_db)
	gw_db_yr <- disaggregate_year(gw_files, year)
	gw_yr_file <- read.csv(gw_db_yr)

	return(gw_yr_file)
}

merge_dataa <- function(main_df, df_to_merge, id_name)
{
	merged_df <- merge(main_df, df_to_merge, by=id_name, 
						all.x=TRUE, sort=FALSE)
	return(merged_df)
}

#IGNORE THIS |! WRONG LEACHING !!!
#CREATE MAIN DF WITH PREC, GW DATA AND LEACHING DATA
ignore_create_main_df <- function(year, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  main_df <- load_gw_db(year)

	nl_muni <- load_leaching_muni(year, tier_leaching)
	prec_caa <- load_prec(year, TRUE)

	main_df <- merge_dataa(main_df, nl_muni, 'Muni_ID')
	main_df <- merge_dataa(main_df, prec_caa, 'CAA_ID')

	return(main_df)
}

#THIS GETS THE LEACHING TO DEEPER GROUNDWATER (IN KG N), CALCULATES IT IN KG N/HA 
prepare_leaching_to_groundwater <- function(year, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  gw_leach_file <- get_module_output('Leaching', tier_leaching, year)
  caa_df <- create_caa_df(year)
  caa_df$leaching_gw <- round(gw_leach_file$leaching_gw/gw_leach_file$poly_corr, 5)
  caa_df <- data_cleaning(caa_df)
  caa_df <- caa_df[, c(2,5)]
  
  return(caa_df)
}

#CREATE MAIN DF WITH PREC, GW DATA AND LEACHING DATA
create_main_df <- function(year, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  main_df <- load_gw_db(year)
  gw_leach_caa <- prepare_leaching_to_groundwater(year, tier_leaching)
  prec_caa <- load_prec(year, TRUE)
  
  main_df <- merge_dataa(main_df, gw_leach_caa, 'CAA_ID')
  main_df <- merge_dataa(main_df, prec_caa, 'CAA_ID')
  
  return(main_df)
}

#================================================================================#
############################### COMPUTATION FUNCS ################################
compute_drainage_litre <- function(year, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  main_df <- create_main_df(year, tier_leaching)

	poly_caa_m2 <- identify_rf_col(main_df, 'corr')*10000
	rech_rate <- identify_rf_col(main_df, 'rech')/100
	prec_m <- identify_rf_col(main_df, 'prec')/1000

	rech_litre <- (prec_m*rech_rate*poly_caa_m2)*1000
	rech_litre <- data_cleaning(rech_litre)
	colnames(rech_litre)[1] <- 'drainage_litre'

	return(rech_litre)
}

#computes leaching (in kg N) at the CAA_v2 (uncorrected one)
compute_leaching_mg <- function(year, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  main_df <- create_main_df(year, tier_leaching)

	poly_caa_ha <- identify_rf_col(main_df, 'corr')
	leaching_nha <- identify_rf_col(main_df, 'leaching')

	leaching_mg <- (poly_caa_ha*leaching_nha)*1000000
	leaching_mg <- data_cleaning(leaching_mg)
	colnames(leaching_mg)[1] <- 'leaching_mg'

	return(leaching_mg)
}

#computes agricultural n loadings in mg N/L
compute_nloading <- function(year, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  
	drainage_l <- compute_drainage_litre(year, tier_leaching)
	leaching_mg <- compute_leaching_mg(year, tier_leaching)

	nloading <- leaching_mg/drainage_l
	nloading <- data_cleaning(nloading)
	colnames(nloading)[1] <- 'nloading'

	return(nloading)
}

#summarizes all into a main dataset
#scale - incorrect CAA scale/GW scale
aggregate_gw_dataset <- function(year, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  
	drainage_l <- compute_drainage_litre(year, tier_leaching)
	leaching_mg <- compute_leaching_mg(year, tier_leaching)
	nloading <- compute_nloading(year, tier_leaching)

	main_df <- create_main_df(year, tier_leaching)
	main_df <- cbind(main_df, drainage_l, leaching_mg, nloading)

	return(main_df)
}

#================================================================================#
############################### LOADD CAA_ID DATA ################################

#loads the CAA_ID col only (NOT THE CORRECTED)
load_incorr_CAA_ID <- function(year, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  main_df <- create_main_df(year, tier_leaching)
	main_df <- as.data.frame(main_df[, 1])
	colnames(main_df)[1] <- 'CAA_ID'

	return(main_df)
}

#loads correct CAA_ID
load_corr_CAA_ID <- function(year)
{
	prec_df <- load_prec(year)
	corr_caa_id <- as.data.frame(prec_df[, 1])
	colnames(corr_caa_id)[1] <- 'CAA_ID'

	return(corr_caa_id)
}

#================================================================================#
################################ CORRECT CAA_ID ##################################
################################# FOR PLOTTING ###################################

#this upscales the leaching in mg N, and drainage in litre, to the correct CAA scale
#NOTE: USED FOR PLOTTING ONLY
caa_corr_input_data <- function(year, tier_leaching)
{
	incorr_caa_id <- load_incorr_CAA_ID(year, tier_leaching)
	corr_caa_id <- load_corr_CAA_ID(year)
	leaching<- compute_leaching_mg(year, tier_leaching)
	drainage_litre <- compute_drainage_litre(year, tier_leaching)

	df_leach <- cbind(incorr_caa_id, leaching)
	df_drainage <- cbind(incorr_caa_id, drainage_litre)

	corr_leaching <- sumif(df_leach, 'CAA', 'leaching')
	corr_drainage <- sumif(df_drainage, 'CAA', 'drainage')
	correct_df <- cbind(corr_caa_id, corr_drainage,corr_leaching)

	return(correct_df)
}

#FIX THIS
#computes the correct N-loadings (in mg N/L) to the correct CAA scale
#NOTE: USED FOR PLOTTING ONLY
compute_gw_leaching <- function(year, tier_leaching)
{
	main_df <- caa_corr_input_data(year, tier_leaching)
	#conditional for computw_gw_leaching
	#ifelse(grepl('irrig', tier_leaching)==T, tier_ssnb <- 'tier2_irrig', tier_ssnb <- 'tier2_ssnb')
	
	#leaching <- compute_leaching_gw(year, tier_ssnb)[, c('CAA_ID', 'leaching_gw')]
	
#	leaching$leaching_gw <- leaching$leaching_gw*1000000
	#main_df <- merge(main_df, leaching, 'CAA_ID')

	main_df$nloading <- round(main_df$leaching/main_df$drainage, 2)
	main_df <- data_cleaning(main_df)
	
	return(main_df)
}

#================================================================================#
################################## GW LOADINGS ###################################

#sorts by aquifer_ID 
sort_by <- function(df, sort_by_name)
{
	id <- col_id_by_name(sort_by_name, df)
	id_col <- df[, id]
	id_col_unique <- unique(id_col)

	sort_id_col <- as.data.frame(sort(id_col_unique))
	colnames(sort_id_col)[1] <- sort_by_name

	return(sort_id_col)
}

#sums drainage and leaching for each aquifer
#creates a dataset with each specified by_col_name and respective drainage and leaching
#NOTE: GENERAL FUNCTION
compute_leaching_drainage_func <- function(year, by_col_name, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  
	main_gw_df <- aggregate_gw_dataset(year, tier_leaching)
	df <- sort_by(main_gw_df, by_col_name)

	cond_names <- c('leaching_mg', 'drainage')

	for (i in 1:length(cond_names))
	{
		sum <- sumif(main_gw_df, by_col_name, cond_names[i])
		df <- cbind(df, sum)

		colnames(df)[i+1] <- cond_names[i]
	}
	return(df)
}

#GENERAL FUNCTION TO COMPUTE NLOADING
#NOTE: COL 2 AND 3 MUST BE LEACHING AND DRAINAGE
compute_nloading_func <- function(df, year)
{
	main_df <- df

	main_df$nloading <- round(main_df[, 2]/main_df[, 3], 5)
	#main_df <- cbind(main_df, nloading)
	#colnames(main_df)[ncol(main_df)] <- 'nloading'

	return(main_df)
}

#computes leaching and drainage for each aquifer_ID and nloadings
sum_by_gw_id <- function(year, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  
	main_df <- compute_leaching_drainage_func(year, 'aquifer_ID', tier_leaching)
	main_df_complete <- compute_nloading_func(main_df, year)
	main_df_complete <- data_cleaning(main_df_complete)

	return(main_df_complete)
}

#selects a new dataframe with aquifer_ID and gw_type
gw_typology_identifier <- function(year, rech, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  
	main_df <- aggregate_gw_dataset(year, tier_leaching)
	gw_type <- col_id_by_name('gw_type', main_df)
	aqui_id <- col_id_by_name('aquifer_ID', main_df)

	if (missing(rech)==TRUE)
	{
		new_df <- main_df[, c(aqui_id, gw_type)]
	}
	else 
	{
		rech_id <- col_id_by_name('recharge', main_df)
		new_df <- main_df[, c(aqui_id, gw_type, rech_id)]
	}
	return(new_df)
}

#organizes this new dataset for only unique numbers
gw_typology_organize <- function(year, rech, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  
	if (missing(rech)==TRUE)
	{
		new_df <- gw_typology_identifier(year, tier_leaching=tier_leaching)

		new_df = new_df %>%
			distinct(aquifer_ID, gw_type)
	}
	else 
	{
		new_df <- gw_typology_identifier(year, 'yes', tier_leaching)
		new_df = new_df %>%
			distinct(aquifer_ID, gw_type, recharge)
	}
	return(new_df)
}

#this creates the complete dataset for groundwater bodies
#aquifer_id, gw_type, leaching, drainage, nloading
gw_complete_dataset <- function(year, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  
	main_df <- sum_by_gw_id(year, tier_leaching)
	gw_type_df <- gw_typology_organize(year, tier_leaching=tier_leaching)
	main_df <- merge_dataa(main_df, gw_type_df, 'aquifer_ID')

	return(main_df)
}

#================================================================================#
################################## MUNI LOADINGS ###################################
#computes leaching and drainage for each Muni_ID
nloading_muni <- function(year, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  
	main_df <- compute_leaching_drainage_func(year, 'Muni_ID', tier_leaching)
	main_df_complete <- compute_nloading_func(main_df, year)
	main_df_complete <- data_cleaning(main_df_complete)

	return(main_df_complete)

}

#aggregates the nloading estimates using the correct muni template
aggregate_nloading_muni <- function(year, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  
	template_df <- create_main_csv()
	nloading_muni <- nloading_muni(year, tier_leaching)
	merged_df <- merge_dataa(template_df, nloading_muni, 'Muni_ID')
	return(merged_df)
}

write_nloading_output <- function(data_to_write, name, year)
{
	yr <- year_prefix(year)
	fullname <- paste0(name, yr)

	write_output('NLoading', data_to_write, fullname)
}

######################################################################################################
######################################################################################################
######################################################################################################
########################## CORRECT INPUT CONCENTRATION TO GW ##########################################

## ATTENTION!!!
## UPDATED FUNCTION
#corrections the leaching to leaching_gw
CORRECTED_leaching_gw_conc <- function(year, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  
  df <- compute_leaching_drainage_func(year, 'aquifer_ID', tier_leaching)
  #new_leaching_df <- load_leaching_gw(year, TRUE)
 # df <- merge(new_leaching_df, df[, c(1,3)], 'aquifer_ID')
  
  return(df)
}

CORRECT_gw_concentration <- function(year, tier_leaching)
{  
  tier_leaching <- tier_condition(year,tier_leaching)
  
  df <- CORRECTED_leaching_gw_conc(year, tier_leaching)
  df$nc <- df$leaching_mg/df$drainage
 # df[, 2] <- df[, 2]*1000000
 # df <- compute_nloading_func(df, year)
  
  return(df)
}


CORRECT_gw_complete_dataset <- function(year, tier_leaching)
{
  tier_leaching <- tier_condition(year,tier_leaching)
  
  main_df <- CORRECT_gw_concentration(year, tier_leaching)
  gw_dataset <- gw_typology_organize(year,'yes', tier_leaching)
  main_df <- merge_dataa(main_df, gw_dataset, 'aquifer_ID')
  
  return(main_df)
}


