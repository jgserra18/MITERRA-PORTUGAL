#load lf_CAA
#Load ssnb_muni
#downscale ssnb to CAA
#calculate N leached_CAA
#calculate denitrification to N2

# N leached * lf_CAA
# calculate N leached_muni
# calculate denirification (kg N/ha) at the munic and CAA scales
#calculate N-leaching (kg N/ha) at the muni and CAA scales
#export all

#import submodules
source('./Main_functions.R')
source('./Gaseous_functions.R')
source('./Other_N_balances/Function/N_balance_functions.R')
source('./Runoff_module/Function/Compute_runoff_application.R')

#libraries
library(dplyr)


read_me <- function()
{
	text <- 'This module calculates N-leaching and denitrification from the SSNB.\nThese are derived from leaching fractions at the CAA scale. As such, the calculations proceed at the CAA and then at the muni scales.'

	path <- './MITERRA-PORTUGAL/Leaching_module/'
	write.table(text, paste0(path, 'read_me.txt'), sep='\t', row.names =F, col.names = F)
}

##=======================  LOAD ACTIVITY DATA  =============================##
##############################################################################

#loads leaching fractions at the CAA scale
load_lf_fraction <- function(year)
{
	lf_data_path <- gaseous_select_source(pattern = 'Leaching', source = 'Leaching')
	lf_store_files <- store_folder_files(lf_data_path)
	lf_yr <- disaggregate_year(lf_store_files, year)

	lf_df <- read.csv(lf_yr)

	return(lf_df)
}

#correct negative values to 0
#by default ncol is the last col
correct_ncol_neative_to_zero <- function(main_df, col)
{
	ifelse(missing(col),
		col <- ncol(main_df),
		col <- col)

	correct_col <- sapply(main_df[, col], function(x)
 		{
    		replace(x, x<0, 0)
  		}
	)

	main_df[, col] <- correct_col #replaces col with corrct_col

	return(main_df)
}

#load ssnb at the municipality scale (kg N/Ha)
#whole_df can be T (all cols) or F (only last col)
load_ssnb_muni <- function(year, whole_df, tier_ssnb)
{
	pattern = c('Other_N_balances', 'SSNB')
	ssnb_df <- select_output_data(pattern, year, tier_ssnb)

	ifelse(whole_df==TRUE,
		ssnb_df <- cbind(create_main_csv(),
							ssnb_df),
		ssnb_df)

	ssnb_df <- correct_ncol_neative_to_zero(ssnb_df)
	return(ssnb_df)
}

#disaggregates ssnb_muni to only Muni_ID and ssnb cols
disagg_ssnb_muni <- function(year, tier_ssnb)
{
	cond <- c('Muni_ID', 'ssnb')

	ssnb_muni <- load_ssnb_muni(year, TRUE, tier_ssnb)
	cond_id <- integer(0)

	cond_id <- sapply(cond, function(x) which(colnames(ssnb_muni) %in% x==T))

	return(ssnb_muni[cond_id])
}

#downscales the ssnb from the muni to the caa level
#tier is either 'tier2_irrig' or 'tier2_ssnb'
downscale_ssnb_caa <- function(year, tier_ssnb)
{
	ssnb_muni_df <- disagg_ssnb_muni(year, tier_ssnb) #only muni_id and ssnb
	lf_caa <- load_lf_fraction(year)

	caa_df <- merge(lf_caa, ssnb_muni_df, by='Muni_ID', all.x=T) #all.x = T odes not add the by col

	return(caa_df)
}

#computes the ssnb (in kg N) for each CAA polygon
compute_ssnb_N_CAA <- function(year, tier_ssnb)
{
	main_df <- downscale_ssnb_caa(year, tier_ssnb)

	#load all the required columns
	poly_caa_ha <- identify_rf_col(main_df, 'corr')
	ssnb <- identify_rf_col(main_df, 'ssnb')

	compute_ssnb_N <- ssnb*poly_caa_ha

	return(compute_ssnb_N)
}

#computes total N leached per CAA polygon
#d <- compute_leaching(2009, 'tier2_irrig')
compute_leaching <- function(year, tier_ssnb)
{
	main_df <- downscale_ssnb_caa(year, tier_ssnb)

	ssnb_N_caa <- compute_ssnb_N_CAA(year, tier_ssnb) #kg N
	lf_frac_caa <- identify_rf_col(main_df, 'lf')

	compute_nl <- ssnb_N_caa*(lf_frac_caa/100)
	colnames(compute_nl)[1] <- 'leaching_N'

	return(compute_nl)
}

#computes total N lost through denitrification to N2
#d <- compute_leaching(2009, 'tier2_irrig')
compute_denitrification <- function(year, tier_ssnb)
{
	main_df <- downscale_ssnb_caa(year, tier_ssnb)

	ssnb_N_caa <- compute_ssnb_N_CAA(year, tier_ssnb) #kg N
	lf_frac_caa <- identify_rf_col(main_df, 'lf')

	compute_denitrification <- ssnb_N_caa*(1-lf_frac_caa/100)
	colnames(compute_denitrification)[1] <- 'denitri_N'

	return(compute_denitrification)
}

#load caa area
#d <- caa_load_area(2009, 'tier2_ssnb')
caa_load_area <- function(year, tier_ssnb)
{
	main_df <- downscale_ssnb_caa(year, tier_ssnb)
	poly_area <- identify_rf_col(main_df, 'corr')

	return(poly_area)
}

#computes the kg N/ha at the CAA scale
#input variable must be at CAA scale with variables in kg N
#subfunction of aggregate_to_maindf
compute_nha <- function(year, input_variable, tier_ssnb)
{
	poly_area <- caa_load_area(year, tier_ssnb)

	calc_nha <- input_variable/poly_area

	return(calc_nha)
}

#add the output of compute_leaching and denitrification to the main_df
#compute_area must be TRUE or FALSE
#dd <- aggregate_to_maindf(2009, F, 'tier2_irrig')
aggregate_to_maindf <- function(year, compute_area, tier_ssnb)
{
	main_df <- downscale_ssnb_caa(year, tier_ssnb)
	nl <- compute_leaching(year, tier_ssnb)
	denit <- compute_denitrification(year, tier_ssnb)

	if (compute_area==TRUE)
	{
		denit <- compute_nha(year, denit, tier_ssnb)
		nl <- compute_nha(year, nl, tier_ssnb)

		main_df <- cbind(main_df, denit, nl)
		colnames(main_df)[ncol(main_df)] <- 'leaching_nha'
		colnames(main_df)[ncol(main_df)-1] <- 'denit_nha'
	}
	else 
	{
		main_df <- cbind(main_df, denit, nl)
	}
	
	main_df <- data_cleaning(main_df)

	return(main_df)
}

#returns the col names to the specified colname_to_
identify_cols_names <- function(year, colname_to_search, tier_ssnb)
{
	main_df <- aggregate_to_maindf(year, FALSE, tier_ssnb)

	id_name_col <- which(grepl(colname_to_search, colnames(main_df))==T)

	col_name <- colnames(main_df)[id_name_col]

	return(col_name)
}

#upscales Caa to Muni by summing each Muni_ID 
#CONDITION: must be in kg N so compute_arwa == FALSE
upscale_caa_to_muni <- function(year, tier_ssnb)
{
	main_df <- aggregate_to_maindf(year, FALSE, tier_ssnb)

	col_leaching_name <- identify_cols_names(year, 'leaching', tier_ssnb)
	col_denit_name <- identify_cols_names(year, 'denit', tier_ssnb)

	upscaled_df <- main_df %>%
		group_by(Muni_ID) %>%
		summarise(
			sum_denit = sum(denitri_N),
			sum_leaching = sum(leaching_N) 
      			 )
	upscaled_df <- data_cleaning(upscaled_df)

	return(upscaled_df)
}

#merge the output of upscale_caa_to_muni and the remaining muni data for further analysis
merge_upscaled_data <- function(year, tier_ssnb)
{	
	main_df <- create_main_csv()
	upscaled_df <- upscale_caa_to_muni(year, tier_ssnb)

	main_df_muni <- merge(main_df, upscaled_df, by='Muni_ID', all.x=T, sort=FALSE)

	return(main_df_muni)
}

#compute denitrification and leaching in kg N/ha
compute_nha_muni <- function(year, tier_ssnb)
{
	uaa <- load_uaa(year)
	main_df <- merge_upscaled_data(year, tier_ssnb)

	for (i in 4:ncol(main_df))
	{
		main_df[, i] <- round(main_df[, i]/uaa, 5)
	}
	colnames(main_df)[5] <- 'leaching_nha'
	colnames(main_df)[4] <- 'denit_nha'
	return(main_df)
}

write_leaching_output <- function(data_to_write, name, year, tier_ssnb)
{
	yr <- year_prefix(year)
	fullname <- paste0(name, yr)

	write_output('Leaching', data_to_write, paste0(tier_ssnb, '_',fullname))

}