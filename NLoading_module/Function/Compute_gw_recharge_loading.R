#load load_gw_prec
#merge with rech_rates

source('./NLoading_module/Function/Compute_gw_loadings.R')
source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')

## LOOKS FOR AQUIFER RECHARGE RATES AND MERGES WITH WITH COMPLETE DATA SET
## --------------------------------------------------------------------------

gw_identify_recharge <- function(year)
{
	main_df <- aggregate_gw_dataset(year)
	aqui_id <- col_id_by_name('aquifer_ID', main_df)
	rech <- col_id_by_name('recharge', main_df)

	new_df <- main_df[, c(aqui_id, rech)]

	return(new_df)
}

gw_recharge_db <- function(year)
{
  main_df <- gw_complete_dataset(year)
  main_df <- main_df[-ncol(main_df)] #remove last col
	prec_df <- load_gw_prec(year)
	gw_rech <- gw_typology_organize(year, 'yes')

	main_df <- merge(main_df, gw_rech, 'aquifer_ID')
	main_df <- merge(main_df,prec_df, 'aquifer_ID')
	
	return(main_df)
}
