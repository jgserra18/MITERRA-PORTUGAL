source('./NLoading_module/Function/Compute_gw_loadings.R')
source('./NLoading_module/Function/Compute_gw_recharge_loading.R')

tiers <- c('source_leaching_irrig', 'source_leaching')

for (i in tiers)
{
  # for naming --------------------------------------------------------------------------------------------
  ifelse(i=='source_leaching_irrig', pattern <- 'irrig', pattern <- 'default')
  
  #creates initial dataset with prec, leaching (kg N/ha) and data on groundwater bodies
  #note: incorrect CAA scale (or GW level)
  #incorrect CAA
  initial_db09 <- create_main_df(2009, i)
  initial_db99 <- create_main_df(1999, i)
  
  #creates updated dataset with the calculated drainage (litres), leaching (mg N) and nloadings (mg N/L)
  #note: #note: incorrect CAA scale (or GW level)
  #incorrect CAA
  #updated_dataset09 <- aggregate_gw_dataset(2009)
  #updated_dataset99 <- aggregate_gw_dataset(1999)
  
  #corrects the estimates to the proper CAA scale
  #note: this is only for plotting 
  corr_caa_dataset09 <- compute_gw_leaching(2009, i)
  corr_caa_dataset99 <- compute_gw_leaching(1999, i)
  
  #compute all the relevant estimates for each aquifer
  #optional used in the next functions
  #gw_db99 <- sum_by_gw_id(1999)
  #gw_db09 <- sum_by_gw_id(2009)
  
  #gw COMPLETE DATASET
  #incorrect
  #gw99 <- gw_complete_dataset(1999)
  #gw09 <- gw_complete_dataset(2009)
  #correct
  #the correct dataset also accounts for gw recharge
  gw99 <- CORRECT_gw_complete_dataset(1999, i)
  gw09 <- CORRECT_gw_complete_dataset(2009, i)
  
  #NOTE: THIS IS FROM COMPUTE_GW_RECHARGE_LOADING
  #load complete GW db including recharge rate
 # complete_gw99 <- gw_recharge_db(1999)
  #complete_gw09 <- gw_recharge_db(2009)
  
  #aggregate all the estimates for each municipality
  #note: correct order of the template file
  #NOT CORRECTED
  muni_db99 <- aggregate_nloading_muni(1999, i)
  muni_db09 <- aggregate_nloading_muni(2009, i)
  
  #================================================================================#
  ################################## WRITE DATA ###################################
  
  #write correct caa for plotting
  #This was excluded for irrigation as this wont be plot
  write_nloading_output(corr_caa_dataset09, paste0('tier2_', pattern, '_plot_corr_CAA_db'), 2009)
  write_nloading_output(corr_caa_dataset99, paste0('tier2_', pattern, '_plot_corr_CAA_db'), 1999)
  
  #write groundwater dataset
  write_nloading_output(gw99, paste0('tier2_', pattern, '_gw_db'), 1999)
  write_nloading_output(gw09, paste0('tier2_', pattern, '_gw_db'), 2009)
  
  #write complete groundwater DATA
  #write_nloading_output(complete_gw99, paste0('incorrect_', pattern, '_complete_gw_db'), 1999)
  #write_nloading_output(complete_gw09, paste0('incorrect_', pattern, '_complete_gw_db'), 2009)
  
  #write muni dataset
  write_nloading_output(muni_db99, paste0('incorrect_', pattern, '_tier2_muni_db'), 1999)
  write_nloading_output(muni_db09, paste0('incorrect_', pattern, '_tier2_muni_db'), 2009)
}
