source('./Runoff_module/Function/Compute_runoff_application.R')

#"Gross" N in all fertiliser types 
n_sources99 <- list_rf_data_year()[[1]]
n_sources09 <- list_rf_data_year()[[2]]

#N-NH3 emissions from each fertiliser type
nh3_sources99 <- compute_nh3_emission(1999)
nh3_sources09 <- compute_nh3_emission(2009)

#sort column names
main_df99 <- sort_col_names(n_sources99, nh3_sources99)
main_df09 <- sort_col_names(n_sources09, nh3_sources09)

#compute net N - activity to estimate runoff losses
net_n99 <- compute_runoff_activity_data(main_df99, 1999)
net_n09 <- compute_runoff_activity_data(main_df09, 2009)

#Optional: Load the sheet with rf fractions
#rf_frac99 <- load_rf_fraction(1999)
#rf_frac09 <- load_rf_fraction(2009)

#load all the main_df at the CAA scale
rf_main_df_caa99 <- organize_rf_data_CAA(net_n99, 1999)
rf_main_df_caa09 <- organize_rf_data_CAA(net_n09, 2009)

#calculate N-runoff losses at the CAA scale per source
rf_N_losses_caa99 <- compute_runoff_losses_caa(rf_main_df_caa99)
rf_N_losses_caa09 <- compute_runoff_losses_caa(rf_main_df_caa09)

#compute N-runoff (kg N/ha) per municipality per source
runoff_per_source99 <- compute_runoff_losses(rf_N_losses_caa99, 1999, irrig_mode = 'ON')
runoff_per_source09 <- compute_runoff_losses(rf_N_losses_caa09, 2009, irrig_mode = 'ON')

#compute total N-runoff losses (kg N/ha)
runoff_total99 <- NS_runoff_total_nha(runoff_per_source99, irrig_mode = 'ON') #T/F regards irrigation mode
runoff_total09 <- NS_runoff_total_nha(runoff_per_source09, irrig_mode = 'ON')

#==================================================================================#
############################# WRITE ALL THE FILES #################################
#write runoff (kg N/ha) per source
write_runoff_output(data_to_write = runoff_per_source99, name = 'runoff_source_muni', year = 1999)
write_runoff_output(data_to_write = runoff_per_source09, name = 'runoff_source_muni', year = 2009)

write_runoff_output(data_to_write = rf_N_losses_caa99, name = 'runoff_source_CAA', year = 1999)
write_runoff_output(data_to_write = rf_N_losses_caa09, name = 'runoff_source_CAA', year = 2009)

write_runoff_output(data_to_write = runoff_total99, name = 'runoff_total_irrig', year = 1999)
write_runoff_output(data_to_write = runoff_total09, name = 'runoff_total_irrig', year = 2009)
