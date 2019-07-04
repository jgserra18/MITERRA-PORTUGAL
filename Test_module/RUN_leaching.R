source('./Main_functions.R')
source('./Gaseous_functions.R')
source('./Other_N_balances/Function/N_balance_functions.R')
source('./Runoff_module/Function/Compute_runoff_application.R')
source('./Leaching_module/Function/Compute_leaching.R')


#==============================================================#
######################### LOAD DATA ############################

#load lfraction
#optional
lf99 <- load_lf_fraction(1999)
lf09 <- load_lf_fraction(2009)

#tier <- c('tier1', 'tier2')
tier <- c('tier2_irrig')
for (i in tier)
{
  ssnb99 <- load_ssnb_muni(1999, TRUE, i)
  ssnb09 <- load_ssnb_muni(2009, TRUE, i)
  
  #downscale ssnb to the caa scale
  #optional
  caa_ssnb99 <- downscale_ssnb_caa(1999, i)
  caa_ssnb09 <- downscale_ssnb_caa(2009, i)
  
  #compute leaching (in kg N) at the caa scale
  leach_N99 <- compute_leaching(1999, i)
  leach_N09 <- compute_leaching(2009, i)
  
  #compute denitrification (in kg N) at the caa scale
  denit99 <- compute_denitrification(1999, i)
  denit09 <- compute_denitrification(2009, i)
  
  #compute leaching and denit (in kg N/ha) at the caa scale
  caa_db99 <- aggregate_to_maindf(1999, TRUE, i)
  caa_db09 <- aggregate_to_maindf(2009, TRUE, i)
  
  #upscaled total N nad compute kg N/ha at the municipality scale
  muni_d99 <- compute_nha_muni(1999, i)
  muni_d09 <- compute_nha_muni(2009, i)
  
  #==============================================================#
  ########################## EXPORT # ############################
  write_leaching_output(caa_db99, 'caa_irrig', 1999, i)
  write_leaching_output(caa_db09, 'caa_irrig', 2009, i)
  
  write_leaching_output(muni_d99, 'muni_irrig', 1999, i)
  write_leaching_output(muni_d09, 'muni_irrig', 2009, i)
}


