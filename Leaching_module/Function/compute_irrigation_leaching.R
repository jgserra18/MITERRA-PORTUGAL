source('./Main_functions.R')
source('./Gaseous_functions.R')
source('./Other_N_balances/Function/N_balance_functions.R')
source('./Runoff_module/Function/Compute_runoff_application.R')
source('./Leaching_module/Function/Compute_leaching.R')
source('./Irrigation_module/Functions/Global_irrigation_functions.R')

#to calculate N-leaching from irrigation some aspects need to be computed first:
#1 - SSNB with and without irrigatioN
#2 - N-leaching with and withput irrigatioN,
#3 - N-leaching_irrig - N-leaching_default = Irrig_leaching

load_tiers <- function()
{
  tiers <- c('tier2_irrig', 'tier2_ssnb') #file pattern within SSNB output folder
  return(tiers)
}

#modifies compute_nha_muni function to return only the leaching_nha in kg N
compute_N_leaching_muni <- function(year, tier)
{
  df <- compute_nha_muni(year, tier) #computes denit and leaching (in nha) at muni scale
  df$leaching_kgN <- round(df[, 'leaching_nha']*load_uaa(year), 3)
  df <- data_cleaning(df)
  
  return(df[, 'leaching_kgN'])
}

write_irrig_leaching <- function(df, write, year)
{
  if (write==TRUE)
  {
    print('Writing irrigation N-leaching (in kg N and kg N/ha).')
    print(paste0('Year----', year))
    write_irrig_output(paste0('Irrigation_leaching/', year), df, paste0('irrigatioN_leaching', year))
  }
  else if (write==FALSE)
  {
    return(df)
  }
}

compute_irrigation_leaching <- function(year, write)
{
  #load data
  tiers <- load_tiers()
  df <- create_main_csv()
  
  leach_w_irrig <- compute_N_leaching_muni(year, tiers[1])
  leach_wo_irrig <- compute_N_leaching_muni(year, tiers[2])
  
  df$irrig_leach_kgN <- leach_w_irrig-leach_wo_irrig
  df[which(df$irrig_leach_kgN<0), 'irrig_leach_kgN'] <- 0
  df$irrig_leach_nha <- round(df$irrig_leach_kgN /load_uaa(year), 4)
  
  write_irrig_leaching(df, write, year)
}

  


