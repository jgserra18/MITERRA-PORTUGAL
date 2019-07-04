source('./Runoff_module/Function/Compute_irrigation_runoff.R')
source('./Runoff_module/Function/Compute_runoff_application.R')

print('Loading Irrigation runoff submodule')

year <- c(1999, 2009)

for (i in year)
{
  #compute_irrigation_sys_inefficiency(i, TRUE) #computes irrigation N losses through inefficiencies
  compute_runoff_irrig(i, T) #computes irrigation N losses through runoff || Writes data to irrigation N inefficiency
}