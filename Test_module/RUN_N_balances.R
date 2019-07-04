source('./Other_N_balances/Function/N_balance_functions.R')

#==============================================================#
######################### LOAD DATA ############################
#ALL OPTIONAL

#gnb call
gnb99 <- select_output_data('GNB', 1999, 'default')
gnb09 <- select_output_data('GNB', 2009, '_irrig')

#gaseous call for NH3 and NOx
#note: N2O is calculated separately

nh399 <- gas_output_data('NH3', 1999)
nox99 <- gas_output_data('NOx', 1999)

nh309 <- gas_output_data('NH3', 2009)
nox09 <- gas_output_data('NOx', 2009)

#==============================================================#
###################### NITROGEN SURPLUS ########################

#compute nh3 housing + housing emissions
#optional
nh3_b4_app99 <- ns_select_gas('NH3', 1999)
nh3_b4_app09 <- ns_select_gas('NH3', 2009)

#COMPUTE THE NITROGEN SURPLUS (NS)
ns99 <- ns_compute('NH3', 1999, 'ON')
ns09 <- ns_compute('NH3', 2009, 'ON')

#==============================================================#
############################ SSNB ##############################

#organize total gaseous emissions for the three sources
#OPTIONAL
gas_compile99 <- ssnb_compile_tot_gaseous(1999, 'tier2', irrig_mode = 'ON') 
gas_compile09 <- ssnb_compile_tot_gaseous(2009, 'tier2', irrig_mode = 'ON') #w irrigation

#EXPORT RUNOFF DATA
#OPTIONAL
runoff99 <- runoff_output_data('Runoff', 1999, 'total_irrig')
runoff09 <- runoff_output_data('Runoff', 2009, 'total_irrig')

#Export other manure data
#OPTIONAL
other_manure99 <- other_manure_output_data(1999, 'manure')
other_manure09 <- other_manure_output_data(2009, 'manure')

#compute SOIL SURFACE N BALANCE (SSNB)
tier <- c('tier1', 'tier2')

#run SSNB calculations for each tier
for (i in tier)
{
  ssnb99 <- ssnb_compute(1999, i, 'ON')
  ssnb09 <- ssnb_compute(2009, i, 'ON')
  
  irrig_filename <- correct_filename_irrigation_n2o(ssnb99)
  write_output_n_balance(ssnb99, paste0(i, '_irrig_ssnb99', irrig_filename), 'SSNB')
  write_output_n_balance(ssnb09, paste0(i,'_irrig_ssnb09', irrig_filename), 'SSNB')
}

#================================================================#
############################ EXPORT ##############################
#EXPORT OUTPUTS TO APPROPRIATE
irrig_filename <- correct_filename_irrigation_n2o(ns99)

write_output_n_balance(ns99, paste0('nitrogen_surplus99', irrig_filename), 'NS')
write_output_n_balance(ns09, paste0('nitrogen_surplus09', irrig_filename), 'NS')
