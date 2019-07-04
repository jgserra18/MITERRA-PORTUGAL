source('./Gaseous_N_losses/Emission function/Compute_tier1_emission.R')

#call activity data
#not mandatory
data_manure_app99 <- call_manure_app(1999)
data_manure_app09 <- call_manure_app(2009)

org_fert99 <- select_inputs_cols(1999)
org_fert09 <- select_inputs_cols(2009)

#compute N-NOx emissions from manure applied to soil N 
#and write to activity data
nox_manure_app99 <- compute_manure_app(1999, 'NOx')
nox_manure_app09 <- compute_manure_app(2009, 'NOx')

#compute N-NOx emissions from grazing N (without N-NH3 correction)
#and write to activity data of N-NOx
nox_grazing99 <- nox_compute_grazing(1999)
nox_grazing09 <- nox_compute_grazing(2009)

#compute N-NOx emissions from sludge and fertiliser N applied to soil
#and write to activity data
nox_inputs99 <- compute_org_fert(1999, 'NOx')
nox_inputs09 <- compute_org_fert(2009, 'NOx')
