source('./Irrigation_module/Functions/Compute_1999_areas.R')


######################################################################################################################################
####################################### EXTRAPOLATE DATA FROM 2009 TO THEN APPLY FOR 1999 ############################################

write_irrig_sys_areas <- compute_irrig_sys_volumes(2009, TRUE, get_irrig_areas, 'irrig_areas')
write_irrig_sys_proportions <- compute_irrig_syst_proportion(2009, TRUE)
write_irrig_sys_volumes <- compute_irrig_sys_volumes(2009, TRUE, modify_get_crop_water_volume, 'irrig_volumes')
write_correct_irrig_sys_vol_eff <- correct_irrig_vol_efficiency(2009, TRUE, 'irrig_volumes_efficiency')
write_vol_per_ha <- compute_2009_m3_per_ha(2009, TRUE, 'irrig_sys_m3_per_ha', FALSE) #FALSE=IGNORES IRRIGATION EFFICIENCIES

# CORRECTED WATER SOURCES

######################################################################################################################################
####################################################### COMPUTE DATA FOR 1999 ########################################################

#compute irrigation system areas for 1999
compute_1999_irrig_sys_areas <- compute_1999_general(1999, TRUE, 'irrig_sys_area', compute_irrig_syst_proportion)
compute_1999_irrig_sys_volumes <- compute_1999_irrig_sys_vol(1999, TRUE, 'irrig_volumes_wo_efficiency', with_efficiency = FALSE) #FALSE=IGNORES IRRIGATION EFFICIENCIES
