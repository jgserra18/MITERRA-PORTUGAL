source('./Irrigation_module/Functions/Global_irrigation_functions.R')

########################## GET DATA ###############################
water_sources <- get_folder_files('water_source')
d <- select_module_output('Irrigation_module')

####################### OPTIONAL FUNCTIONS ###############################
## THESE ARE USED IN THE MAIN FUNCTIONS SUCH AS POPULATE_REF_VOLUMES_CROP

#irrig <- read_folder_files('Reference_volumes', 'reference_volume')

#cereal_subcrops <- get_crop_names(2009, 'cereals')
#maize <- get_irrig_areas(2009, 'cereals', 'maize')

#main_crops <- get_maincrops_names(2009)

#pop_tempalte_ref_vole_crop <- populate_ref_volumes_crop(2009)

#populate the irrigation_module output with the reference volumes per crop and irrigation systems at the municipality scale
populate_ref_volumes_crop <- populate_ref_volumes_crop(2009)

#computes crop volumes (in m3)
compute_crop_volume09 <- compute_crop_volume(2009)
compute_crop_volume99 <- compute_crop_volume(1999)

#aggregates the crop volumes per irrig system
compute_irrig_sys_vol <- compute_irrig_sys_vol(year = 2009, compute_sum_irrig_sys = T)
