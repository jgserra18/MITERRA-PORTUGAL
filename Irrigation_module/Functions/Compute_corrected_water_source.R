source('./Irrigation_module/Functions/no3_functions.R')

#corrected the water source based on existing no3 sampling data
corrected_no3_water_source <- compute_corrected_water_sources(2009, TRUE)

