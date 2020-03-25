source('./GIS_module/Function/Irrigation/N2O_N_module.R')

# get climatic regions shapefile per municipality
get_GIS_climatic_region()

# compute gross irrigation N, which is the activity data for direct N2O emissions
# allocate irrigation N to different LU classes 
# important note: this is the activity data also for net irrigation N
spatial_allocation_n2o_heterogeneous(year = 2009)
spatial_allocation_n2o_LU(year = 2009)

# create mosaic of gross irrigation N
mosaic_n2o_main_irrig_systems(year = 2009)

# compute direct n2o emissions for temperate and mediterranean regions
# this is the main function, which calls other smaller modules to handle such data
#unit : kg N-N2O/HLU ha
compute_irrig_n2o_nha_mosaic(year = 2009, main_irrig_system = 'sprinkler', write = TRUE)
  # loops around different main irrig systems
loop_n2o_nha(year = 2009)

# create raster mosaic of total direct N2O emissions from irrigation N
#unit : kg N-N2o/LU ha
compute_total_LU_n2o_nha(year = 2009, write = TRUE)

# compute direct N-N2O emissions per municipality on a UAA basis
# unit : kg N-N2O/UAA ha/municipality
compute_runoff_nha_muni(year = 2009, write = TRUE)

