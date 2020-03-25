
## ------------------------------ LEACHING RISK INDEX ----------------------------------- ##
############################################################################################

## MEAN RESIDENCE TIME CALCULATIONS --------------------------------------------------------
source('./Leaching_risk_index/Functions/Computation_MeanResidenceTime.R')

# calculate no3 velocity (meters/year), general mean residence time (year) and aquifer residence time (year)
loop_MRT_computations()

# reclassifies GW MRT into 5 main risk indexes, from very low (1) to very high (5)
loop_MRT_reclass()


## NC CALCULATIONS -------------------------------------------------------------------------
source('./Leaching_risk_index/Functions/Computation_Nc.R')

# reclassifies GW NC into 5 main risk indexes, from very low (1) to very high (5)
loop_NC_reclass(irrig_mode = T)


## RISK INDEX CALCULATIONS -----------------------------------------------------------------
source('./Leaching_risk_index/Functions/Computation_Risk_index.R')

#COMPUTE AND EXPORT THE RISK INDEX FOR BOTH YEARS
loop_risk_index()

rc_mrt09 <- raster(get_risk_data(subfolder = 'GIS_index', filename = paste0('Leaching_index', year_prefix(2009))))
rc_mrt99 <- raster(get_risk_data(subfolder = 'GIS_index', filename = paste0('Leaching_index', year_prefix(1999))))
