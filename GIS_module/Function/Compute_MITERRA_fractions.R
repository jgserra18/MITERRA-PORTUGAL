
##OLD CODE - HARDCORE
#NEEDS TO BE UPDATED

################################################################################################################
########################################################### 1999 ###############################################
################################################################################################################
################################################################################################################

library(raster)
library(rgdal)
library(spatial.tools)

#########################
####1999 RUNOFF calculations####
#########################
#temp <- raster(ncol=3679, nrow=5250, ext, res=res(ps_reclass), crs(ps_reclass))

prec99 <- raster('G:\\Water surplus\\IDW\\Prec99_activity.tif')
evap99 <- raster('G:\\Water surplus\\IDW\\Evapo99_activity.tif')

ps99 <- prec99-evap99
ps99_1 <- reclassify(ps99, c(-Inf, 0, 0, 0, Inf, 1))
ps99 <- ps99*ps99_1


#writeRaster(ps99, filename = 'g:\\GIS Leaching db\\Leaching calculatiosn\\99\\Water surplus\\prec_surplus_99.tif', format='GTiff')

## reclass according to miterra (lower, upper, new value)
ps_reclass <- reclassify(ps99, c(-Inf, 50, 25, 50, 100, 50, 100, 300, 75, 300, Inf, 100))
ps_reclass
#writeRaster(ps_reclass, filename = 'g:\\GIS Leaching db\\Leaching calculatiosn\\99\\Water surplus\\prec_surplus_99reclass.tif', format='GTiff')
#load remaining input files

dr <- raster('G:\\GIS Leaching db\\DB_reductionfactors\\RF_DR.tif')
dr <- spatial_sync_raster(unsynced = dr, reference = ps_reclass)

soil <- raster('G:\\GIS Leaching db\\DB_reductionfactors\\RF_soiltype_runoff.tif')
rc_soil <- spatial_sync_raster(unsynced = soil, reference = ps_reclass)

LU <- raster('G:\\GIS Leaching db\\DB_reductionfactors\\Runoff_landuse.tif')
rc_LU <- spatial_sync_raster(unsynced = LU, reference = ps_reclass)

slope <- raster('G:\\GIS Leaching db\\DB_reductionfactors\\RF_slope.tif')
rc_slope <- spatial_sync_raster(unsynced = slope, reference = ps_reclass)

Rf_min99 <- min(ps_reclass, dr, rc_soil)

writeRaster(Rf_min99, filename = 'g:\\GIS Leaching db\\Leaching calculatiosn\\99\\Runoff\\Rf_min_v2.tif', format='GTiff')

Rf99 <- rc_slope*rc_LU/100*Rf_min99/100
writeRaster(Rf99, filename = 'g:\\GIS Leaching db\\Leaching calculatiosn\\99\\Runoff\\Rf99_v2.tif', format='GTiff')

#########################
####1999 LEACHING calculations####
#########################

##check dataset##
identical(coordinates(Rf99), coordinates(Lf_frac99))

##loading dataset and rework data##
Lf_PS <- raster('G:\\GIS Leaching db\\Leaching calculatiosn\\Lf_PS\\99\\PS_99_sum.tif')
rc_Lf_PS <- spatial_sync_raster(unsynced = Lf_PS, reference = ps_reclass)

root <- raster('g:\\GIS Leaching db\\DB_reductionfactors/Calculations/Leaching_update/Root_update.tif')
rc_root <- spatial_sync_raster(unsynced = root, reference = ps_reclass)

Lf_lu <- raster('g:\\GIS Leaching db\\DB_reductionfactors/leaching_landuse.tif')
rc_LU <- spatial_sync_raster(unsynced = Lf_lu, reference = ps_reclass)

t99 <- raster('g:\\GIS Leaching db\\DB_reductionfactors/RF_temperature99.tif')
rc_t99 <- spatial_sync_raster(unsynced = t99, reference = ps_reclass)

OC <- raster('g:\\GIS Leaching db\\DB_reductionfactors/RF_OCtop.tif')
rc_OC <- spatial_sync_raster(unsynced = OC, reference = ps_reclass)

s_texture <- raster('g:\\GIS Leaching db\\DB_reductionfactors/Lf_soiltype.tif')
rc_texture <- spatial_sync_raster(unsynced = s_texture, reference = ps_reclass)

##calculate min Lf fraction
Lf_min99 <- min(rc_root, rc_Lf_PS, rc_t99, rc_OC)
#writeRaster(Lf_min99, filename = 'G:\\GIS Leaching db\\Leaching calculatiosn\\99\\Leaching\\Lf_min99_v2', format='GTiff')

##Calculate Lf fraction
Lf_frac99 <- rc_texture*rc_LU/100*Lf_min99/100
#writeRaster(Lf_frac99, filename = 'G:\\GIS Leaching db\\Leaching calculatiosn\\99\\Leaching\\Lf_frac99_v2', format='GTiff')


################################################################################################################
########################################################### 2009 ###############################################
################################################################################################################
################################################################################################################
#########################
####2009 RUNOFF calculations####
#########################
prec09 <- raster('D:\\Water surplus\\IDW\\Prec09_activity.tif')


evap09 <- raster('D:\\Water surplus\\IDW\\Evapo09_activity.tif')

ps09 <- prec09-evap09
ps09_1 <- reclassify(ps09, c(-Inf, 0, 0, 0, Inf, 1))
ps09 <- ps09*ps09_1
#writeRaster(ps09, filename = 'g:\\GIS Leaching db\\Leaching calculatiosn\\09\\Prec surplus\\prec_surplus_09.tif', format='GTiff')

## reclass according to miterra (lower, upper, new value)
ps_reclass <- reclassify(ps09, c(-Inf, 50, 25, 50, 100, 50, 100, 300, 75, 300, Inf, 100))
#writeRaster(ps_reclass, filename = 'g:\\GIS Leaching db\\Leaching calculatiosn\\09\\Prec surplus\\prec_surplus09_reclass.tif', format='GTiff')

dr <- raster('G:\\GIS Leaching db\\DB_reductionfactors\\RF_DR.tif')
dr <- spatial_sync_raster(unsynced = dr, reference = ps_reclass)

soil <- raster('G:\\GIS Leaching db\\DB_reductionfactors\\RF_soiltype_runoff.tif')
rc_soil <- spatial_sync_raster(unsynced = soil, reference = ps_reclass)

LU <- raster('G:\\GIS Leaching db\\DB_reductionfactors\\Runoff_landuse.tif')
rc_LU <- spatial_sync_raster(unsynced = LU, reference = ps_reclass)

slope <- raster('G:\\GIS Leaching db\\DB_reductionfactors\\RF_slope.tif')
rc_slope <- spatial_sync_raster(unsynced = slope, reference = ps_reclass)

Rf_min09 <- min(ps_reclass, dr, rc_soil)
#writeRaster(Rf99, filename = 'g:\\GIS Leaching db\\Leaching calculatiosn\\09\\Runoff\\Rf09min_v2.tif', format='GTiff')

Rf_frac09 <- Rf_min09/100*rc_slope*rc_LU/100
#writeRaster(Rf_frac09, filename = 'g:\\GIS Leaching db\\Leaching calculatiosn\\09\\Runoff\\Rf09_v2.tif', overwrite=T, format='GTiff')

#########################
####2009 LEACHING calculations####
#########################

Lf_PS <- raster('G:\\GIS Leaching db\\Leaching calculatiosn\\Lf_PS\\09\\PS_09_sum.tif')
rc_Lf_PS <- spatial_sync_raster(unsynced = Lf_PS, reference = ps_reclass)

root <- raster('g:\\GIS Leaching db\\DB_reductionfactors/Calculations/Leaching_update/Root_update.tif')
rc_root <- spatial_sync_raster(unsynced = root, reference = ps_reclass)

Lf_lu <- raster('g:\\GIS Leaching db\\DB_reductionfactors/leaching_landuse.tif')
rc_LU <- spatial_sync_raster(unsynced = Lf_lu, reference = ps_reclass)

t09 <- raster('g:\\GIS Leaching db\\DB_reductionfactors/RF_temperature09.tif')
rc_t09 <- spatial_sync_raster(unsynced = t09, reference = ps_reclass)

OC <- raster('g:\\GIS Leaching db\\DB_reductionfactors/RF_OCtop.tif')
rc_OC <- spatial_sync_raster(unsynced = OC, reference = ps_reclass)

s_texture <- raster('g:\\GIS Leaching db\\DB_reductionfactors/Lf_soiltype.tif')
rc_texture <- spatial_sync_raster(unsynced = s_texture, reference = ps_reclass)

##calculate min Lf fraction
Lf_min09 <- min(rc_root, rc_Lf_PS, rc_t09, rc_OC)
#writeRaster(Lf_min09, filename = 'G:\\GIS Leaching db\\Leaching calculatiosn\\09\\Leaching\\Lf_min09_v2', overwrite=T, format='GTiff')

Lf_frac09 <- Lf_min09/100*rc_LU/100*rc_texture
#writeRaster(Lf_frac09, filename = 'G:\\GIS Leaching db\\Leaching calculatiosn\\09\\Leaching\\Lf_frac09_v2', overwrite=T, format='GTiff')
