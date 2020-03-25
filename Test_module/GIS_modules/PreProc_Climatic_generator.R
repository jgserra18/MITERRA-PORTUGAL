source('./GIS_module/Function/PreProcessing/Precipitation_idw.R')


## PRECIPITATION -----------------------------------------------------------------
#whole precipitation
p99 <- downscale_idw_caa(1999, caa_whole=FALSE, 'precipitation')
p09 <- downscale_idw_caa(2009, caa_whole=FALSE, 'precipitation')

write_idw(p99, 1999, 'rast_p', 'precipitation')
write_idw(p09, 2009, 'rast_p', 'precipitation')

#precipitation within UAA
prec99 <- downscale_idw_caa(1999, caa_whole=TRUE, 'precipitation')
prec09 <- downscale_idw_caa(2009, caa_whole=TRUE, 'precipitation')

write_idw(prec99, 1999, 'rast_caa', 'precipitation')
write_idw(prec09, 2009, 'rast_caa', 'precipitation')

## EVAPO --------------------------------------------------------------------------
e99 <- downscale_idw_caa(1999, caa_whole=FALSE, 'evapo')
e09 <- downscale_idw_caa(2009, caa_whole=FALSE, 'evapo')

write_idw(e99, 1999, 'rast_e', 'evapo')
write_idw(e09, 2009, 'rast_e', 'evapo')

evapo99 <- downscale_idw_caa(1999, caa_whole=TRUE, 'evapo')
evapo09 <- downscale_idw_caa(2009, caa_whole=TRUE, 'evapo')

write_idw(evapo99, 1999, 'rast_caa', 'evapo')
write_idw(evapo09, 2009, 'rast_caa', 'evapo')
