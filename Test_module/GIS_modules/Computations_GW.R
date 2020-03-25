source('./GIS_module/Function/compute_GIS_leaching_pathways.R')
source('./GIS_module/Function/GW_computation_functions.R')


## ------------------------------------------------------------------------------------------
## N-loads to deeper groundwater ----------------------------
## ------------------------------------------------------------------------------------------

#calculate N-loads to deeper groundwater (in kg N/ha)
nloads_gw99 <- compute_nloading_DeeperGroundwater(1999,  irrig_mode = 'Default')
nloads_gw09 <- compute_nloading_DeeperGroundwater(2009, irrig_mode = 'Default')

#correct N-loads based on CLC adjustment factor
correct_nl_gw99 <- correct_nloads_gw(1999, write = TRUE, irrig_mode ='Default')
correct_nl_gw09 <- correct_nloads_gw(2009,  write = TRUE, irrig_mode='Default')

#calculate N-loads to each groundwater (in kg N) (in DATAFRAME)
df_nl_gw99 <- compute_nload_entering_gw(1999, 'Default')
df_nl_gw09 <- compute_nload_entering_gw(2009, 'Default')

## ------------------------------------------------------------------------------------------
## DRAINAGE CALCULATONS (THESE ARE NOW DISREGARDED ;; USE WATER BALANCE) ----------------------------
## ------------------------------------------------------------------------------------------

#compute precipitation runoff (using RF) in mm
rf_prec99 <- rf_precipitation_runoff(1999, write = TRUE, 'Default')
rf_prec09 <- rf_precipitation_runoff(2009, write = TRUE, 'Default')

#compute soil drainage (using RF) in mm
#drainage = prec - prec_runoff
rf_drainage99 <- rf_precipitation_drainage(1999, write = TRUE, 'Default')
rf_drainage09 <- rf_precipitation_drainage(2009, write = TRUE, 'Default')

##Compute aquifer recharge in mm
gw_rech99 <- gw_drainage(1999, write = TRUE, 'Defaut')
gw_rech09 <- gw_drainage(2009, write = TRUE, 'Defaut')

##compute aquifer recharge to each GW (in DATAFRAME)
df_gw_rech99 <- compute_drainage_entering_gw(1999, 'Default')
df_gw_rech09 <- compute_drainage_entering_gw(2009, 'Default')

## ---- CORRECTED DRAINAGE = IRRIGATION LEACHING + AQUIFER RECHARGE
loop_func_total_drainage()

## ------------------------------------------------------------------------------------------
## WATER BALANCE ----------------------------
## ------------------------------------------------------------------------------------------

## see water balance module

## ------------------------------------------------------------------------------------------
## NC CALCULATONS ----------------------------
## ------------------------------------------------------------------------------------------

## DEFAULT NC CALCULATION, i.e. only aquifer recharge as drainage
# DISREGARDED
## ------------------------------------------------------------------

#compute Nc using soil drainage
rf_Nc99 <- rf_Nc_gw(1999, TRUE, 'Default')
rf_Nc09 <- rf_Nc_gw(2009, TRUE, 'Default')

#compute Nc using gw recharge
gw_NC99 <- gw_Nc_gw(1999, TRUE, 'Default')
gw_NC09 <- gw_Nc_gw(2009, TRUE, 'Default')

#compute Nc for each GW (in DATAFRAME)
df_gw_Nc99 <- df_compute_gw_nc_OLD(1999, TRUE)
df_gw_Nc09 <- df_compute_gw_nc_OLD(2009, TRUE)

## IRRIG NC CALCULATION, i.e. aquifer recharge + irrigation leaching
# DISREGARDED
## ------------------------------------------------------------------

irrig_gw_NC99 <- load_correct_Nc_mgNL(1999)
irrig_gw_NC09 <- load_correct_Nc_mgNL(2009)

df_gw_Nc99 <- df_compute_gw_nc(1999, TRUE, irrig_mode = 'Irrig')
df_gw_Nc09 <- df_compute_gw_nc(2009, TRUE, irrig_mode = 'Irrig')


## SEE THIS ------------------------------------------------------------
# Nc calculated based on the water balance from below the root zone (DEFAULT)
# write Nc at the cellgrid level 
wb_Nc_gw(year = 2009, write = TRUE)
wb_Nc_gw(year = 1999, write = TRUE)

# compute Nc for each GW body
df_gw_Nc99 <- df_compute_gw_Nc(1999, write = T)
df_gw_Nc09<- df_compute_gw_Nc(2009, write = T)

## GET FILES
rf_nc99 <- get_Nc_rasters('gw_Nc99', 'Default')
rf_nc09 <- get_Nc_rasters('gw_Nc09', 'Default')


