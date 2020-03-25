source('./WaterBalance_module/Functions/GW_Water_Balance.R')

## this calculates the water balance for each groundwater body
# using UAA adjustment factor

# computes GW water balance as raster and as dataframe
compute_gw_WaterBalance(year = 1999)

r99 <- raster('./WaterBalance_module/Output/GW/GW_wb_r99.tif')
r09 <- raster('./WaterBalance_module/Output/GW/GW_wb_r09.tif')

d <- (r09-r99)/r99*100
plot(d)
