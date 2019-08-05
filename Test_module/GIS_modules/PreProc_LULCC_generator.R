source('./GIS_module/Function/PreProcessing/LULCC_generator_funcs.R')

clc_years <- c(2000, 2012)

for (i in clc_years)
{
  #export different LandUses 
  export_LU(i)
  
  #export LC related to CLC agricultural areas 
 # write_UAA_LC(i)
}