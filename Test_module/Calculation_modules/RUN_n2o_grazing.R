#load functions
source('./Gaseous_N_losses/N-N2O/Function/grazing_N2O_functions.R')


#reads the file for each year containing N excreted in grazing for each animal class
#OPTIONAL!!!!!!!!!!!!!!!!!!!!!!!!
graz99 <- read_grazing_yr(1999)
graz09 <- read_grazing_yr(2009)

#computes emissions in kg N-N2O for each animal class
#Optional!!!!!!!!!!!!!!!!!!!!!!!!
graz_N_N2O99 <- compute_n2o_graz(1999)
graz_N_N2O09 <- compute_n2o_graz(2009)

#computes emissions in kg N-N2O/ha for each animal class
n2o_ha_graz99 <- compute_n2o_ha(1999)
n2o_ha_graz09 <- compute_n2o_ha(2009)

#export to activity data N-N2O application
graz_db99 <- export_n2O(n2o_ha_graz99, 'grazing99')
graz_db09 <- export_n2O(n2o_ha_graz09, 'grazing09')

