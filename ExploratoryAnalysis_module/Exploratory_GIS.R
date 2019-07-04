source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')
source('./Leaching_module/Function/Compute_leaching.R')
source('./Leaching_module/Function/compute_correct_surface_water_areas.R')
source('./NLoading_module/Function/Compute_gw_loadings.R')

library(rast)
##################################################################################################################
########################################## GIS ANALYSIS ##########################################################
#muni leaching
nleaching99 <- retrieve_data('Leaching', 1999, 'tier2_muni')
nleaching09 <- retrieve_data('Leaching', 2009, 'tier2_muni')

#reduction in gw drainage
reduction_drainage <- read.csv('./ExploratoryAnalysis_module/Exploratory_results/Drainage.csv')
View(reduction_drainage)
#caa leaching
nlec_caa99 <- aggregate_to_maindf(1999, TRUE, 'tier2')
nlec_caa09 <- aggregate_to_maindf(2009, TRUE, 'tier2')

#highlight CAA with statistical biases where N-leaching == 0
nlec_caa09[which(nlec_caa09$ssnb==0), 11] <- NA
nlec_caa99[which(nlec_caa99$ssnb==0), 11] <- NA

#caa N-loading
nl_caa99 <- caa_corr_loading(1999) 
nl_caa09 <- caa_corr_loading(2009)

#highlight CAA with statistical biases where N-leaching == 0
nl_caa99[which(nl_caa99$leaching==0), ncol(nl_caa99)] <- NA
nl_caa09[which(nl_caa09$leaching==0), ncol(nl_caa09)] <- NA

#aquifer N-loadings
gw_nl99 <- load_gw_data(1999)
gw_nl09 <- load_gw_data(2009)

#critical Nc
nc99 <- gw_nl99[which(gw_nl99$nloading>11.3), c('aquifer_ID', 'nloading')]
nc09 <- gw_nl09[which(gw_nl09$nloading>11.3), c('aquifer_ID', 'nloading')]

#load caa shp
caa99 <- load_shp('CAA99_test')
caa09 <- load_shp('CAA09')
muni <- load_shp('Muni')


#ind_gw_inters <- load_shp('ind_aquifer')
gw <- load_shp('gw')
colnames(gw@data)[ncol(gw)] <- 'aquifer_ID'

hydro <- load_shp('main_hydro')


#merge data N-LEACHING
caa_db99 <- merge(caa99, nlec_caa99, by='CAA_ID')
caa_db09 <- merge(caa09, nlec_caa09, by='CAA_ID')
caa_db09$leaching_nha <- as.integer(caa_db09$leaching_nha)

#merge data N-LOADING
caa_nl99 <- merge(caa99, nl_caa99, by='CAA_ID')
caa_nl09 <- merge(caa09, nl_caa09, by='CAA_ID')

#merge data AQUIFER NLOADINGS
gw_db99 <- merge(gw, gw_nl99, 'aquifer_ID')
gw_db09 <- merge(gw, gw_nl09, 'aquifer_ID')

#merge Nc critical
gw_db99_nc<- merge(gw, nc99, 'aquifer_ID')
gw_db09_nc<- merge(gw, nc09, 'aquifer_ID')

red_drainage <- merge(gw, reduction_drainage, 'aquifer_ID')

##################################################################################################################
############################################ TMAP PLOTS ##########################################################

#note: create a function that cross-validates the municipalities with negative SSNB (and hence leach=0)
#to identify them in the plot as "ND (in black)"

### N-LEACHING ###
#call tmap and make plot
p99 <- create_map(caa_db99, 'leaching_nha', '1999', FALSE, 'N-leaching', muni, '(kg N/ha)')
p09 <- create_map(caa_db09, 'leaching_nha', '2009', TRUE, 'N-leaching', muni, '(kg N/ha)')
plot_together <- map_arrange(p99, p09, 'GIS_N-leaching_paper.pdf')
 
### N-LOADING ###
#nl_p99 <- create_map(caa_nl99, 'nloading', '1999 - CAA', TRUE, 'Nc', muni, '(mg N/L)')
#nl_p09 <- create_map(caa_nl09, 'nloading', '2009 - CAA', TRUE, 'Nc', muni, '(mg N/L)')
#nl_plot_together <- map_arrange(nl_p99, nl_p09, 'GIS_N-loading_paper_correct.pdf')

### N-LOADING with main_hydro###
nl_p99 <- caa_nc_plot_main_hydro(caa_nl99, 'nloading', '1999 - CAA', TRUE, 'Nc', muni, '(mg N/L)')
nl_p09 <- caa_nc_plot_main_hydro(caa_nl09, 'nloading', '2009 - CAA', TRUE, 'Nc', muni, '(mg N/L)')
nl_plot_together <- map_arrange(nl_p99, nl_p09, 'GIS_N-loading_paper_correct.jpeg')
nl_p09
### AQUIFER NLOADINGS ###
gw_p99 <- gis_gw_plot(gw_db99, 'nloading', '1999 - GW', TRUE, 'Nc', '(mg N/L)')
gw_p09 <- gis_gw_plot(gw_db09, 'nloading', '2009 - GW', TRUE, 'Nc', '(mg N/L)')
gw_plot_together <- map_arrange(gw_p99, gw_p09, 'GIS_GW_N-loading_paper.pdf')

master_plot <- master_plot_nload(nl_p99, gw_p99, nl_p09, gw_p09, height = 11, width = 6.5, 'master_plot_correct_v1.pdf')

### AQUIFER CRITICAL Nc
nc_p99 <- gis_gw_plot(gw_db99_nc, 'nloading', '1999: > 11.3 mg L/L', TRUE, 'Nc', '(mg N/L)')
nc_p09 <- gis_gw_plot(gw_db09_nc, 'nloading', '2009: > 11.3 mg N/L', TRUE, 'Nc', '(mg N/L)')
gw_plot_together <- map_arrange(nc_p99, nc_p09, 'critical_Nc.pdf')
