source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')
source('./Leaching_module/Function/Compute_leaching.R')
source('./Leaching_module/Function/compute_correct_surface_water_areas.R')
source('./NLoading_module/Function/Compute_gw_loadings.R')
source('./GIS_module/Function/GW_computation_functions.R')
source('./GIS_module/Function/General_GIS_functions.R')

library(raster)
##################################################################################################################
########################################## GIS ANALYSIS ##########################################################
#muni leaching
nleaching99 <- retrieve_data('Leaching', 1999, 'tier2_muni')
nleaching09 <- retrieve_data('Leaching', 2009, 'tier2_muni')

#reduction in gw drainage
reduction_drainage <- read.csv('./ExploratoryAnalysis_module/Exploratory_results/Drainage.csv')

#caa leaching
nlec_caa99 <- aggregate_to_maindf(1999, TRUE, 'tier2_ssnb')
nlec_caa09 <- aggregate_to_maindf(2009, TRUE, 'tier2_ssnb')

#highlight CAA with statistical biases where N-leaching == 0
nlec_caa09[which(nlec_caa09$ssnb==0), 11] <- NA
nlec_caa99[which(nlec_caa99$ssnb==0), 11] <- NA

#caa N-loading
nl_caa99 <- compute_gw_leaching(1999, 'tier2_muni')
nl_caa09 <- compute_gw_leaching(2009, 'tier2_muni')

#highlight CAA with statistical biases where N-leaching == 0
nl_caa99[which(nl_caa99$leaching==0), ncol(nl_caa99)] <- NA
nl_caa09[which(nl_caa09$leaching==0), ncol(nl_caa09)] <- NA

#aquifer N-loadings
gw_nl99 <- load_gw_data(1999, 'source_leaching')
gw_nl09 <- load_gw_data(2009, 'source_leaching')

#critical Nc
nc99 <- gw_nl99[which(gw_nl99$nloading>11.3), c('aquifer_ID', 'nc')]
nc09 <- gw_nl09[which(gw_nl09$nloading>11.3), c('aquifer_ID', 'nc')]

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

## ------------------------- POLYGONS ----------------------- ##

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

### AQUIFER NLOADINGS ###
gw_p99 <- gis_gw_plot(gw_db99, 'nloading', '1999 - GW', TRUE, 'Nc', '(mg N/L)')
gw_p09 <- gis_gw_plot(gw_db09, 'nloading', '2009 - GW', TRUE, 'Nc', '(mg N/L)')
gw_plot_together <- map_arrange(gw_p99, gw_p09, 'GIS_GW_N-loading_paper.pdf')

master_plot <- master_plot_nload(nl_p99, gw_p99, nl_p09, gw_p09, height = 11, width = 6.5, 'master_plot_correct_v1.pdf')

### AQUIFER CRITICAL Nc
nc_p99 <- gis_gw_plot(gw_db99_nc, 'nloading', '1999: > 11.3 mg L/L', TRUE, 'Nc', '(mg N/L)')
nc_p09 <- gis_gw_plot(gw_db09_nc, 'nloading', '2009: > 11.3 mg N/L', TRUE, 'Nc', '(mg N/L)')
gw_plot_together <- map_arrange(nc_p99, nc_p09, 'critical_Nc.pdf')

## ------------------------- RASTERS ----------------------- ##

# plot recharge rate
rech_rate <- create_recharge_rate(TRUE)
path <- plot_output()
tmap_save(rech_rate, filename = paste0(path, 'rech_rate.jpeg'), dpi=600)

#plot GW recharge in both years
d99 <- create_aquifer_recharge(1999, TRUE, '1999', FALSE)
d09 <- create_aquifer_recharge(2009, TRUE, '2009', FALSE)
plot_together <- map_arrange(d99, d09, 'Drainage_GW_bothyears.pdf')

#plot difference in drainage over time
df_gw09 <- get_df_Nc(2009)
df_gw99 <- get_df_Nc(1999)
drain_dif <- (df_gw09$drainage-df_gw99$drainage)/df_gw99$drainage*100
df <- data.frame(GW_ID=df_gw99$GW_ID)
df$drain_dif <- drain_dif
gww <- merge(gw, df, 'GW_ID')
gww <- subset(gw, gw_data0_1 != 'Ind')
sb <- subset(gw, gw_data0_1=='Ind')
plot_rech_diff <- create_aquifer_recharge_result_proportion(sb, gww)

plot_result_w_1999 <- map_arrange(d99, plot_rech_diff, 'result_gw_test.jpeg')


### N-LEACHING ###
ln09 <- get_modelling_files('Total_leaching', 'tot_leaching09')
ln09[ln09<0] <- 0
ln99 <- get_modelling_files('Total_leaching', 'tot_leaching99')
ln99[ln99<0] <- 0

#GW
l09 <- get_modelling_files('Pathway_leaching', 'nload_correct_gw09', 'Default')
l99 <- get_modelling_files('Pathway_leaching', 'nload_correct_gw99', 'Default')
muni <- get_muni_shp()

p_nl09 <- create_raster_leaching(ln09, 'tot_leaching09', '2009 - Total N-leaching', TRUE, 'N-leaching', muni, '(kg N/ha)')
p_nl99 <- create_raster_leaching(ln99, 'tot_leaching99', '1999 - Total N-leaching', FALSE, 'N-leaching', muni, '(kg N/ha)')

p_l09 <- create_raster_leaching_gw(l09, 'nload_correct_gw09', '2009', TRUE, 'Ngw', muni, '(kg N/ha)')
p_l99 <- create_raster_leaching_gw(l99, 'nload_correct_gw99', '1999', FALSE, 'Ngw', muni, '(kg N/ha)')
plot_together <- master_plot_nload(p_nl99, p_nl09, p_l99, p_l09, height = 11, width = 6.5, 'N__leaching.jpeg')


### Nc at ha
nc09 <- get_Nc_rasters('gw_Nc09', 'Default')
nc99 <- get_Nc_rasters('gw_Nc99', 'Default')
muni <- get_muni_shp()
p_nc09 <- create_raster_Nc(nc09, 'gw_Nc09', '2009', TRUE, 'Nc', muni, '(mg N/L)')
p_nc99 <- create_raster_Nc(nc99, 'gw_Nc99', '1999', TRUE, 'Nc', muni, '(mg N/L)')
plot_together <- map_arrange(p_nc99, p_nc09, 'GIS_Nc_paperr.pdf')

### Nc at GW
df_nc99 <- get_df_Nc(1999)
df_nc09 <- get_df_Nc(2009)
df_nc99$N.loads.mg.N. <- df_nc99$N.loads.mg.N./1000 #t N
df_nc09$N.loads.mg.N. <- df_nc09$N.loads.mg.N./1000 #t N

df_dif <- as.data.frame(cbind(df_nc09$GW_ID, df_nc09$N.loads.mg.N., df_nc99$N.loads.mg.N.))
colnames(df_dif) <- c('GW_ID', 'nloads09', 'nloads99')
df_dif$dif <- df_dif$nloads09-df_dif$nloads99
df_dif$dif_prc <- df_dif$dif/df_dif$nloads99*100
df_dif <- data_cleaning(df_dif)

gw <- load_shp('gw')
gw99 <- merge(gw, df_nc99, 'GW_ID')

gw09 <- merge(gw, df_nc09, 'GW_ID')
gw99 <- merge(gw, df_nc99, 'GW_ID')

gw09 <- subset(gw09, gw_data0_1 != 'Ind')
gw99 <- subset(gw99, gw_data0_1!='Ind')

write_results('Nc', as.data.frame(gw09), 'gw_dataset09')

gw_dif <- merge(gw, df_dif, 'GW_ID')
gw_dif <- subset(gw_dif, gw_data0_1!='Ind')
subset <- subset(gw, gw_data0_1 == 'Ind')

p_nl99 <- create_gw_leaching_result(subset, gw99)
p_dif <- create_gw_leaching_result_proportion(subset, gw_dif)

pll <- map_arrange(p_nl99, p_dif, 'GW_Nloads.jpeg')
plot_together <- master_plot_nload(p_l99, p_l09, p_nl99, p_dif, height = 11, width = 6.5, 'N__leaching_GW.jpeg')




gw_p99 <- create_raster_gw(subset, gw99, 'Nc.mg.NO3.L.', '1999 - GW', TRUE, 'Nc', '(mg NO3/L)')
gw_p09 <- create_raster_gw(subset, gw09, 'Nc.mg.NO3.L.', '2009 - GW', TRUE, 'Nc', '(mg NO3/L)')
plot_together <- map_arrange(gw_p99, gw_p09, 'GIS_Nc_paperr_gw.jpeg')

master_plot <- master_plot_nload(p_nc99, gw_p99, p_nc09, gw_p09, height = 11, width = 6.5, 'master_plot_correct_v1.pdf')

