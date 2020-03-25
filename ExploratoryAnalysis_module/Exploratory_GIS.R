source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')
source('./Leaching_module/Function/Compute_leaching.R')
source('./Leaching_module/Function/compute_correct_surface_water_areas.R')
source('./NLoading_module/Function/Compute_gw_loadings.R')
source('./GIS_module/Function/GW_computation_functions.R')
source('./GIS_module/Function/General_GIS_functions.R')

library(raster)
library(GADMTools)
library(rworldmap)
library(rworldxtra)
library(tmap)

world <- getMap(resolution='high')
sb <- subset(world, ADMIN== 'Spain' | ADMIN=='Portugal')
pt <- subset(world, ADMIN=='Portugal')

p <- function(AA) {
  tm_shape(sb, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_fill(col='grey85') + 
    tm_layout(bg.color='lightblue') +  
    tm_credits(text=AA, size=1.2, position=c(0.88, 0.93), fontfamily = 'serif') +
    tm_shape(pt, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) +tm_borders(col='black', lwd=0.8) + 
    tm_fill(col='grey85')
}
pp <-  tm_shape(pt, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) +tm_borders(col='black', lwd=0.8)


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
df_gw09 <- get_df_Nc(year = 2009)
df_gw99 <- get_df_Nc(1999)
drain_dif <- (df_gw09$drainage-df_gw99$drainage)/df_gw99$drainage*100
df <- data.frame(GW_ID=df_gw99$GW_ID)
df$drain_dif <- drain_dif
gww <- merge(gw, df, 'GW_ID')

gww <- subset(gw, gw_data0_1 != 'Ind')
sb <- subset(gw, gw_data0_1=='Ind')
plot_rech_diff <- create_aquifer_recharge_result_proportion(sb, gww)
plot_result_w_1999 <- map_arrange(d99, plot_rech_diff, 'result_gw_test.pdf')


### N-LEACHING ###
ln09 <- get_modelling_files('Total_leaching', 'tot_leaching09')
ln09[ln09<0] <- 0
ln99 <- get_modelling_files('Total_leaching', 'tot_leaching99')
ln99[ln99<0] <- 0

#GW
l09 <- get_modelling_files('Pathway_leaching', 'nload_correct_gw09', 'Default')
l99 <- get_modelling_files('Pathway_leaching', 'nload_correct_gw99', 'Default')
muni <- get_muni_shp()

st <- stack(l99, l09)



p1 <- tm_shape(sb, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black', lwd = 1) + tm_fill(col='grey') + 
  tm_shape(st[[1]], projection = 'longlat', bbox=c(-9.8, 36.8, -5.5, 42.25)) + 
  tm_raster(breaks = c(0, 0.00001, 1, 3, 5, 10, 20, +Inf),
            palette = 'Greens', 
            labels=c('No gw', '< 1', '1 - 3', '3 - 5', '5 - 10', '10 - 15', '> 20'),
            title = expression(N[gw]~(kg~N~ha^{-1}))) + 
  tm_legend(show=T, position=c(0.7, 0.3), frame=F)+
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.9,
               position=c(0.55, 0.02), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1.1, text.size = 0.9,
             position=c(0.05, 0.93)) +
  tm_layout(frame=T,
            panel.label.height = 1,
            panel.label.size = 1,
            panel.labels = c('1999', '2009'),
            legend.text.size = 0.8,
            legend.frame = F,
            bg.color = 'lightblue',
            panel.show = T,
            legend.title.size = 1,
            fontfamily = 'serif',
            inner.margins = c(0, 0, 0, 0)) +
  tm_credits(text='A', size=1.5, position=c(0.88, 0.9), fontfamily = 'serif')
p1
p2 <-  tm_shape(sb, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black', lwd = 1) + tm_fill(col='grey') + 
  tm_shape(st[[2]], projection = 'longlat', bbox=c(-9.8, 36.8, -5.5, 42.25)) + 
  tm_raster(breaks = c(0, 0.00001, 1, 3, 5, 10, 20, +Inf),
            palette = 'Greens', 
            labels=c('No gw', '< 1', '1 - 3', '3 - 5', '5 - 10', '10 - 20', '> 20'),
            title = expression(N[gw]~(kg~N~ha^{-1}))) + 
  tm_legend(show=T, position=c(0.7, 0.3), frame=F)+
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.9,
               position=c(0.55, 0.02), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1.1, text.size = 0.9,
             position=c(0.05, 0.93)) +
  tm_layout(frame=T,
            panel.label.height = 1,
            panel.label.size = 1,
            panel.labels = '2009',
            legend.text.size = 0.80,
            legend.frame = F,
            bg.color = 'lightblue',
            panel.show = T,
            legend.title.size = 1,
            fontfamily = 'serif',
            inner.margins = c(0, 0, 0, 0)) +
  tm_credits(text='B', size=1.5, position=c(0.88, 0.9), fontfamily = 'serif')


Figure2_plot <- function(shp_file, col_plot, breaks, labels, title, panel_title, chr_rght) {
  
  tm_shape(sb, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black', lwd = 1) + tm_fill(col='grey') + 
    tm_shape(shp_file, projection = 'longlat', bbox=c(-9.8, 36.8, -5.5, 42.25)) + 
    tm_polygons(col=col_plot,breaks = breaks,
                palette = 'Reds', 
                labels=labels,
                title = title) + 
    tm_legend(show=T, position=c(0.7, 0.3), frame=F)+
    tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.9,
                 position=c(0.55, 0.02), breaks = c(0, 50, 100)) + 
    tm_compass(type='4star', size=1.1, text.size = 0.9,
               position=c(0.05, 0.93)) +
    tm_layout(frame=T,
              panel.label.height = 1,
              panel.label.size = 1,
              panel.labels = panel_title,
              legend.text.size = 0.8,
              legend.frame = F,
              bg.color = 'lightblue',
              panel.show = T,
              legend.title.size = 1,
              fontfamily = 'serif') +
    tm_credits(text=chr_rght,size=1.5, position=c(0.88, 0.9), fontfamily = 'serif')
}

p3 <- Figure2_plot(gw99, 'n_load_kgN', c(0,100,200,400,600,900,1150),
                   c('< 100', '100 - 200', '200 - 400', '400 - 600', '600 - 900', '900 - 1150'),
                   expression(N[gw]~(Mg~N~yr^{-1})),
                   '1999', 'C')
p3
p4 <-  tm_shape(sb, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black', lwd = 1) + tm_fill(col='grey') + 
  tm_shape(gw_dif, projection = 'longlat', bbox=c(-9.8, 36.8, -5.5, 42.25)) + 
  tm_polygons(col='dif_prc',breaks = c(-Inf, -40,-20,0,20,+Inf),
              palette = 'Reds', midpoint=NA,
              labels=c('< -40', '-40 - -20', '-20 - 0', '0 - 20', '> 20'),
              title = 'Change in\nN-loads (%)') + 
  tm_legend(show=T, position=c(0.7, 0.3), frame=F)+
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.9,
               position=c(0.55, 0.02), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1.1, text.size = 0.9,
             position=c(0.05, 0.93)) +
  tm_layout(frame=T,
            panel.label.height = 1,
            panel.label.size = 1,
            panel.labels = '2009 - 1999',
            legend.text.size = 0.8,
            legend.frame = F,
            bg.color = 'lightblue',
            panel.show = T,
            legend.title.size = 1,
            fontfamily = 'serif') +
  tm_credits(text='D', size=1.5, position=c(0.88, 0.9), fontfamily = 'serif')
p4

pp <- tmap_arrange(p1,p2, p3, p4, nrow=1,ncol=4) 
tmap_save(pp, './test.jpeg', dpi=600, width = 12, height = 6)


gw99$wsurplus_L <- gw99$wsurplus_L/1e9
perc_lft <- tm_shape(sb, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black', lwd = 1) + tm_fill(col='grey') + 
  tm_shape(gw99, projection = 'longlat', bbox=c(-9.8, 36.8, -5.5, 42.25)) + 
  tm_polygons(col='wsurplus_L',breaks = c(0,10,25,50,100, 300, 604),
              palette = 'Blues', #tyle='cont',
              labels=c('< 10', '10 - 25', '25 - 50', '50 - 100', '100 - 300', '300 - 604'),
              title = expression(Perc~(hm^{3}~yr^{-1}))) + 
  tm_legend(show=T, position=c(0.7, 0.3), frame=F)+
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.9,
               position=c(0.55, 0.02), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1.1, text.size = 0.9,
             position=c(0.05, 0.93)) +
  tm_layout(frame=T,
            panel.label.height = 1,
            panel.label.size = 1,
            panel.labels = '1999',
            legend.text.size = 0.8,
            legend.frame = F,
            bg.color = 'lightblue',
            panel.show = T,
            legend.title.size = 1,
            fontfamily = 'serif')

perc_dif <- tm_shape(sb, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black', lwd = 1) + tm_fill(col='grey') + 
  tm_shape(gw_dif, projection = 'longlat', bbox=c(-9.8, 36.8, -5.5, 42.25)) + 
  tm_polygons(col='dif_prc',breaks = c(-Inf, -40, -20, 0, 20, +Inf),
              palette = 'Blues', midpoint = NA,
              labels=c('< -40', '-40 - -20', '-20 - 0', '0 - 20', '>20'),
              title = 'Change in\nPerc (%)') + 
  tm_legend(show=T, position=c(0.7, 0.3), frame=F)+
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.9,
               position=c(0.55, 0.02), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1.1, text.size = 0.9,
             position=c(0.05, 0.93)) +
  tm_layout(frame=T,
            panel.label.height = 1,
            panel.label.size = 1,
            panel.labels = '2009 - 1999',
            legend.text.size = 0.8,
            legend.frame = F,
            bg.color = 'lightblue',
            panel.show = T,
            legend.title.size = 1,
            fontfamily = 'serif')
perc_dif
pp <- tmap_arrange(perc_lft, perc_dif, ncol=2)
tmap_save(pp, './test2.jpeg', dpi=600, width = 7.5, height = 6)


p_nl09 <- create_raster_leaching(ln09, 'tot_leaching09', '2009 - Total N-leaching', TRUE, 'N-leaching', muni, title = expression('kg N ha'^-1))
p_nl99 <- create_raster_leaching(ln99, 'tot_leaching99', '1999 - Total N-leaching', FALSE, 'N-leaching', muni,title = expression('kg N ha'^-1))
p_l09 <- create_raster_leaching_gw(l09, 'nload_correct_gw09', '2009', TRUE, muni)
p_l99 <- create_raster_leaching_gw(l99, 'nload_correct_gw99', '1999', TRUE, muni)

#p1 <- p('A') + p_nl99 + pp
#p2 <- p('B') + p_nl09 + pp
p1 <- p('A') + p_l99 + pp
p2 <-  p('B') + p_l09 + pp

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

df_nc99$n_load_kgN <- df_nc99$n_load_kgN/1000 #t N
df_nc09$n_load_kgN <- df_nc09$n_load_kgN/1000 #t N

df_dif <- as.data.frame(cbind(df_nc09$GW_ID, df_nc09$n_load_kgN, df_nc99$n_load_kgN))
colnames(df_dif) <- c('GW_ID', 'nloads09', 'nloads99')
df_dif$dif <- df_dif$nloads09-df_dif$nloads99
df_dif$dif_prc <- df_dif$dif/df_dif$nloads99*100
df_dif <- data_cleaning(df_dif)

gw <- load_shp('gw')
gw99 <- merge(gw, df_nc99, 'GW_ID')

gw99 <- subset(gw99, gw_data0_1 != 'Ind')
gw09 <- merge(gw, df_nc09, 'GW_ID')
gw99 <- merge(gw, df_nc99, 'GW_ID')

gw09 <- subset(gw09, aquifer_ID != 'Ind')
gw99 <- subset(gw99, gw_data0_1!='Ind')

write_results('Nc', as.data.frame(gw09), 'gw_dataset09')

gw_dif <- merge(gw, df_dif, 'GW_ID')
gw_dif <- subset(gw_dif, gw_data0_1!='Ind')
subset <- subset(gw, gw_data0_1 == 'Ind')

p2 <- create_gw_leaching_result(subset, gw99) 

p3 <- p('C') + create_gw_leaching_result(subset, gw99) 
p4 <- p('D') + create_gw_leaching_result_proportion(subset, gw_dif)

pll <- map_arrange(p_nl99, p_dif, 'GW_Nloads.jpeg')
plot_together <- master_plot_nload(p_l99, p_l09, p_nl99, p_dif, height = 11, width = 6.5, 'N__leaching_GW.pdf')

p09 <- p('B') + 
  tm_shape(gw09) + tm_polygons(col='Nc_mgNO3L',breaks = c(0, 25, 50, 75, 100, +Inf), textNA = NULL,
                               palette = c('blue1', 'green1', 'yellow1', 'orange1', 'red1')) + 
  tm_legend(show=legend, position=c(0.7, 0.3), frame=F)+
  tm_scale_bar(color.dark = 'black', text.color = 'black',
               position=c(0.63, 0.05), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1.1, fontsize = 0.7,
             position=c(0.1, 0.9)) +
  tm_layout(frame=T,
            legend.text.size = 0.71,
            panel.show = T,
            legend.title.size = 0.9,
            panel.labels = '2009',
            fontfamily = 'serif',
            # legend.width = 2.3,
            panel.label.height = 1)
p
p99 <-p('A') + 
  tm_shape(gw99) + tm_polygons(col='Nc_mgNO3L',breaks = c(0, 25, 50, 75, 100, +Inf), textNA = NULL,
                               palette = c('blue1', 'green1', 'yellow1', 'orange1', 'red1'))

pp_f <- tmap_arrange(p99, p09, ncol=2)
tmap_save(pp_f, 'save_me.pdf', dpi=600)
gw_p99 <-  pp +create_raster_gw(subset, gw99, 'Nc.mg.NO3.L.', '1999', TRUE, 'Nc', '(mg NO3/L)')
gw_p99
gw_p09 <- create_raster_gw(subset, gw09, 'Nc.mg.NO3.L.', '2009', TRUE, 'Nc', '(mg NO3/L)')
plot_together <- map_arrange(gw_p99, gw_p09, 'GIS_Nc_paperr_gw_correct.jpeg')

master_plot <- master_plot_nload(p1, p2, p3, p4, height = 11, width = 6.5, 'master_plot_correct_v1.pdf')
master_plot <- master_plot_nload(p1, p2, p3, p4, height = 5, width = 11, 'master_plot_correct_v2.jpeg')

