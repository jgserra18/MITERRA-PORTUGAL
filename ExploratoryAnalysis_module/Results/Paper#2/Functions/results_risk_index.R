source('./Leaching_risk_index/Functions/Computation_Risk_index.R')
source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')

gw <- readOGR('./Activity data/GIS_data/gw.shp')
subset <- subset(gw, gw_data0_1!='Ind')
subset <- spTransform(subset, proj4string(rc_mrt09))


rc_mrt09 <- round(raster(get_risk_data(subfolder = 'GIS_index', filename = paste0('MRT_reclass', year_prefix(2009)))), 0)
rc_mrt99 <- round(raster(get_risk_data(subfolder = 'GIS_index', filename = paste0('MRT_reclass', year_prefix(1999)))), 0)
mrt <- stack(rc_mrt99, rc_mrt09)
mrt <- mask(mrt, subset)

rc_nl09 <- round(raster(get_risk_data(subfolder = 'GIS_index', filename = paste0('NC_reclass', year_prefix(2009)))), 0)
rc_nl99 <- round(raster(get_risk_data(subfolder = 'GIS_index', filename = paste0('NC_reclass', year_prefix(1999)))), 0)
nl <- stack(rc_nl99, rc_nl09)
nl <- mask(nl, subset)

rc_ri09 <- round(raster(get_risk_data(subfolder = 'GIS_index', filename = paste0('Leaching_index', year_prefix(2009)))), 0)
rc_ri99 <- round(raster(get_risk_data(subfolder = 'GIS_index', filename = paste0('Leaching_index', year_prefix(1999)))), 0)
ri <- stack(rc_ri99, rc_ri09)
ri <- mask(ri, subset)

index_map <- function(r_map, title_plot, gw_subset, lab, leg) {
  
  
  tm_shape(sb, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black', lwd = 1) + tm_fill(col='grey') + 
    tm_shape(r_map, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + 
    tm_raster(breaks = c(1,2,3,4,5), style='cat', legend.show = F,
              palette = c('blue1', 'green1', 'yellow1', 'orange1', 'red1'),
              labels=c('Very low', 'Low', 'Moderate', 'High', 'Very high'),
              title =title_plot) + 
    tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.7,
                 position=c(0.55, 0.02), breaks = c(0, 50, 100)) + 
    tm_compass(type='4star', size=0.9, text.size = 0.7,
               position=c(0.05, 0.9)) +
    tm_shape(gw_subset, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black', lwd = 1) +
    tm_add_legend(type='fill', labels = c('Very low', 'Low', 'Moderate', 'High', 'Very high'), 
                  col =  c('blue1', 'green1', 'yellow1', 'orange1', 'red1'), 
                  border.col = 'black', title = title_plot) + 
    tm_layout(legend.position = c(0.7, 0.3)) +
    tm_layout(frame=T,
              panel.label.height = 1,
              panel.label.size = 1,
              panel.labels =lab,
              legend.position = c(0.7, 0.3),
              legend.text.size = 0.8,
              legend.frame = F,
              bg.color = 'lightblue',
              panel.show = T,
              legend.title.size = 1,
              fontfamily = 'serif')
}

p1 <- index_map(nl[[1]], 'Hazard', subset, '1999', F)
p2 <- index_map(nl[[2]], 'Hazard', subset, '2009', T)
p3 <- index_map(mrt[[1]], 'Vulnerability', subset, '1999', F)
p4 <- index_map(mrt[[2]], 'Vulnerability', subset, '2009', T)
p5 <- index_map(ri[[1]], 'Risk', subset, '1999', F)
p6 <- index_map(ri[[2]], 'Risk', subset, '2009', T)
p <- tmap_arrange(p1,p2,p3,p4,p5,p6, ncol=2, nrow=3)

tmap_save(p, './test3.jpeg', dpi=100, height = 15, width = 7)
# master plot Nc vs Leaching index
# note: I didn't bother to properly implement this
# all the Nc data comes from Exploratory_GIS.R

master_plot_nload(gw_p99, gw_p09, d99, d, height = 10, width = 7, name = 'Nc_Leaching_index.jpeg')


