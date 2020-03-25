source('./WaterBalance_module/Functions/Water_Balance_funcs.R')
source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')

library(raster)
library(GADMTools)
library(rworldmap)
library(rworldxtra)
library(tmap)

world <- getMap(resolution='high')
sb <- subset(world, ADMIN== 'Spain' | ADMIN=='Portugal')
pt <- subset(world, ADMIN=='Portugal')

p <- tm_shape(sb, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_fill(col='grey85') + 
    tm_layout(bg.color='lightblue')
    #tm_shape(pt, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) +tm_borders(col='black') + 
    #tm_fill(col='grey85')
p

compute_gw_WB_dif <- function() {
  # computes the difference in terms of LITRE and % between 2009 and 1999
  
  wb09 <- read.csv(select_WB_subfolder_file('GW', 'GW_wb_df', 2009))
  wb99 <- read.csv(select_WB_subfolder_file('GW', 'GW_wb_df', 1999))
  
  df_wb_dif <- wb09[, -ncol(wb09)]
  df_wb_dif$WB_dif_L <- wb09$wsurplus_L-wb99$wsurplus_L
  df_wb_dif$WB_dif_hm3 <- df_wb_dif$WB_dif_L/1e9
  df_wb_dif$WB_dif_prct <- df_wb_dif$WB_dif_L/wb99$wsurplus_L*100
  return(df_wb_dif)
}

list_data_GW_WB_plot <- function(subset) {
  
  wb99 <- read.csv(select_WB_subfolder_file('GW', 'GW_wb_df', 1999))
  wb_dif <- compute_gw_WB_dif()
  
  wb99 <- wb99[wb99$GW_type=='Aq', ]
  wb99$wsurplus_hm3 <- wb99$wsurplus_L/1e9
  wb_dif <- wb_dif[wb_dif$GW_type=='Aq', ]
  
  return(list(wb99, wb_dif))
}
gw <- load_shp('gw')
p1 <- merge(gw, list_data_GW_WB_plot()[[1]], 'GW_ID')
p2 <- merge(gw, list_data_GW_WB_plot()[[2]], 'GW_ID')

p_right <- p + tm_shape(p2) + tm_borders(col='black') + 
  tm_polygons(col='WB_dif_prct',
              title = '\u0394Water\nBalance (%)',
              breaks = c(-Inf, -75, -50, -25, 0, +Inf), midpoint=NA, textNA=NULL, colorNA=NULL,
              palette = c('red1', 'orange1', 'yellow1', 'green1', 'blue1'),
              label = c('<-75', '-75 - -50', '-50 - -25', '-25 - 0', '>0')) +
  tm_legend(legend.outside=F, legend.position = c(0.7, 0.3)) + 
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.7,
               position=c(0.63, 0.05), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1, text.size = 1,
             position=c(0.1, 0.9)) +
  tm_layout(legend.title.size = 1.3, legend.text.size=0.9, panel.label.size = 1.1, # legend.width=2, 
            frame=T,                                 
            fontfamily = 'serif', panel.label.height = 1.1, panel.labels='Change in Water Balance')
p_right
p_left <- p +                                                  
tm_shape(p1) + tm_borders(col='black') + 
  tm_polygons(col='wsurplus_hm3',
              palette = 'Blues',
              textNA=NULL,
              colorNA=NULL,
              style='cont',
              breaks = c(0, 15, 30, 45, 75, 560),
              title=expression(paste('WB (', hm^3, ')'))) + tm_borders(col='black') + 
    tm_legend(legend.outside=F, legend.position = c(0.7, 0.3)) + 
    tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.7,
                 position=c(0.63, 0.05), breaks = c(0, 50, 100)) + 
    tm_compass(type='4star', size=1, text.size = 1,
               position=c(0.1, 0.9)) +
    tm_layout(legend.title.size = 1.3, legend.text.size=0.9, panel.label.size = 1.1, # legend.width=2, 
              frame=T,                                 
              fontfamily = 'serif', panel.label.height = 1.1, panel.labels='1999')
map_arrange(p1 = p_left, p2 = p_right, name = 'WB_plots.tiff', heigth = 6.5, width = 8, ncol = 2)
