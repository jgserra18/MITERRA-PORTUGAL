source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')

nl_caa09 <- caa_corr_loading(2009)
colnames(gw_nl09)[1] <- 'GW_ID'

gww <- sf::st_read('/home/serra/grive/MITERRA/MITERRA-PORTUGAL/Activity data/GIS_data/gw.shp')
gww_df <- merge(gww, gw_nl09, 'GW_ID')
gww_df <- subset(gww_df, select=c(gw_type))
gww_df <- subset(gww_df, gww_df$gw_type=='Ind')

gw_ind <- merge(gww, gw_nl09, 'GW_ID')
gw_ind <- subset(gw_ind, gw_ind$gw_type=='Ind')


gridvals <- sf::st_make_grid(gw_ind, n=c(50, 50)) %>% 
  sf::st_cast("POLYGON")

res <- sf::st_intersection(gww_df, gridvals)

sf::st_write(res, './Activity data/GIS_data/ind_aquifer_intersect.shp')
