source('./GIS_module/Function/General_GIS_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')
source('./Main_functions.R')


compute_change_uaa_hydro <- function()
{
  hydro <- load_shp('main_hydro')
  hydro <- spTransform(hydro, CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))
  hu_names <- hydro$nome
  year <- c(1999, 2009)
  df <- data.frame(hu = hu_names)
  
  gw <- load_shp('gw')
  gw <- spTransform(gw, CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))
  
  for (i in year)
  {
    uaa <- get_GIS_file(paste0('caaRP', year_prefix(i)), 'LandCover')
    adj <- get_modelling_files(folder = 'Adjustment_factor', filename = paste0('rast_adj_factor', year_prefix(i)))
    
    ctr <- 1
    ifelse(i==1999, id <- 2, id <- 3)
    
    for (j in hu_names)
    {
      sb <- subset(hydro, nome==j)
      adj_uaa <- uaa*adj
      crop <- crop(adj_uaa, extent(sb))
      mask <- mask(crop, sb)
      sum <- cellStats(mask, 'sum')
      df[ctr, id] <- sum
      
      ctr <- ctr + 1
      print(j)
    }
  }
  df$dif <- df$V3-df$V2
  df$dif_prop <- df$dif/df$V2*100
  
  colnames(df) <- c('hu_id', '1999', '2009', 'dif_uaa', 'dif_uaa_prct')
  write_results('Drainage', file = df, filename = 'uaa_hu_change')
}

#compute_change_uaa_hydro()


compute_change_uaa_gw <- function()
{
  year <- c(1999, 2009)
  
  gw <- load_shp('gw')
  gw <- spTransform(gw, CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))
  
  gw <- subset(gw, gw_data0_1 != 'Ind')
  ids <- gw$GW_ID
  df <- data.frame(hu = ids)
  
  for (i in year)
  {
    uaa <- get_GIS_file(paste0('caaRP', year_prefix(i)), 'LandCover')
    adj <- get_modelling_files(folder = 'Adjustment_factor', filename = paste0('rast_adj_factor', year_prefix(i)))
    
    ctr <- 1
    ifelse(i==1999, id <- 2, id <- 3)
    
    for (j in ids)
    {
      sb <- subset(gw, GW_ID==j)
      adj_uaa <- uaa*adj
      crop <- crop(adj_uaa, extent(sb))
      mask <- mask(crop, sb)
      sum <- cellStats(mask, 'sum')
      df[ctr, id] <- sum
      
      ctr <- ctr + 1
      print(j)
    }
  }
  df$dif <- df$V3-df$V2
  df$dif_prop <- df$dif/df$V2*100
  
  colnames(df) <- c('gw_id', '1999', '2009', 'dif_uaa', 'dif_uaa_prct')
  df$dif <- df$V3-df$V2
  df$dif_prop <- df$dif/df$V2*100
  write_results('Drainage', file = df, filename = 'uaa_gw_change')
}
df <- get_results('Drainage', 'uaa_gw_change')
df <- df[, seq(2,7)]
View(df)

p2 <- ggplot(df, aes(dif_uaa_prct)) + geom_histogram(aes(fill=X.1, group=X.1), color='black', position='stack', bindwidth=15,  alpha=.5) + 
  scale_x_continuous(limits=c(-45, 85), breaks=c(-45, -30, -15, 0, 15, 30, 45, 70, 85)) + 
  scale_y_continuous(expand=c(0,0), limits=c(0, 12.5), breaks=c(0, 1, 3, 6, 9, 12)) + 
  theme_test() + 
  xlab('Change in UAA (%)') +
  ylab('Number of aquifers (No.)') + 
  theme(
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text.x = element_text(size=16),
    axis.text.y = element_text(size=15),
    strip.text.x = element_text(size=16),
    text=element_text(family='serif', size=15),
    legend.position = 'none',
    legend.text = element_text(size=15),
    legend.title = element_text(size=16)) 
p2

df_hu <- df %>% group_by(X.1) %>% summarise(sum = sum(dif_uaa))
df_hu$sum <- df_hu$sum/1000
colnames(df_hu)[1] <- 'HU'

p3 <- ggplot(df_hu, aes(x=HU, y=sum)) + geom_bar(aes(fill=HU), color='black', alpha=.5, stat='identity') + 
  scale_y_continuous(name='Change in UAA (1000 ha)', limit=c(-40, 0), expand=c(0,0)) + 
 # scale_fill_viridis_d() + scale_color_viridis_d() + 
  theme_test() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=18),
    legend.position='bottom', 
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=16),
    strip.text.x = element_text(size=20),
    text=element_text(family='serif', size=20),
    legend.text = element_text(size=15),
    legend.title = element_blank()) 
p3

final_plot <- p2 + annotation_custom(ggplotGrob(p3), xmin=.4, ymin=5)
final_plot

plot <- gridExtra::grid.arrange(p1, final_plot, ncol=2)
path <- 
ggsave('UAA_gw_hu.pdf', plot=final_plot, dpi=600)
