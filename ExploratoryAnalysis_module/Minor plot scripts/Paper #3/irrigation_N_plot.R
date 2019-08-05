source('./Irrigation_module/Functions/Global_irrigation_functions.R')
source('./GIS_module/Function/General_GIS_functions.R')
source('./GIS_module/Function/LU_irrigation_allocation.R')
source('./Irrigation_module/Functions/Compute_crop_irrigatioN.R')

library(tmap)


## ---------------------------------------------------------------------------------------------

## CROP N IRRIGATION ---------------------------------------------------------------------------

## ---------------------------------------------------------------------------------------------

d <- compute_cropN_hierarchy(2009)
max5 <- data.frame(crop=c('rice', 'maize', 'forage maize', 'olive', 'perma grassland'),
                   crop_N = c(5427193, 3805749, 1864911, 1314813, 1013883))
max5$crop_N <- max5$crop_N/1000000
max5$prop <- round(max5$crop_N/17*100, 0)
max5$info <- 'Crops 2009'

p_crop <- ggplot(max5, aes(y=crop_N)) + geom_bar(aes(x=crop), stat='identity', color='black', fill='orange') + 
  scale_y_continuous(name = 'Crop N irrigation (kt N)', limits = c(0, 6.3), breaks=c(0, 1, 2, 4, 6), expand=c(0,0)) + 
  theme_bw() + 
  facet_grid(~info) + 
  geom_text(aes(x=crop, label=paste0(prop, ' %'), y = crop_N + 0.2), position = position_dodge(0.9), vjust=0, family='serif', size = 7.5) + 
  theme(
    axis.title.x =element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size=22),
    axis.text.x = element_text(size=16),
    axis.text.y = element_text(size=18),
    strip.text.x = element_text(size=18),
    text=element_text(family='serif', size=18),
    legend.position = 'None',
    legend.text = element_text(size=18),
    legend.title = element_text(size=18))
ggsave('major_cropN.jpeg', dpi=600)

## ---------------------------------------------------------------------------------------------

## MAIN CROP N IRRIGATION ---------------------------------------------------------------------------

## ---------------------------------------------------------------------------------------------

d <- compute_maincropN_hierarchy(2009)
sum_maincrop <- as.data.frame(colSums(d[, seq(4, ncol(d))]))
sum_maincrop[, 2] <- colnames(d[seq(4, ncol(d))])
colnames(sum_maincrop) <- c('maincrop_N', 'main_crop')
sum_maincrop$maincrop_N <- sum_maincrop$maincrop_N/1000000
sum_maincrop$prop <- round(sum_maincrop$maincrop_N/17*100, 1)
sum_maincrop$info <- 'Main crops 2009'

p_main <- ggplot(sum_maincrop, aes(y=maincrop_N)) + geom_bar(aes(x=main_crop), stat='identity', color='black', fill='green') + 
  scale_y_continuous(name = 'Main crop N irrigation (kt N)', limits = c(0, 10.3), breaks=c(0, 1, 2, 5, 7.5, 10), expand=c(0,0)) + 
  theme_bw() + 
  coord_flip() + 
  facet_grid(~info) + 
  geom_text(aes(x=12, y=8), label='(as % of total irrigation N)', family='serif', size = 6) + 
 geom_text(aes(x=main_crop, label=paste0(prop, ' %'), y = maincrop_N + 0.2), position = position_dodge(0.3), hjust=.1, 
           family='serif', size = 6) + 
  theme(
    axis.title.x =element_text(size=22),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size=16),
    axis.text.y = element_text(size=18),
    strip.text.x = element_text(size=18),
    text=element_text(family='serif', size=18),
    legend.position = 'None',
    legend.text = element_text(size=18),
    legend.title = element_text(size=18))
p_main
plot <- gridExtra::grid.arrange(p_main, p_crop, ncol=2, widths=2.5:1)
ggsave('crop_plot.jpeg', plot, dpi=600)

s## ---------------------------------------------------------------------------------------------

## IRRIGATION N PER LU CLASS -------------------------------------------------------------------

## ---------------------------------------------------------------------------------------------

irrigN_lu99 <- raster(get_irrig_LU_data(1999, 'N_irrigation', 'gross_irrigation', 'mosaic'))
irrigN_lu09 <- raster(get_irrig_LU_data(2009, 'N_irrigation', 'gross_irrigation', 'mosaic'))

pt <- getData('GADM', country='PRT', level=0)


irrigN_plot <- function(raster_irrigN, label_title, pt) {
  #general plot for irrigation N LU CLASS
  plot <- tm_shape(pt, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black') + 
  tm_shape(raster_irrigN, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + 
    tm_raster(col='LU_mosaic_IrrigN_', breaks = c(0, 0.001, 5, 10, 25, 50, 100, +Inf),
                                      palette = c('azure4', 'blue1', 'forestgreen', 'green1', 'yellow1', 'orange1', 'red1'),
                                      labels = c('No irrigation', '< 5', '5 - 10', '10 - 25', '25 - 50', '50 - 100', '> 100'),
                                      title = 'Irrigation N\n(kg N/ha LU)') + 
    tm_legend(show=T, position=c(0.7, 0.32), frame=F) + 
    tm_scale_bar(color.dark = 'black', text.color = 'black',
                 position=c(0.63, 0.05), breaks = c(0, 50, 100)) + 
    tm_compass(type='4star', size=1.1, fontsize = 0.7,
               position=c(0.1, 0.9)) + 
    tm_layout(legend.title.size = 1.2, legend.width = 2.5, legend.position = c(0.7, 0.32), panel.label.size = 1.2,
              frame=TRUE, legend.text.size=0.9, panel.labels=label_title, fontfamily = 'serif', panel.label.height = 1.2)  
  
  return(plot)
}

p09 <- irrigN_plot(irrigN_lu09, '2009', pt)
p99 <- irrigN_plot(irrigN_lu99, '1999', pt)

plot <- tmap_arrange(p99, p09, ncol=2)
tmap_save(plot, 'irrigN_plot_LU.jpeg', dpi=600, width = 8)

## ---------------------------------------------------------------------------------------------

## IRRIGATION N PER HA municiapltiy ------------------------------------------------------------

## ---------------------------------------------------------------------------------------------
n_irrig99 <- get_output_file('Irrigation_N', 1999, 'wo_eff_total')
n_irrig99[is.na(n_irrig99)==TRUE] <- 0


n_irrig09 <- get_output_file('Irrigation_N', 2009, 'wo_eff_total')
n_irrig09[is.na(n_irrig09)==TRUE] <- 0

#irrig N 2009 total irrigated areas
irrig_tot09 <- get_irrig_total_areas(2009)
n_irrig09$nha_irrig <- round(n_irrig09$SUM/irrig_tot09$Irrig_tot, 0)
n_irrig09[is.na(n_irrig09)==TRUE] <- 0

muni <- get_muni_shp()
irr99 <- merge(muni, n_irrig99, 'Muni_ID')
irr09 <- merge(muni, n_irrig09, 'Muni_ID')

gnb_prop <- read.csv('./Activity data/GNB_data/Inputs09.csv')
prop <- merge(muni, gnb_prop, 'Muni_ID')

irrigN_uaa_plot <- function(irrigN, col_name, label_title, title) {
  
  plot <- tm_shape(pt, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black') + 
    tm_shape(irrigN, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + 
    tm_polygons(col=col_name, breaks = c(0, 0.001, 2.5, 5, 10, 20, 30, +Inf), #textNA='NA', colorNA='azure4',
              palette = c('azure4', 'blue1', 'forestgreen', 'green1', 'yellow1', 'orange1', 'red1'),
              labels = c('NA', '< 2.5', '2.5 - 5', '5 - 10', '10 - 20', '20 - 30', '> 30'),
              title = title) + 
    tm_legend(show=T, position=c(0.7, 0.32), frame=F) + 
    tm_scale_bar(color.dark = 'black', text.color = 'black',
                 position=c(0.63, 0.05), breaks = c(0, 50, 100)) + 
    tm_compass(type='4star', size=1.1, fontsize = 0.7,
               position=c(0.1, 0.9)) + 
    tm_layout(legend.title.size = 1.2, legend.width = 2.5, legend.position = c(0.7, 0.32), panel.label.size = 1.2,
              frame=TRUE, legend.text.size=0.9, panel.labels=label_title, fontfamily = 'serif', panel.label.height = 1.2)  
  return(plot)
}

p99 <- irrigN_uaa_plot(irr99, 'nha', '1999', 'Irrigation N\n(kg N/ha UAA')
p09 <- irrigN_uaa_plot(irr09, 'nha', 'Irrig N vs UAA 2009', 'Irrigation N\n(kg N/ha UAA')
p09_irrig <- irrigN_uaa_plot(irr09, 'nha_irrig', 'Irrig N vs ha irrigated 2009', 'Irrigation N\n(kg N/ha irrig)')



plot_prop <- irrigN_uaa_plot(prop, 'prop_irrig', 'Proportion of irrigation N in total N-inputs', 'Irrigation N\n(% of N-inputs)')
plot_prop
tmap_save(plot_prop, 'irrigN_prop.jpeg', dpi=600)

plot <- tmap_arrange(p09, p09_irrig, ncol=2)
tmap_save(plot, 'irrigN_plot_UAA_irrigated.jpeg', dpi=600, width = 8)




## MAINLAND N FLOWS PLOT ---------------------------------------------------------------

n_flows <- data.frame(inputs=c('Atmospheric deposition', 'Sludge', 'Fertiliser N', 'Gross manure N', 'N fixation', 'Irrigation'),
                      values = c(2.3, 0.6, 25.9, 40.8, 2.6, 4.8))
n_flows$info <- 'Mainland N flows 2009'
p2 <- ggplot(n_flows, aes(x=inputs)) + geom_bar(aes(y=values), stat='identity', color='black', fill='green') + 
  scale_y_continuous(name = 'N-inputs at the mainland level (kg N/ha)', 
                     limits = c(0, 45), breaks=c(0, 5, 10, 20, 30, 40), expand=c(0,0)) + 
  geom_text(aes(x=inputs, label=values, y= values + 2), position=position_dodge(0.3), hjust=0.6, family='serif', size=6) + 
  theme_test() + 
  coord_flip() + 
  facet_grid(~info) + 
  theme(
    axis.title.x = element_text(size=22),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size=16),
    axis.text.y = element_text(size=18),
    strip.text.x = element_text(size=18),
    text=element_text(family='serif', size=18),
    legend.position = 'None',
    legend.text = element_text(size=18),
    legend.title = element_text(size=18))
ggsave('N_flows.jpeg', dpi=600)

sb <- subset(gnb_prop, prop_irrig!=0)
sb$info <- '2009 Distribution'

p1 <- ggplot(sb, aes(prop_irrig)) + geom_histogram(breaks=c(0, 5, 10, 20, 30, 70), fill='green', color='black') + 
  scale_x_continuous(name = 'Irrigation N as proportion of N-inputs (%)', breaks = c(0, 5, 10, 20, 30, 70)) + 
  scale_y_continuous(name = 'Number of municipalities (No.)', breaks = c(5, 25, 50, 100, 140), limits=c(0, 150), expand=c(0,0)) + 
  theme_test() + 
  facet_grid(~info) + 
  theme(
    axis.title.x = element_text(size=22),
    axis.title.y = element_text(size=22),
    axis.text.x = element_text(size=16),
    axis.text.y = element_text(size=18),
    strip.text.x = element_text(size=18),
    text=element_text(family='serif', size=18),
    legend.position = 'None',
    legend.text = element_text(size=18),
    legend.title = element_text(size=18))

plot <- ggpubr::ggarrange(p2, p1, nrow=2, heights = 1:3)
plot
ggsave('N_flows_dist.jpeg', dpi=600)
