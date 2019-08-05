source('./GIS_module/Function/GW_computation_functions.R')

library(ggplot2)

## create maindataset of aquifers for both years --------------------------------------------------------------------------

df99 <- get_main_df_gw_dataset(1999, F) %>% filter(gw_type=='Aq') %>% dplyr::select(GW_ID, Nc.mg.NO3.L., HU, recharge)
df09 <- get_main_df_gw_dataset(2009, F)%>% filter(gw_type=='Aq') %>%dplyr::select(GW_ID, Nc.mg.NO3.L., HU, recharge)
colnames(df09)[2] <- '2009'
colnames(df99)[2] <- '1999'

df <- merge(df99, df09[, c('GW_ID', '2009')], 'GW_ID', all.x=F, all.y=F)
df_m <- melt(df, measure.vars = c('1999', '2009'), id.vars=c('GW_ID', 'HU', 'recharge'))
df_m[1:60, 'year'] <- 1999
df_m[61:120, 'year'] <- 2009

## Calculate dif in Nc --------------------------------------------------------------------------

df$diff <- df$`2009`-df$`1999`
df$diff_prct <- df$diff/df$`1999`*100

cnt <- df %>% group_by(diff, diff_prct, GW_ID) %>% filter(diff<0) %>% count(diff)

gw <- load_shp('gw')
see_gw <- merge(gw, cnt, 'GW_ID')

tm_shape(see_gw) + tm_polygons(col='diff', colorNA='azure4')

## Aggregate in recharge rates ranges --------------------------------------------------------------------------

new_df <- df_m %>% mutate(cat_rech = cut(recharge, breaks=c(0, 15, 30, 45, 62)))
avg_df <- new_df %>% group_by(cat_rech, year) %>% summarise(mean_rech = mean(value))
avg_df$year <- as.factor(avg_df$year)


## Aggregate the data to the main HU --------------------------------------------------------------------------

df_m_HU <- df_m %>% group_by(HU, year) %>% summarise(mean_nc = mean(value))
df_m_HU$mean_nc <- round(df_m_HU$mean_nc, 0)


## PLOTS ----------------------------------------------------------------------------------------------------

p_HU <- ggplot(df_m_HU, aes(x=year, y=mean_nc, group=HU)) + geom_line(aes(color=HU), size=2, alpha=.5) + 
  geom_point(aes(color=HU), size=4, alpha=.5) + 
  geom_vline(xintercept=1999, alpha=.5) + 
  geom_vline(xintercept=2009, alpha=.5) + 
  scale_x_continuous(breaks=c(1999, 2009), limits=c(1991, 2017), position='bottom') + 
  geom_text(data=df_m_HU %>% filter(year==1999), aes(label=paste0(HU, ': ', mean_nc, ' mg/L')), hjust=1.1, fontface='bold', family = 'serif', size =5) + 
  geom_text(data=df_m_HU %>% filter(year==2009), aes(label=paste0(HU, ': ', mean_nc, ' mg/L')), hjust=-0.11, fontface='bold', family='serif', size=5) + 
  theme_test() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size=15),
    axis.text.y = element_blank(),
    strip.text.x = element_text(size=16),
    text=element_text(family='serif', size=15),
    legend.position ='none',
    legend.text = element_text(size=15),
    legend.title = element_text(size=16)) 

avg_df$mean_rech <- round(avg_df$mean_rech, 0)
p_rech <- ggplot(avg_df, aes(x=cat_rech, y=mean_rech, group=year, fill=year, color=year)) + 
  geom_bar(stat='identity', position='dodge2') +
  geom_hline(yintercept = 52.5, linetype='dashed', alpha=.35) + 
  geom_hline(yintercept = 38.5, linetype='dashed', alpha=.35) + 
  geom_hline(yintercept = 31, linetype='dashed', alpha=.35) + 
  scale_x_discrete(labels = c("(0,15]" = '<15', "(15,30]" = "15-30", "(30,45]" = "30-45", "(45,62]" = "45-62")) + 
  scale_y_continuous(expand=c(0,0), limits = c(0,82), breaks = c(0, 10, 25, 50, 80)) + 
  geom_text(aes(label=mean_rech), color='black', position=position_dodge(width=0.9), vjust=-0.25, family='serif', size = 5) + 
  theme_test()+ 
  xlab('Aquifer recharge rates (%)') + ylab('Mean Nc values (mg NO3/L)') + labs(fill='Year', color='Year') + 
  theme(
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    strip.text.x = element_text(size=16),
    text=element_text(family='serif', size=15),
    legend.position = c(0.8, 0.85),
    legend.text = element_text(size=15),
    legend.title = element_text(size=16))
p_rech

plot <- gridExtra::grid.arrange(p_HU, p_rech, ncol=2)
path <- plot_output()
name <- file.path(path, 'AR_gw_HU.pdf')
ggsave(name, plot=plot, dpi=600)
