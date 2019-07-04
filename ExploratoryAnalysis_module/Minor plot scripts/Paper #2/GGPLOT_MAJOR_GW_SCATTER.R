source('./NLoading_module/Function/Compute_gw_loadings.R')
source('./NLoading_module/Function/Compute_gw_recharge_loading.R')
source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')


##############################################################################################
######################################## FUNCTION #############################3#############
modify_gw_plot <- function(year)
{
  df <- gw_rech_mean_all(year)
  df$recharge <- as.numeric(df$recharge)
  df$drainage <- df$drainage/1000000000 #hm3
  df$leaching_mg <- df$leaching_gw/1000000000 #tonnes N
  df$recharge_rates <- cut(df$recharge, c(0, 10, 20, 30, 40, 50, 70))
  df$N_loads <- cut(df$nloading, c(0, 10, 25, 50, +Inf))
  df$N_loads <- factor(df$N_loads, levels = c('(0,10]', '(10,25]', '(25,50]', '(50,Inf]'),
                       labels=c('<10', '10-25', '25-50', '>50'))
  df$year1 <- as.numeric(year)
  
  return(df)
}

##############################################################################################
######################################## MANUAL HIST #############################3#############
bar09 <- data.frame(range=c('<10', '10-20', '20-30', '30-40', '40-50', '>50'),
                    no3 = c(18.9, 12.4, 5.8, 5.5, 4.9, 3.1))
bar99 <- data.frame(range=c('<10', '10-20', '20-30', '30-40', '40-50', '>50'),
                    no3 = c(15.7, 9.4, 5.6, 3.9, 4.9, 2.4))

gw_bar_plot <- function(df)
{
  plot <- ggplot(df, aes(x=factor(range, level=df$range), y=no3)) + 
    geom_bar(stat='identity', fill='lightgrey', color='black', alpha=.5) + 
    xlab('Recharge rates (%)') + ylab(expression(paste(NO[3]^-~'',~'levels (mg N/L)'))) + 
    geom_text(aes(y=no3+1, label=round(no3, 1)), family='serif', size=5.5) + 
    scale_y_continuous(limits=c(0, 25), expand=c(0,0)) +
    theme_test() + 
    theme(
      axis.title.x =element_text(size=18),
      axis.title.y = element_text(size=18),
      axis.text.x = element_text(size=18),
      axis.text.y = element_text(size=18),
      strip.text.x = element_text(size=18),
      text=element_text(family='serif', size=16),
      legend.text = element_text(size=18),
      legend.title = element_text(size=18))
  
  return(plot)
}

p1_09 <- gw_bar_plot(bar09)
p1_99 <- gw_bar_plot(bar99)

################################################################################################
######################################## MAIN SCATTER #############################3#############
df09 <- modify_gw_plot(2009)
df99 <- modify_gw_plot(1999)

p2_99 <- ggplot(df99, aes(x=drainage, y=leaching_mg)) + geom_point()+ 
  geom_smooth(method='lm', se=F, color='black', lwd=0.5) + 
  geom_jitter(aes(size=N_loads, color=N_loads))  + 
  ylab('N-leaching (tonnes N)') + 
  #geom_vline(xintercept = 2.5, linetype=4, lwd=0.3) + 
  scale_color_manual(values = c('blue', 'green', 'orange', 'red')) + 
  scale_x_continuous(breaks=seq(0, 240, 60), expand=c(0, 0), limits=c(0, 250)) + 
  scale_y_continuous(limits=c(0, 2820), breaks=seq(0, 2800, 400),expand=c(0,0)) + 
  theme_test() + 
  facet_grid(~year1) + 
  theme(
    axis.title.x =element_blank(),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    strip.text.x = element_text(size=18),
    text=element_text(family='serif', size=18),
    legend.text = element_text(size=18),
    legend.position = 'none',
    legend.title = element_text(size=18))

lines <- list(bquote(NO[3]^-~''~'concentration'), '(mg')

p2_09 <- ggplot(df09, aes(x=drainage, y=leaching_mg)) + geom_point()+ geom_smooth(method='lm', se=F, color='black', lwd=0.5) + 
  geom_jitter(aes(size=N_loads, color=N_loads))  + 
  ylab('N-leaching (tonnes N)') + 
  labs(color=expression(atop(NO[3]^-~''~levels, paste('(mg N/L)'))), size=expression(atop(NO[3]^-~''~levels, paste('(mg N/L)'))))+
  scale_color_manual(values = c('blue', 'green', 'orange', 'red')) + 
  scale_x_continuous(breaks=seq(0, 240, 60), expand=c(0, 0), limits=c(0, 250)) + 
  scale_y_continuous(limits=c(0, 2820), breaks=seq(0, 2800, 400),expand=c(0,0)) + 
  theme_test() + 
  facet_grid(~year1) + 
  theme(
    axis.title.x =element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size=18),
    axis.text.y = element_blank(),
    strip.text.x = element_text(size=18),
    text=element_text(family='serif', size=18),
    legend.text = element_text(size=18),
    legend.position = c(0.9, 0.89),
    legend.title = element_text(size=18)) 

final_plot09<- p2_09 + annotation_custom(ggplotGrob(p1_09), xmin=5, xmax= 150, ymin=1400, ymax=2780)

final_plot99<- p2_99 + annotation_custom(ggplotGrob(p1_99), xmin=5, xmax= 150, ymin=1400, ymax=2780)

full_plot <- gridExtra::grid.arrange(final_plot99, final_plot09, ncol=2,
                                     bottom=textGrob(label=expression(paste('Drainage (', hm^3, ')')),
                                                     gp=gpar(fontfamily='serif', fontsize=20)))
path <- plot_output()
ggsave(full_plot, filename = paste0(path, 'GGPLOT_GW_SCATTER_correct.pdf'), dpi=600, height = 10, width = 18)                                  
