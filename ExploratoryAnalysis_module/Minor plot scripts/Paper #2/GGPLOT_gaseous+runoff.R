source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')
source('./Other_N_balances/Function/N_balance_functions.R')

## SSNB BOXPLOT ------------------------
db99 <- n_balance_data(1999, F)
db09 <- n_balance_data(2009, F)


db99 <- subset(db99, select=c(1,2,3,5))
db99$year <- 1999
db09 <- subset(db09, select=c(1,2,3,5))
db09$year <- 2009
main_ssnb_df <- cbind(db99, db09[, 4])
colnames(main_ssnb_df)[ncol(main_ssnb_df)] <- 'ssnb2'

melt__df <- melt(main_ssnb_df, c('Muni_ID'), c('ssnb', 'ssnb2'))
melt__df[1:278, 'year'] <- 1999
melt__df[279:556, 'year'] <- 2009
melt__df$info <- 'SSNB'
bpxlt <- ggplot(melt__df, aes(x=factor(year), y=value, fill=factor(year))) + 
  geom_violin() + 
  geom_boxplot(width=.25, outlier.size = 2.5) + 
  stat_summary(fun.y = 'mean', size=3, colour='black', geom='point', shape=10) + 
  scale_y_continuous(name = expression('SSNB ( kg N '~ha^-1~yr^-1~')'), trans = 'pseudo_log',
                     limits=c(0, 610), expand=c(0,0), breaks=c(0, 50, 100, 200, 400, 600)) + 
  theme_bw(base_line_size = 1, base_rect_size = 2) + 
  #facet_grid(~info) + 
  labs(fill='Year') + 
  theme(
    axis.title.x =element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=18),
    strip.text.x = element_text(size=18),
    text=element_text(family='serif', size=18),
    legend.position = c(0.25, 0.9),
    legend.text = element_text(size=18),
    legend.title = element_text(size=18)) 
bpxlt 

#optional
#dataframe with these N losses
df99 <- gaseous_losses_runoff(1999, F)
df09 <- gaseous_losses_runoff(2009, F)

##############################################################################################
###################################### SUMMARY DATA #####################################

db99 <- compute_N_losses(1999,F)[[2]]
db09 <- compute_N_losses(2009, F)[[2]]
lc09 <- data.frame(loss='Leaching', kt_N=15.9)
lc99 <-  data.frame(loss='Leaching', kt_N=23.5)
db09 <- rbind(db09, lc09)
db99 <- rbind(db99, lc99)
db99$year <- 1999
db09$year <- 2009

db <- rbind(db99, db09)
db <- melt_df(db, 'loss', 'kt_N')

leach_idx <- which(db[, 1]=='Leaching')
db <- db[-leach_idx,]
labels <- c(expression(NH[3]), expression(paste(N[2], 'O')), expression(NO[x]), 'Runoff')#, 'LeachN')

p1 <- ggplot(db, aes(factor(loss, level=c('NH3', 'N2O', 'NOx', 'Runoff')), value, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge', color='black') + 
  scale_y_continuous(name = 'Total losses (kt N)', limits=c(0, 50), expand=c(0,0), breaks=c(0, 5, 10, 20, 30, 40, 50)) + 
  scale_x_discrete(label=labels) + 
  coord_flip() + 
  theme_test() + 
  theme(
    axis.title.x =element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    strip.text.x = element_text(size=15),
    text=element_text(family='serif', size=15),
    legend.position = 'None',
    legend.text = element_text(size=18),
    legend.title = element_text(size=18)) 
p1

##############################################################################################
###################################### BOTH YEARS IN SAME PLOT #################################
df <- ggplot_same_plot_gaseous_losses_runoff(irrig_mode = F)
df <- df[-which(df$variable=='leaching_nha'), ]
df$info <- 'Environmental N losses'
plot <- same_plot_gaseous_runoff_boxplot(df, "GGPLOT_same_N_losses_lel1.pdf", leaching = FALSE)
plot
final_plot <- plot + annotation_custom(ggplotGrob(p1), ymin=50, ymax=450, xmin=2)
final_plot <- plot + annotation_custom(ggplotGrob(p1), xmin = 1.5, xmax=4.5, ymin=4, ymax=5.75)
final_plot
ggsave(plot =final_plot, filename = paste0(plot_output(), 'KKK.tiff'), dpi=600,  compress='lzw')

big_final <- gridExtra::grid.arrange(plot, bpxlt, ncol=2, widths=2:1)
ggsave(big_final, filename = paste0(plot_output(), 'OVERVIEW_PLOT.tiff'), compression='lzw',dpi=600)

########################################################################################
###################################### USING FACET_GRID #################################
db_plot99 <- melt_gaseous_losses_runoff(1999)
db_plot09 <- melt_gaseous_losses_runoff(2009)

p99 <- gaseous_runoff_boxplot(db_plot99, 15)
p09 <- gaseous_runoff_boxplot(db_plot09, 0)
general_ggplot_arrange(p99, p09, 'GGPLOT_gaseous+runoff.pdf', 'FALSE)'





