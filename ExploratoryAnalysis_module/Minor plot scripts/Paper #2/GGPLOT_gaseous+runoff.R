source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')
source('./Other_N_balances/Function/N_balance_functions.R')

#optional
#dataframe with these N losses
df99 <- gaseous_losses_runoff(1999)
df09 <- gaseous_losses_runoff(2009)

##############################################################################################
###################################### SUMMARY DATA #####################################
View(summary(df09[, seq(4,7)]))
View(summary(df99[, seq(4,7)]))


db99 <- compute_N_losses(1999)[[2]]
db09 <- compute_N_losses(2009)[[2]]
db99$year <- 1999
db09$year <- 2009
db <- rbind(db99, db09)
db <- melt_df(db, 'loss', 'kt_N')
labels <- c(expression(NH[3]), expression(paste(N[2], 'O')), expression(NO[x]), 'Runoff')

p1 <- ggplot(db, aes(factor(loss, level=c('NH3', 'N2O', 'NOx', 'Runoff')), value, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge', color='black') + 
  scale_y_continuous(name = 'Total losses (kt N)', limits=c(0, 50), expand=c(0,0), breaks=c(0, 5, 10, 20, 30, 40, 50)) + 
  scale_x_discrete(label=labels) + 
  theme_test() + 
  theme(
    axis.title.x =element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=18),
    strip.text.x = element_text(size=18),
    text=element_text(family='serif', size=18),
    legend.position = 'None',
    legend.text = element_text(size=18),
    legend.title = element_text(size=18))
p1
##############################################################################################
###################################### BOTH YEARS IN SAME PLOT #################################
df <- ggplot_same_plot_gaseous_losses_runoff()
plot <- same_plot_gaseous_runoff_boxplot(df, "GGPLOT_same_N_losses_lel.jpeg")

final_plot <- plot + annotation_custom(ggplotGrob(p1), ymin=20, ymax=55, xmin=2)
ggsave(plot =final_plot, filename = paste0(plot_output(), 'GGPLOT_SAME_n_LOSSES_MERGED.jpeg'), dpi=600, height = 6, width = 7.5)
final_plot
########################################################################################
###################################### USING FACET_GRID #################################
db_plot99 <- melt_gaseous_losses_runoff(1999)
db_plot09 <- melt_gaseous_losses_runoff(2009)

p99 <- gaseous_runoff_boxplot(db_plot99, 15)
p09 <- gaseous_runoff_boxplot(db_plot09, 0)
general_ggplot_arrange(p99, p09, 'GGPLOT_gaseous+runoff.pdf', 'FALSE)'





