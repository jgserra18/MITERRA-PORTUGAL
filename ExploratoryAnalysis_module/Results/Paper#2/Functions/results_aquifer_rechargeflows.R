source('./GIS_module/Function/General_GIS_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')
source('./Main_functions.R')
source('./GIS_module/Function/GW_computation_functions.R')
source('./GIS_module/Function/General_GIS_functions.R')


require(ggExtra)
require(cowplot)

gw <- load_shp('gw')
gw <- subset(gw, gw_data0_1 != 'Ind')
ids <- gw$GW_ID

df_gw09 <- get_df_Nc(2009)
df_gw99 <- get_df_Nc(1999)
df_gw99$drainage <- df_gw99$drainage/1000000000
df_gw09$drainage <- df_gw09$drainage/1000000000
df_gw09 <- df_gw09[ids, ]
df_gw99 <- df_gw99[ids, ]


df <- cbind(df_gw99[, c(1,2)], df_gw09[, 2])
colnames(df) <- c('GW_ID', '1999', '2009')

new_df <- melt(df, id.vars='GW_ID', measure.vars = c('1999', '2009'))

p <- ggplot(new_df, aes(value)) +
  geom_histogram(aes(fill=variable, color=variable), bins=10, position = 'identity', alpha=.5, breaks=c(0, 15, 30, 45, 75, 150, 300, 400, 520)) +
 scale_x_continuous(name=expression(paste('Recharge (', hm^3, ')')), breaks=seq(0, 600, 50)) + 
  scale_y_continuous(name='Number of aquifers (No.)', expand=c(0,0), limits=c(0, 52), breaks=c(0, 10, 20, 30, 40, 50)) + 
  theme_classic() + 
  labs(fill='Year', color='Year') + 
  theme(
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text.x = element_text(size=11),
    axis.text.y = element_text(size=11),
    strip.text.x = element_text(size=11),
    text=element_text(family='serif', size=10),
    legend.position = c(0.8, 0.7),
    legend.text = element_text(size=10),
    legend.title = element_text(size=10)) 
xbox <- axis_canvas(p, axis='x', coord_flip = T) + geom_boxplot(data=new_df, aes(y=value,fill=factor(variable), color=factor(variable)), alpha=.5) + 
  coord_flip() 
p1 <- insert_xaxis_grob(p, xbox, grid::unit(1, "cm"), position = "top")
ggdraw(p1)

path <- plot_output()
name <- file.path(path, 'gw_recharge_dist.pdf')
ggsave(name, p1, dpi=600)



