source('./NLoading_module/Function/Compute_gw_loadings.R')
source('./NLoading_module/Function/Compute_gw_recharge_loading.R')
source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')

library(plotly)

#call aquifer db per aquifer_ID
#not needed
#gw_db99 <- gw_recharge_db(1999)
#gw_db09 <- gw_recharge_db(2009)

#summarise_means 
#not needed
#gw99 <- gw_rech_mean_all(gw_db99)
#gw09 <- gw_rech_mean_all(gw_db09)



df09$drainage <- df09$drainage/1000000000
df = df09 %>% group_by(kkk) %>% select(drainage, nloading,kkk) %>% summarise(mean_d = mean(drainage, na.rm=TRUE),


df_to_plot <- compile_gw_rech_for_plot()
#df_to_plot$prec <- df_to_plot$prec/10
df_to_plot$facet1 <- 'GW N-loads'
df_to_plot$facet2 <- 'Precipitation'
df_to_plot$recharge <- as.numeric(df_to_plot$recharge)
df_to_plot$kkk <- cut(df_to_plot$recharge, c(0, 10, 20, 30, 40, 50, 70))

df99 <- df_to_plot[which(df_to_plot$year==1999),]
df09 <- df_to_plot[which(df_to_plot$year==2009),]


ggplot(df_to_plot, aes(x=kkk, y=nloading, fill=year)) + geom_bar(stat='identity', position='dodge') 


################################# GGPLOT2 TRY #######################################
#####################################################################################
nol <- ggplot(df_to_plot, aes(x=recharge, y=nloading)) + 
  geom_bar(aes(fill=year), stat='identity', position='dodge', width=0.9) +
  geom_errorbar(aes(x=recharge, ymin=ifelse(nloading-sd<0, 0, nloading-sd), ymax=nloading+sd, fill=year), width=.1, position=position_dodge(0.9)) + 
  facet_grid(facet1~.) + 
  theme_test() + 
  labs(fill='Year') + 
  scale_y_continuous(name='N-loads (mg N/L)', expand=c(0,0), limits=c(0, 110)) + 
  scale_x_discrete(name='Average recharge rate (%)') + 
  theme(
    axis.title.x =element_blank(),
    axis.title.y = element_text(size=15),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=15),
    #strip.text.x = element_text(size=15),
    strip.text.y = element_text(size=15),
    text=element_text(family='serif', size=15),
    legend.position = c(0.9, 0.9),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15))
nol


prec <- ggplot(df_to_plot, aes(x=recharge)) + 
  geom_line(aes(y=prec, group=year, color=year), size = 1.5) + 
  scale_y_continuous(name='Precipitation (cm)', expand=c(0,0), limits=c(0, 100)) + 
  scale_x_discrete(name='Average recharge rate (%)') + 
  facet_grid(facet2~.) + 
  theme_test() + 
  theme(
    axis.title.x =element_text(size=15),
    axis.title.y = element_text(size=15),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    #strip.text.x = element_text(size=15),
    strip.text.y = element_text(size=15),
    text=element_text(family='serif', size=15),
    legend.position = c(0.9, 0.9),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15),
    panel.grid.major = element_line(color='gray50', size = 0.2),
    panel.grid.major.y = element_blank()
    ) + 
  labs(color='Year')
prec
general_ggplot_arrange(nol, prec, "GGPLOT2_RECH_PLOT.jpeg", FALSE)

################################# PLOTLY TRY #######################################
#####################################################################################
font1 <- list(
  family = "Old Standard TT, serif",
  size = 18,
  color = "black"
)

font2 <- list(
  family = "Old Standard TT, serif",
  size = 15,
  color = "black"
)

font3 <- list(
  family = "Old Standard TT, serif",
  size = 15,
  color = "black"
)

xx <- list(
  title = "Avg recharge rate (%)",
  titlefont = font1,
  showticklabels = TRUE,
  tickfont = font2,
  zeroline=TRUE,
  showline=TRUE
)

yy <- list(
  title = "N-loading (mg N/L)",
  titlefont = font1,
  showticklabels = TRUE,
  tickfont = font2,
  zeroline=TRUE,
  showline=TRUE,
  mirrr='ticks',
  range = c(0, 110),
  linecolor = 4,
  linecolor = 4,
  ticks='outside',
  autotick = TRUE,
  axis.automargin=TRUE,
  showgrid=FALSE
)

double_y <- list(
  titlefont = font1,
  tickfont = font2,
  overlaying = "y",
  side = "right",
  title = "Precitation (mm)",
  family='Old Standard TT, serif',
  linecolor = 4,
  range=c(0, 1200),
  
  ticks='outside',
  autotick = TRUE,
  axis.automargin=TRUE,
  showgrid=FALSE
)

m = list(
  l = 60,
  r = 60,
  b = 2,
  t = 20,
  pad = 0
)


p1 <- plot_ly(df_to_plot, x=~recharge) %>%
  add_trace(y =~nloading, type='bar', name='Nol',
            error_y= list(type='data', array=df_to_plot$sd, color='black'),
              transforms= list(
                list(
                  type='groupby',
                  groups=df_to_plot$year,
                  styles = list(
                  list(target = 1999, value = list(marker =list(color = 'blue'))),
                  list(target = 2009, value = list(marker =list(color = 'red')))
                  )
                )
              )
            ) %>%
  add_trace(df_to_plot, x=~recharge, y=~prec, name='Prec', type='scatter', mode='lines',yaxis='y2',
            transforms= list(
              list(
                type='groupby',
                groups=df_to_plot$year,
                styles = list(
                  list(target = 1999, value = list(line =list(color = 'blue'))),
                  list(target = 2009, value = list(line =list(color = 'red')))
                )
            )
          )
      ) %>%
  layout(
    yaxis2 = double_y,
    xaxis = xx,
    yaxis = yy,
    legend = list(font = font3, xanchor='center', y=0.99, x=0.5, orientation='h'), 
    autosize=T,
    margin=m,
    barmode='group'
  )

p1
path <- plot_output()

Sys.setenv("plotly_username" = "jgvms18")
Sys.setenv("plotly_api_key" = "wKz5vwECLhGj3X9DlRhw")

#orca(p1, file='PLOTLY_rech_plot.pdf')
  
plotly_IMAGE(p1, format = 'pdf', out_file = paste0(path, 'PLOTLY_rech_plot.pdf'), dpi=600)



