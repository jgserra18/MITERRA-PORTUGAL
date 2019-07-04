#LOAD LIBRARIES
library(ggplot2)
library(tmap)
library(grid)

source('./Main_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')
source('./Gaseous_functions.R')
source('./Other_N_balances/Function/N_balance_functions.R')
source('./Runoff_module/Function/Compute_runoff_application.R')
source('./Leaching_module/Function/Compute_leaching.R')
source('./NLoading_module/Function/Compute_gw_loadings.R')

################################################################################################################
################################################# LOAD DATA ####################################################

retrieve_data <- function(pattern, year, pattern2)
{
  ncol_df <- select_output_data(pattern ,year, pattern2)
  main_df <- create_main_csv()
  
  main_df <- cbind(main_df, ncol_df)
  main_df <- data_cleaning(main_df)
  
  return(main_df)
}

remove_duplic_cols <- function(df)
{
 df <- df[, !duplicated(colnames(df))]
 
 return(df)
}

melt_df <- function(df, by_col, vars)
{
  new_df <- melt(df, by=by_col, measure.vars=vars)
  
  return(new_df)
}

################################################################################################################
################################################# N BALANCES ####################################################
n_balance_data <- function(year)
{
  gnb_db <- retrieve_data('GNB', year)
  ssnb_db <- load_ssnb_muni(year, TRUE, 'tier2')
  ns_db <- ns_compute('NH3', year=year)
  
  main_df <- cbind(gnb_db, ssnb_db, ns_db)
  main_df <- remove_duplic_cols(main_df)
  main_df <- data_cleaning(main_df)
  
  return(main_df)
  
}

#returns list with the worked df of nutrient balances for 99 and 09
n_budgets_years_db <- function()
{
  db99 <- n_balance_data(1999)
  db09 <- n_balance_data(2009)
  
  db99$year <- 1999
  db09$year <- 2009
  
  df99 <- melt_df(db99, c('Muni_ID', 'year'),c('gnb', 'ns', 'ssnb'))
  df09 <- melt_df(db09, c('Muni_ID', 'year'),c('gnb', 'ns', 'ssnb'))
  
  return(list(df99, df09))
}


#merges n balances with leaching
merging_leaching_n_balances <- function(year)
{
  leaching<- load_leaching_muni(year) #default tier2
  leaching <- leaching[, ncol(leaching)]
  n_balances <- n_balance_data(year)

  merged_df <- cbind(n_balances, leaching)


  return(merged_df)
}

#prepare the merged df with leaching and all the n balances
#by melting it 
prepare_merged_leaching_n_balance <- function(year)
{
  main_df <- merging_leaching_n_balances(year)
  melt_df <- melt_df(main_df, 'Muni_ID', c('gnb', 'ns', 'ssnb'))

  return(melt_df)

}

#corrects the negative values to 0
#NOTE: THIS IS THE FUNCTION TO CALL THE DATA FOR SCATTERPLOT ANALYSIS
correct_leaching_n_balance_data <- function(year)
{
  main_df <- prepare_merged_leaching_n_balance(year)
  main_df[main_df< 0] <- 0
  main_df$year <- as.numeric(year)

  return(main_df)
}

################################################################################################################
################################################# RUNOFF AND GASEOUS ####################################################
#merges gaseous N losses and runoff
#by default it is tier2
gaseous_losses_runoff <- function(year)
{
  gas_db <- ssnb_compile_tot_gaseous(year, 'tier2')
  rf_db <- runoff_output_data('Runoff', year, 'total')
  
  main_df <- cbind(gas_db, rf_db)
  
  return(main_df)
}

#this function is used to plot these losses in the same plot for both years
#the output is prepared to be plotted
ggplot_same_plot_gaseous_losses_runoff <- function()
{
  db99 <- gaseous_losses_runoff(1999)
  db99$year <- 1999
  
  db09 <- gaseous_losses_runoff(2009)
  db09$year <- 2009
  
  main_df <- rbind(db99, db09)
  main_df <- melt_df(main_df, 'Muni_ID', c('tot_NH3', 'tier2_tot_N2O', 'tot_NOx', 'runoff_nha'))
  main_df$N_losses <- 'Gaseous and runoff N losses'
  
  return(main_df)
}

#compute N losses in kilotonnes N 
#output is a list
compute_N_losses <- function(year)
{
  main_df <- gaseous_losses_runoff(year)
  uaa <- load_uaa(year)
  
  sum_df <- data.frame(loss=c('NH3', 'N2O', 'NOx', 'Runoff'), kt_N = vector(mode='numeric', length=4))
  
  for (i in 4:ncol(main_df))
  {
    main_df[, i] <- main_df[, i]*uaa
    sum_df[i-3, 2] <- sum(main_df[, i], na.rm=TRUE)/1000000
  }

  return(list(main_df, sum_df))
}


#for plotting
melt_gaseous_losses_runoff <- function(year)
{
  main_df <- gaseous_losses_runoff(year)
  cols <- colnames(main_df)
  cols <- cols[seq(4, length(cols))]
  
  main_df <- melt_df(main_df, 'Muni_ID', cols)
  main_df$year <- as.numeric(year)
  
  return(main_df)
}

################################################################################################################
################################################# GW CALL DATA ####################################################
#loads all the necessary data required for exploratory analysis
load_gw_data <- function(year, tier_leaching)
{
  df <- CORRECT_gw_complete_dataset(year, tier_leaching)
  
  #need to merge with recharge rates
  
  return(df)
}

#loads average precipitation for each groundwater body
load_gw_prec <- function(year, tier_leaching)
{
  df <- aggregate_gw_dataset(year, tier_leaching)
  cols <- c('CAA_ID', 'Muni_ID', 'aquifer_ID', paste0('prec', year_prefix(year)))
  
  df <- df[cols]
  colnames(df)[ncol(df)] <- 'prec'

  df_avg <- average_if(df, 'aquifer_ID')
  df_avg <- df_avg[c(4,5)]
  
  return(df_avg)
}

#NOTE: NO3_recharge_rate
#calculates the means for all variables (leach, drainage, prec, nloading)
gw_rech_mean_all <- function(year, tier_leaching)
{
  df <- CORRECT_gw_complete_dataset(year, tier_leaching)

  df_rech_5 <- df[which(df$recharge==5), ] #rech_rate=5
  df_rech_5 <- df_rech_5 %>% select(recharge, everything()) #change first col to match df
  
  df = df %>% group_by(recharge) %>% summarise_all(funs(mean))
  df[2, ] <- df_rech_5

  df$recharge <- as.factor(df$recharge)
  df$year <- as.numeric(year)
  
  return(df)
}

#computes sd for nloadings
gw_rech_sd <- function(year, tier_leaching)
{
  df <- CORRECT_gw_complete_dataset(year, tier_leaching)
  df = df %>% group_by(recharge) %>% summarise_each(funs(sd), nloading)
  df[is.na(df)] <- 0
  colnames(df)[2] <- 'sd'
  
  return(df)
}

gw_rbind_dfs <- function(function_1, for_plot)
{
  df99 <- function_1(1999)
  df09 <- function_1(2009)
  df <- rbind(df99, df09)

  if(for_plot==TRUE){ df$year <- as.factor(df$year) }
  
  return(df) 
}


compile_gw_rech_for_plot <- function()
{
  df_sd_nloading <- gw_rbind_dfs(gw_rech_sd, FALSE)
  df_mean <- gw_rbind_dfs(gw_rech_mean_all, TRUE)

  final_df <- cbind(df_mean, df_sd_nloading[, 2])

  return(final_df)
}

################################################################################################################
################################################# GGPLOT MODULE ####################################################
plot_output <- function()
{
  path <- './ExploratoryAnalysis_module/Plots/'
  return(path)
}

#boxplot with the different n budgets
#REQUIREMENT: MELT_DF
n_budget_boxplot <- function(df_plot, y_axis)
{
  
  plot <- ggplot(data=df_plot, aes(x=variable, y=value)) + 
    geom_boxplot() + 
    geom_hline(yintercept = 0) + 
    theme_test() + 
    facet_grid(~year) + 
    scale_y_continuous(name=expression(paste('Different N balances (kg N ', ha^-1, ')')),
          limits=c(-50, 1000), breaks=c(0, 200, 400, 600, 800, 1000)) + 
    theme(axis.title.x = element_blank(),
            text=element_text(family = "serif"))
  
  if (y_axis==FALSE)
  {
    #override plot
    plot <- plot %+% theme(axis.text.y = element_blank(),
                         axis.title.y = element_blank(),
                         axis.title.x = element_blank())
  }
  
  return(plot)
}

#note: the df must be melted
gaseous_runoff_boxplot <- function(df_plot, hide_yy)
{
  labels <- c(expression(NH[3]), expression(paste(N[2], 'O')), expression(NO[x]), 'Runoff')
  
  ggplot(df_plot, aes(variable, value)) + 
    geom_boxplot(outlier.shape = NA, width=0.5, position ='dodge' ,fill='grey80') + 
    scale_y_continuous(name='Different N losses (kg N/ha)', limits=c(0, 56), expand=c(0,0)) + 
    scale_x_discrete(labels=labels)+ 
    #facet_grid(~year) + 
    theme_bw() + 
    theme(text=element_text(family='serif', size=15),
          axis.title.x =element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size=hide_yy),
          axis.text.x = element_text(size=15),
          axis.text.y = element_text(size=hide_yy),
          strip.text.x = element_text(size=15))
}

#note: only for the output of ggplot_same_plot_gaseous_losses_runoff()
same_plot_gaseous_runoff_boxplot <- function(df_plot, name)
{
  labels <- c(expression(NH[3]), expression(paste(N[2], 'O')), expression(NO[x]), 'Runoff')
  
  plot <- ggplot(df_plot, aes(variable, value, fill=factor(year))) + 
    geom_boxplot(position='dodge', outlier.shape = NA) + 
    scale_y_continuous(name = 'Different N losses (kg N/ha)', limits=c(0, 56), expand=c(0,0)) + 
    scale_x_discrete(label=labels) + 
   # facet_grid(~N_losses) + 
    theme_test() + 
    theme(
          axis.title.x =element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size=20),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=18),
          strip.text.x = element_text(size=18),
          text=element_text(family='serif', size=18),
          legend.position = c(0.91, 0.85),
          legend.text = element_text(size=18),
          legend.title = element_text(size=18)) + 
    labs(fill='Year')
  
  path <- plot_output()
  
  ggsave(plot =plot, filename = paste0(path, name), dpi=600, height = 6, width = 7.5)
  
  return(plot)
}


general_ggplot_arrange <- function(plot1, plot2, name, y_grob)
{
  ifelse(y_grob==FALSE,
      full_plot <- gridExtra::grid.arrange(plot1, plot2, nrow=2),
      full_plot <- gridExtra::grid.arrange(plot1, plot2, nrow=2,
                            left=textGrob(label='N-leaching (kg N/ha)', rot=90,
                              gp=gpar(fontfamily='Times', fontsize=15))))

  path <- plot_output()
  
  ggsave(plot =full_plot, filename = paste0(path, name), dpi=600)
}

#specific for n_budget_boxplot
ggplot_arrange <- function(p1, p2, name)
{
  plot1 <- n_budget_boxplot(p1, TRUE)
  plot2 <- n_budget_boxplot(p2, FALSE)
  
  full_plot <- gridExtra::grid.arrange(plot1, plot2, ncol=2)
  path <- plot_output()
  
  ggsave(plot =full_plot, filename = paste0(path, name), dpi=600)
}


#=========================================================================================================#
#=========================================================================================================#
#returns a list with a dataframe for the scatterplot analysis of N balances vs N leaching
#list index 1 returns the column names exept the first 3 cols
#list index 2 returns leaching colname
colnames_r_squared_new_df <- function(year)
{
  db_file <- merging_leaching_n_balances(year)
  col_names <- colnames(db_file)
  col_names <- col_names[4:(length(col_names)-1)] #disregard first 3 idxs

  main_df <- data.frame(variable=col_names, rq=vector('numeric', length=length(col_names)))

  return(main_df)
}

#note: can be improved
#stores the r squared from the scatterplot analysis N-leaching vs N balances
#returns a dataframe with 1 col with variable names and other with r_squares
store_r_squared <- function(year)
{
  db_file <- merging_leaching_n_balances(year)

  template_df <- colnames_r_squared_new_df(year)
  rsquared <- sapply(db_file[, 4:6], function(q) summary(lm(db_file$leaching ~ q))$r.squared) 

  template_df[, 2] <- round(rsquared, 2)
  
  template_df <- template_df[order(template_df$variable), ] #order as gnb, ns, ssnb

  return(template_df)
}


#try to melt the maindf all the NB stuff and keep n leaching
#calls the leaching and N balance MELTED dataframe
#as well as the calculated R² 
#and plots a scatterplot
ggplot_scatter_nb_leaching <- function(year, xx_axis, strip_facet)
{
  df <- correct_leaching_n_balance_data(year)
  r_sq <- store_r_squared(year)

  df_plot <- ggplot(df, aes(x=value, y=leaching, group=variable))  + geom_point() + facet_grid(year~variable) + 
  geom_smooth(method='lm', width=0.5, formula='y~x-1') + 
  geom_text(data = r_sq, aes(label=paste(expression(R^2), '=', r_sq[, 2]), x=820, y=7),
    family='serif') + 
  theme_test() + 
  scale_x_continuous(name = "Different N balance approaches (kg N/ha)",
                     limits=c(0, 1000), breaks = seq(0, 900, 200), expand = c(0,0)) + 
  scale_y_continuous(name='N-leaching (kg N/ha)',
                     limits=c(0, 100), breaks=seq(0, 100, 20), expand=c(0,0)) +
  theme(
        text=element_text(family='serif', size=15),
        strip.text.y = element_text(size=15)  ,
        axis.text.y = element_text(size=15),
        axis.title.y = element_blank(),
        ) 

  ifelse(xx_axis==TRUE,
    df_plot <- df_plot + theme(axis.text.x = element_text(size=15),
                               axis.title.x = element_text(size=15)),
    df_plot <- df_plot + theme(axis.text.x = element_blank(),
                               axis.title.x = element_blank())
    )

  ifelse(strip_facet==TRUE,
    df_plot <- df_plot + theme(strip.text.x = element_blank()),
      df_plot <- df_plot + theme(strip.text.x = element_text(size=15)))

  return(df_plot)
}


################################################################################################################
################################################# TMAP MODULE ####################################################

#creates default map for leaching/n-loadings
create_map <- function(map_plot, col_plot, panel_plot, legend, title, muni, unit){
  tm_shape(muni) + tm_borders(col='grey') + tm_fill(col='grey') + 
  tm_shape(map_plot) + tm_borders(col=NA, lwd = 0, alpha = 0) + 
    tm_polygons(col=col_plot,
                title = paste0(title, '\n', unit), breaks=c(0, 10, 20, 50, +Inf),
                palette = c('blue1', 'green1', 'yellow1', 'red'),
                label = c('<10', '10-20', '20-50', '>50'),
                textNA = 'NA', colorNA='black')+
    tm_legend(show=legend, position=c(0.73, 0.3), frame=F)+
    tm_scale_bar(color.dark = 'black', text.color = 'black',
                 position=c(0.63, 0.009), breaks = c(0, 50, 100)) + 
    tm_compass(type='4star', size=1.5,
               position=c(0.025, 0.9)) +
    tm_layout(frame=T,
              legend.text.size = 0.9,
              panel.show = T,
              legend.title.size = 1.2,
              panel.labels = panel_plot,
              fontfamily = 'Times',
              legend.width = 2.3,
              panel.label.height = 1.2,
              #bottom, left, top, right
              inner.margins = c(0.01, 0, 0.01, 0.05))
}

caa_nc_plot_main_hydro <- function(map_plot, col_plot, panel_plot, legend, title, muni, unit)
{
  hydro <- load_shp('main_hydro')
  hydro$initial <- c('H', 'W', 'TS', 'W', 'W', 'M')

  plot <- create_map(map_plot, col_plot, panel_plot, legend, title, muni, unit)
  plot + tm_shape(hydro) + tm_borders(lwd=1.2, col='black') + 
        tm_text('initial', auto.placement=TRUE, xmod=-0.6, fontface='bold', col='black')

}

#creates map for precipitation
prec_map <- function(map_plot, panel_plot)
{
    tm_shape(map_plot) + 
    tm_raster(breaks=c(0, 500, 1000, 2000, 3000), palette = 'Blues', title = 'Precipitation\n(mm)') + 
    tm_legend(show=TRUE, position=c(0.69, 0.3), frame=F)+
    tm_scale_bar(color.dark = 'black', text.color = 'black',
                 position=c(0.55, 0.009), breaks = c(0, 50, 100)) + 
    tm_compass(type='4star', size=1.5,
               position=c(0.025, 0.9)) +
    tm_layout(frame=T,
              legend.text.size = 0.9,
              panel.show = T,
              panel.label.bg.color = 'white',
              legend.title.size = 1.2,
              panel.labels = panel_plot,
              fontfamily = 'Times',
              panel.label.height = 1.2,
              #bottom, left, top, right
              inner.margins = c(0.01, 0, 0.01, 0.2))
}

gis_gw_plot <- function(map_plot, col_plot, panel_plot, legend, title, unit){

    gw_ind_inters <- load_shp('ind_aquifer')

    tm_shape(map_plot) + tm_borders(col='black', lwd = 0.4) + 
    tm_polygons(col=col_plot,
                title = paste0(title, '\n', unit), breaks=c(0, 10, 20, 50, +Inf),
                palette = c('blue1', 'green1', 'yellow1', 'red'),
                label = c('<10', '10-20', '20-50', '>50'), showNA=FALSE)+
    tm_legend(show=legend, legend.outside=FALSE, position=c(.73, 0.3), frame=F)+
    tm_scale_bar(color.dark = 'black', text.color = 'black',
                 position=c(0.63, 0.009), breaks = c(0, 50, 100)) + 
    tm_compass(type='4star', size=1.5,
               position=c(0.025, 0.9)) +
    tm_shape(gw_ind_inters) + tm_borders(col='black', lwd=0.5, alpha=0.3) +   
    tm_layout(frame=T,
              legend.text.size = 0.9,
              panel.show = T,
              legend.title.size = 1.2,
              legend.width =2.3,
              panel.labels = panel_plot,
              fontfamily = 'Times',
              legend.text.fontfamily ='Times',          
              panel.label.height = 1.2,
              #bottom, left, top, right
              inner.margins = c(0.01, 0, 0.01, 0.05))
}

#V function name
gis_prec_map_hist <- function(df_plot, col, pane_label)
{
  plot <- tm_shape(df_plot) + tm_polygons(col=col, palette = 'Blues', 
                                          legend.hist=TRUE, legend.hist.width=1.1,
                                          breaks=c(0, 500, 1000, 1500, 2000, 3000),
                                          labels = c('< 500', '500 - 1000', '1000 - 1500', '1500 - 2000', '2000 - 3000'),
                                          title='Precipitation (mm)') + 
    tm_legend(legend.outside=FALSE, position=c(.6, 0.25)) + 
    tm_scale_bar(color.dark = 'black', text.color = 'black', 
                 position=c(0.63, 0.009), breaks = c(0, 50, 100)) + 
    tm_compass(type='4star', size=1.5,
               position=c(0.025, 0.9)) +
    tm_layout(frame=TRUE,   
              legend.text.fontfamily ='Times',          
              legend.text.size = 0.9,
              panel.show = T,
              legend.title.size = 1.2,
              panel.labels = pane_label,
              fontfamily = 'Times',
              legend.width = 2.3,
              panel.label.height = 1.2,
              #bottom, left, top, right
              inner.margins = c(0, 0, 0, 0.3))
  
  return(plot)
}

#arranges two different tmap plots as one and then saves it to a default path
map_arrange <- function(p1, p2, name, heigth, width)
{
  path <- plot_output()
  
  pl <- tmap_arrange(p1, p2, ncol=2)
  
  print('Saving this now...')
  tmap_save(pl, paste0(path, name), dpi =600, height = 6.5, width = 8)
}

master_plot_nload <- function(p1, p2, p3, p4, height, width, name)
{
  path <- plot_output()
  
  pl <- tmap_arrange(p1, p2, p3, p4, ncol=2, nrow=2)
  print('Saving this now...')
  tmap_save(pl, paste0(path, name), dpi =600, height=height, width=width)

}
