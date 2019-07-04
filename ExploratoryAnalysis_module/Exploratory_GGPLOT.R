source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')
source('./NLoading_module/Function/Compute_gw_loadings.R')

##################################################################################################################
############################################ N BALANCES BOXPLOTS #################################################
#load n balance data
df99 <- n_budgets_years_db()[[1]]
df09 <- n_budgets_years_db()[[2]]

#LOAD PLOTS
n_balance_plot <- ggplot_arrange(df99, df09, 'GGPLOT_n_balances.pdf')
n_balance_plot <- ggplot_arrange(df99, df09, 'GGPLOT_n_balances.jpeg')

##################################################################################################################
############################################ N BALANCES + LEACHING ################################################
#load melted df with leaching vs N balances AND df with R squares
#totally optional
#df <- correct_leaching_n_balance_data(1999)
#df1 <- correct_leaching_n_balance_data(2009)
#df_rq <- store_r_squared(1999)
#df_rq1 <- store_r_squared(2009)

plot99 <- ggplot_scatter_nb_leaching(1999, FALSE, FALSE)
plot09 <- ggplot_scatter_nb_leaching(2009, TRUE, TRUE)
plot09
main_plot <- general_ggplot_arrange(plot99, plot09, "GGPLOT_merged_scatter.jpeg", TRUE)

#sort r2 by variable and then plot it
#do boxplot with all gaseous N losses


ggplot_scatter_nb_leaching <- function(year, xx_axis, strip_facet)
{
  df <- correct_leaching_n_balance_data(year)
  r_sq <- store_r_squared(year)
  
  df_plot <- ggplot(df, aes(x=value, y=leaching, group=variable))  + geom_point() + facet_grid(year~variable) + 
    geom_smooth(method='lm', width=0.5, formula='y~x-1') + 
    geom_text(data = r_sq, aes(x=820, y=7),label=expression(paste(R^2~'='~, r_sq[, 2])),
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
