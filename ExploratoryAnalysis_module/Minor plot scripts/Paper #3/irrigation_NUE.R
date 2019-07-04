source('./Test_module/RUN_GNB_activity_data.R')
source('./GNB_module/Function/GNB_computation.R')
source('./Test_module/RUN_GNB.R')
source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')

irrig_impact09$cut <- cut(irrig_impact09$diff_nue, c(-Inf, -20,-10,-5, -0.0001, 0))
irrig_impact99$cut <- cut(irrig_impact99$diff_nue, c(-Inf, -20,-10,-5, -0.0001, 0))

irrig_nue09 <- ggplot(irrig_impact09) + 
  geom_point(aes(nue, irrig_nue)) + 
  geom_abline(intercept = 0, slope=1, color='black') + 
  ggalt::geom_encircle(data=irrig_impact09[irrig_impact09$irrig_nue<irrig_impact09$nue, ], aes(nue, irrig_nue), color='red', expand=0.01, fill='red', alpha=.2, spread=0.02) + 
  geom_jitter(aes(x=nue, irrig_nue, size=cut, color=cut)) + 
  scale_size_manual(values=c(10, 8, 6, 4, 2, 1), labels=c('<-20', '-20 - -10', '-10 --5', '>-5', 'No change')) + 
  scale_color_manual(values = c('red', 'orange', 'yellow', 'green', 'black'), labels=c('<-20', '-20 - -10', '-10 --5', '>-5', 'No change')) + 
  labs(size='NUE variation (%)', color='NUE variation (%)') + 
  xlab('Standard-NUE (%)') + 
  ylab('Irrigation-NUE (%)') + 
  theme_bw()
irrig_nue09

ggsave(path = paste0(plot_output(),'Paper #3/'), plot = irrig_nue09, dpi=600, filename = 'irrig_nue09_plot.pdf')
