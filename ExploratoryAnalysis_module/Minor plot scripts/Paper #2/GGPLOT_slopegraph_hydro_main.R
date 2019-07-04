source('./NLoading_module/Function/compute_hydrogeological_loadings.R')

db99 <- db99[, c(1,3,4,5)]
colnames(db99) <- c('main_hydro', 'leachingv0', 'drainagevo', 'nloadingv0')

db09 <- db09[, c(3,4,5)]
colnames(db09) <- c('leachingv1', 'drainagev1', 'nloadingv1')

dff <- cbind(db99, db09)
View(dffclass(dff$nloadingv0)
     
     label_left0 <- paste(dff$main_hydro, round(dff$nloadingv0), sep=',')
     label_left1 <- paste(dff$main_hydro, round(dff$drainagevo), sep=',')
     
     label_right0 <- paste(dff$main_hydro, round(dff$nloadingv1), sep=',')
     label_right1 <- paste(dff$main_hydro, round(dff$drainagev1), sep=',')
     
     p <- ggplot(dff) + geom_segment(aes(x=1, xend=2, y=nloadingv0, yend=nloadingv1, group=main_hydro), colour='blue') +
       geom_segment(aes(x=1, xend=2, y=drainagevo, yend=drainagev1, group=main_hydro), colour='red') + 
       geom_vline(xintercept=1, linetype="dashed", size=.1) + 
       geom_vline(xintercept=2, linetype="dashed", size=.1) + 
       labs(x="", y='N-loads (mg N/L) || Drainage (hm3)') + 
       xlim(0.5, 2.5) + 
       scale_y_continuous(limits=c(0, 130)) + 
       facet_grid(~main_hydro)
     p
     p <- p + geom_text(label='1999', x=0.91, y=120, size=5) + 
       geom_text(label='2009', x=2.09, y=120, size=5) +
       geom_text(label=label_right0, y=dff$nloadingv1, x=2, hjust=-0.1) + 
       geom_text(label=label_right0, y=dff$drainagev1, x=2, hjust=-0.1) 
     