source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')
source('./Main_functions.R')


gnb_dataset <- function(year) {
  
  gnb <- select_maindata_pattern('GNB')
  gnb_year <- list.files(gnb, pattern=year_prefix(year), full.names=TRUE)
  
  gnb_df <- read.csv(gnb_year[1])
  gnb_df <- merge(gnb_df, read.csv(gnb_year[2]), by=c('Muni_ID', 'ID', 'Muni'))
  names(gnb_df) <- c('Muni_ID', 'ID', 'Muni', 'Atmo deposition', 'Sludge N', 'Fertiliser N', 'Gross N manure', 
                'N fixation', 'Irrig', 'Fodder offtake', 'Crop offtake', 'Residues removed', 'Residues burnt', 'other')
  gnb_df$other <- NULL
  gnb_df$Irrig <- NULL
  return(gnb_df)
}

prepare_gnb_dataset <- function(year) {
  
  gnb_df <- gnb_dataset(year)
  new_df <- melt(gnb_df, c('Muni_ID', 'ID', 'Muni'))
  idx <- which(new_df$variable=='Atmo deposition' | 
                 new_df$variable=='Sludge N' |
                 new_df$variable=='Fertiliser N' |
                 new_df$variable=='Gross N manure' |
                 new_df$variable=='N fixation') 
  new_df[idx, 'var'] <- 'N-inputs'
  new_df[-idx, 'var'] <- 'N-outputs'
  return(new_df)
}

db99 <- prepare_gnb_dataset(1999)
db99$year <- 1999
db09 <- prepare_gnb_dataset(2009)
db09$year <- 2009

db <- rbind(db99, db09)
db$year <- as.factor(db$year)
p99 <- ggplot(db, aes(x=variable, y=value, color=var, fill=year)) + 
  geom_boxplot(lwd=1, outlier.size = 2) + theme_bw() + xlab('') + 
  ylab(expression('N flows, kg N ha'^-1)) + 
  labs(fill='Year', color='N flow', family='serif') + 
  scale_y_continuous(limits=c(0, 1350), 
                     breaks = c(0, 5, 25, 50, 150, 300, 600, 1200), 
                     expand=c(0,0), trans='pseudo_log') + 
  coord_flip() +
  scale_color_manual(values=c('black', 'blue3')) + 
  #scale_fill_manual(values=c('blue', 'green')) + 
  theme(
    legend.position = 'bottom',
    axis.title.x =element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=18),
    strip.text.x = element_text(size=20),
    strip.text.y = element_text(size=18),
    text=element_text(family='serif', size=18),
    legend.text = element_text(size=19),
    legend.title = element_text(size=19.5))

p2 <- ggplot(db, aes(x=variable, y=value, color=var, fill=year)) + 
  geom_boxplot(lwd=1.5, outlier.size = 1.5) + theme_bw(base_rect_size = 1.5, base_line_size = 1.5) + xlab('') + 
  ylab(expression('N flows, kg N ha'^-1)) + 
  labs(fill='Year', color='N flow', family='serif') + 
  scale_y_continuous(limits=c(0, 1350), 
                     breaks = c(0, 5, 25, 50, 150, 300, 600, 1200), 
                     expand=c(0,0), trans='pseudo_log') + 
  scale_color_manual(values=c('black', 'brown')) + 
  #scale_fill_manual(values=c('blue', 'green')) + 
  theme(
    legend.position = 'bottom',
    axis.title.x =element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text.x = element_text(size=18, angle=45),
    axis.text.y = element_text(size=18),
    strip.text.x = element_text(size=20),
    strip.text.y = element_text(size=18),
    text=element_text(family='serif', size=15, hjust=1),
    legend.text = element_text(size=19),
    legend.title = element_text(size=19.5))
p2
ggsave('Potsdam_Nflows.tiff', dpi=600, compression='lzw')



## N inputs and outputs

inputs_idx <- which(db$variable=='Atmo deposition' | db$variable =='Sludge N' | db$variable=='Fertiliser N' | db$variable=='Gross N manure' | db$variable == 'N fixation')
inputs <- db[inputs_idx, ]
outputs <- db[-inputs_idx, ]

p_inputs <- ggplot(inputs, aes(x=variable, y=value, fill=year)) + 
  geom_boxplot(lwd=1, outlier.size = 2) + theme_bw() + xlab('') + 
  ylab(expression('N inputs, kg N ha'^-1)) + 
  labs(fill='Year', family='serif') + 
  scale_y_continuous(limits=c(0, 1350), 
                     breaks = c(0, 5, 25, 50, 150, 300, 600, 1200), 
                     expand=c(0,0), trans='pseudo_log') + 
 # coord_flip() +
  scale_color_manual(values=c('black', 'blue3')) + 
  #scale_fill_manual(values=c('blue', 'green')) + 
  theme(
    legend.position = c(0.1, 0.9),
    axis.title.x =element_text(size=22),
    axis.title.y = element_text(size=22),
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=18),
    strip.text.x = element_text(size=20),
    strip.text.y = element_text(size=18),
    text=element_text(family='serif', size=18),
    legend.text = element_text(size=19),
    legend.title = element_text(size=19.5))
p_inputs
ggsave('N_inputs.tiff', dpi=600, compression='lzw')

p_outputs <- ggplot(outputs, aes(x=variable, y=value, fill=year)) + 
  geom_boxplot(lwd=1, outlier.size = 2) + theme_bw() + xlab('') + 
  ylab(expression('N outputs, kg N ha'^-1)) + 
  labs(fill='Year', family='serif') + 
  scale_y_continuous(limits=c(0, 1350), 
                     breaks = c(0, 5, 25, 50, 150, 300, 600, 1200), 
                     expand=c(0,0), trans='pseudo_log') + 
  # coord_flip() +
  scale_color_manual(values=c('black', 'blue3')) + 
  #scale_fill_manual(values=c('blue', 'green')) + 
  theme(
    legend.position = c(0.9, 0.9),
    axis.title.x =element_text(size=22),
    axis.title.y = element_text(size=22),
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=18),
    strip.text.x = element_text(size=20),
    strip.text.y = element_text(size=18),
    text=element_text(family='serif', size=18),
    legend.text = element_text(size=19),
    legend.title = element_text(size=19.5))
p_outputs
ggsave('N_outputs.tiff', dpi=600, compression='lzw')
