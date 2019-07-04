library(ggplot2)

df <- data.frame(x=seq(-10, 10), y=seq(-10,10), z=-seq(-10, 10))

ggplot(df, aes(x, y)) + geom_smooth(color='black') + 
  geom_smooth(aes(x=z), color='black') + 
  geom_ribbon(aes(xmin=predict(loess(y~z)), xmax=predict(loess(y~z)), ymin=predict(loess(y~z)), ymax=0),
              fill='darkgreen', alpha=.5) + 
  geom_ribbon(aes(xmin=-predict(loess(y~z)), xmax=-predict(loess(y~z)), ymin=-predict(loess(y~z)), ymax=0),
              fill='darkgreen', alpha=.5) +
  theme_void() + 
  geom_hline(yintercept=0, lwd=2) + 
  geom_vline(xintercept = 0, lwd=2) + 
  theme(
    text=element_text(family='serif', size=15),
    strip.text.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  ) + 
  scale_x_continuous(limits=c(-10, 10)) + 
  geom_text(aes(y=-.6, x=6.5), label=expression(Delta~italic('drainage (%)')), family='serif', size=9) + 
  geom_text(aes(y=6.5, x=-.6), label=expression(Delta~italic('leaching (%)')), family='serif', size=9, angle=90) + 
  geom_text(aes(y=-3, x=7), label='??? Dilution', family='serif', size=13) + 
  geom_text(aes(y=9, x=-5), label='??? Dilution', family='serif', size=13) +
  geom_text(aes(y=6, x=7.9), label=expression(D[state]~'= 1'), family='serif', size=8) + 
  geom_text(aes(y=-6, x=7.9), label=expression(D[state]~'= -1'), family='serif', size=8) + 
  geom_text(aes(y=6, x=-7.9), label=expression(D[state]~'= -1'), family='serif', size=8) + 
  geom_text(aes(y=-6, x=-7.9), label=expression(D[state]~'= 1'), family='serif', size=8)
  
  
ggsave('lol.jpeg', dpi=600)
                              
getwd()


