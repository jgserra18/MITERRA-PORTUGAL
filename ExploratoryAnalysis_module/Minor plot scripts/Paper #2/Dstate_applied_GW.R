library(ggplot2)

d <- read.csv('C:/Users/serrm/Desktop/sdasdas.csv')
View(d)

yx <- data.frame(x=c(5, 0, -50, -100, -150), y=c(5, 0, -50, -100, -150), z=c(5, 0, -50, -100, -150))


p1 <- ggplot(d, aes(diff_drain, diff_leach)) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  geom_smooth(method='lm', formula=y~x, se=F) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_abline(slope=-1, intercept = 0)
p1

p2 <- ggplot(yx, aes(x, y)) + 
  geom_ribbon(aes(xmin=predict(loess(y~z)), xmax=predict(loess(y~z)), ymin=predict(loess(y~z)), ymax=0),
              fill='darkgreen', alpha=.5) + 
  geom_ribbon(aes(xmin=-predict(loess(y~z)), xmax=-predict(loess(y~z)), ymin=-predict(loess(y~z)), ymax=0),
              fill='darkgreen', alpha=.5)

p2 + geom_point(data=d, aes(diff_drain, diff_leach)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  geom_smooth(data=d, aes(diff_drain, diff_leach), method='lm', formula=y~x, se=F) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_abline(slope=-1, intercept = 0) + 
  scale_x_continuous(limits=c(-50, 5))  + 
  scale_y_continuous(limits=c(-50, 6)) + 
  theme_bw()
