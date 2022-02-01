library(ggplot2)

sinusoidalPoints <- function(x1, x2, n, funct) {
  x = seq(x1, x2, length.out = n) 
  y = funct(seq(x1, x2, length.out = n)) 
  
  sinusoidal = data.frame(x, y)
  
  return(sinusoidal)
}

wave = sinusoidalPoints(1, 1+100*pi, 1000, cos)


# Plot
theme_art <- theme(
  legend.position = 'none',
  aspect.ratio = 1,
  panel.background = element_rect(fill = '#F2EADF'),
  panel.grid = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  panel.border = element_rect(colour = '#F2EADF', fill=NA, size=10)
)

ggplot(wave, aes(x,y)) +
  geom_path(size = 1, aes(alpha = y)) +
  coord_polar() +
  theme_art 

ggsave('./Day08/single_curve.png', width = 5, height = 5)
