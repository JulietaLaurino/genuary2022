library(ggplot2)

# Create datapoints
sinusoidalPoints <- function(x1, x2, funct) {
  x = x1:x2
  y = funct(x1:x2)
  
  sinusoidal = data.frame(x, y)
  
  return(sinusoidal)
}

wave1 = sinusoidalPoints(1, 50, cos)
wave2 = sinusoidalPoints(1, 50, sin)

# Plot
color_palette = c('#D9303E', '#F2A71B', '#D96F32', '#402E23', '#B8D9CE', '#305E8C')

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

ggplot(wave1, aes(x, y, color = y)) +
  geom_curve(
    xend = 24,
    yend = 0,
    linetype = 6,
    size = 6,
    curvature = 1
  ) +
  geom_curve(
    data = wave2,
    aes(x, y, color = y),
    xend = 25,
    yend = 0,
    linetype = 6,
    size = 3,
    curvature = 1
  ) +
  scale_color_gradientn(colors = color_palette)+
  theme_art

ggsave('./Day04/fidenza.png', height = 5, width = 5)
