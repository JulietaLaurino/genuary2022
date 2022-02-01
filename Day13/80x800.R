library(ggplot2)
library(scales)

noisyCircle <- function(xpos, ypos, r, noise = 0.08) {
  T <- seq(0, 2*pi, length.out = 100)
  xnoise = runif(100, min = -noise*r, max = noise*r)
  ynoise = runif(100, min = -noise*r, max = noise*r)
  x = cos(T) * r + xpos + xnoise
  y = sin(T) * r + ypos + ynoise

  df = data.frame(x, y)

  return(df)
}


panel_size = list(x = c(0, 8), y = c(0, .8))

noisy_circle = data.frame(x = double(),
                          y = double(),
                          g = factor())
for (pos in seq(1,8,0.6)) {
  this_noisy_circle = noisyCircle(pos, 1, 1)
  this_noisy_circle$x = rescale(this_noisy_circle$x, to = c(pos-1, pos))
  this_noisy_circle$y = rescale(this_noisy_circle$y, to = c(0, .8))
  this_noisy_circle$g = as.factor(pos)
  noisy_circle = noisy_circle %>% bind_rows(this_noisy_circle) 
}

# Plot
pal = c(
  '#A60321',
  '#F25270',
  '#011640',
  '#F2CF63',
  '#A7DBE9',
  '#F9C3B7',
  '#ED3C56',
  '#A60321',
  '#F25270',
  '#011640',
  '#F2CF63',
  '#A7DBE9',
  '#F9C3B7',
  '#ED3C56'
) 

theme_art <- theme(
  legend.position = 'none',
  #aspect.ratio = 1,
  panel.background = element_rect(fill = '#F2EADF'),
  panel.grid = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  strip.text.x = element_blank(),
  panel.border = element_blank()
)

ggplot(noisy_circle, aes(x,y, group = g)) + 
  geom_polygon(aes(fill = g), alpha = 0.8) +
  xlim(panel_size$x) +
  ylim(panel_size$y)+
  coord_equal() +
  theme_art +
  scale_fill_manual(values = pal) 

ggsave('./Day13/noisy_circles.png', width = 800, height = 80, units = 'mm')
