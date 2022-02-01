library(tidyverse)
library(geomtextpath)

sinusoidalPoints <- function(x1, x2, ypos, n, funct, amp) {
  x = seq(x1, x2, length.out = n)
  y = funct(seq(x1, x2, length.out = n)) * amp + ypos
  
  sinusoidal = data.frame(x, y)
  
  return(sinusoidal)
}

# Generate waves df
data_sin = data.frame(x = double(),
                      y = double())
for (ypos in 1:20) {
  this_data = sinusoidalPoints(-1, 10, ypos, 100, sin, 8)
  this_data$g = ypos
  data_sin = data_sin %>% bind_rows(this_data)
}

data_cos = data.frame(x = double(),
                      y = double())
for (ypos in 1:20) {
  this_data = sinusoidalPoints(0, 11, ypos, 100, cos, 8)
  this_data$x = this_data$x - 1
  this_data$g = ypos
  data_cos = data_cos %>% bind_rows(this_data)
}

# Generate background df
panel_size = list(x = c(-2, 11), y = c(-11, 35))
createDots <- function(seed, n, xmin, xmax, ymin, ymax) {
  set.seed(seed)
  x = c(runif(n, xmin, xmax))
  y = c(runif(n, ymin, ymax))
  df = data.frame(x, y)
  return(df)
}

background_dots = createDots(1,
                             100000,
                             panel_size$x[1],
                             panel_size$x[2],
                             panel_size$y[1],
                             panel_size$y[2])

# Plot
#pal = colorRampPalette(c('#2E718C','#45A9D1', '#A60852', '#731717', '#F23838'))
pal = c(rep(c('#2E718C','#45A9D1', '#A60852', '#731717', '#F23838'), each = 4))

theme_art <- theme(
  legend.position = 'none',
  #aspect.ratio = 1,
  panel.background = element_rect(fill = '#D9BD30'),
  panel.grid = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  strip.text.x = element_blank(),
  panel.border = element_blank()
)

ggplot(background_dots, aes(x,y)) +
  geom_point(
    color = 'white',
    alpha = 0.04,
    size = 0.05
  ) +
  geom_point(
    data_sin,
    mapping = aes(x, y, color = as.factor(g)),
    size = 2,
    shape = c(rep(c(15, 16, 17, 18, 20), 400))
  ) +
  geom_point(
    data_cos,
    mapping = aes(x, y, group = g, color = as.factor(g)),
    size = 2,
    shape = c(rep(c(15, 16, 17, 18, 20), 400)),
  ) +
  scale_color_manual(values = pal) +
  theme_art

ggsave('./Day20/shapes.png')
