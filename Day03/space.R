library(ggplot2)
library(wesanderson)

# Generate background df
createDots <- function(seed, n) {
  set.seed(seed)
  x = c(runif(n))
  y = c(runif(n))
  a = c(runif(n))
  s = c(runif(n))
  df = data.frame(x, y, s)
  return(df)
}

dots = createDots(1, 1000)
background_dots = createDots(1, 10000)
x_center = c(0.5)
y_center = c(0.5)
saturn = data.frame(x_center, y_center)

# Generate lines like asteroids
# createAsteroids <- function(seed, n) {
# lines <- data.frame(
#   x = c(runif(n)),
#   y = c(runif(n)),
#   xend = c(runif(n)),
#   yend = c(runif(n))
# )
# }
# 
# asteroids = createAsteroids(10, 1)


# Plot
library(ggforce)
library(ggnewscale)
theme_art <- theme(
  legend.position = 'none',
  aspect.ratio = 1,
  panel.background = element_rect(fill = 'black'),
  panel.grid = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  panel.border = element_rect(colour = 'white', fill=NA, size=10)
)

ggplot(data = background_dots, aes(x, y)) +
  geom_point(size = 0.1, color = 'gray', alpha = 0.1) +
  geom_point(data = dots, aes(x, y, size = s, color = y, alpha = y)) +
  scale_color_gradientn(colours = wes_palette("Moonrise3", 100, type = "continuous"))+
  scale_size(range = c(0, 0.1)) +
  theme_art +
  # new_scale('size') +
  # geom_link(
  #   data = asteroids,
  #   aes(
  #     x,
  #     y,
  #     xend = xend,
  #     yend = yend,
  #     alpha = stat(index),
  #     size = stat(index),
  #     color = stat(index)
  #   ),
  #   lineend = 'round',
  #   inherit.aes = F,
  #   #color = '#FFFDE7'
  # )  +
  # scale_size(range = c(0, 8)) +
  # scale_color_gradientn(colours = c('white', 'white', 'white', 'orange', 'red')) +
  geom_point(data = saturn, color = '#dfb09e', size = 40, aes(x_center, y_center), pch = 21, stroke = 5, alpha = 0.8) +
  geom_point(data = saturn, color = '#403d49', size = 35, aes(x_center, y_center), pch = 21, stroke = 5, alpha = 0.9) +
  geom_point(data = saturn, color = '#b68487', size = 32, aes(x_center, y_center), pch = 21, stroke = 5) +
  geom_point(data = saturn, color = '#dfb09e', size = 29, aes(x_center, y_center), pch = 21, stroke = 5) +
  geom_point(data = saturn, color = '#b68487', size = 27, aes(x_center, y_center), pch = 21, stroke = 5)+
  geom_point(data = saturn, color = '#e5d7bc', size = 24, aes(x_center, y_center), pch = 21, stroke = 5)+
  geom_point(data = saturn, color = '#403d49', size = 14, aes(x_center, y_center))


ggsave('./Day03/space.png', width = 5, height = 5, dpi = 1000)
