library(ggplot2)
library("wesanderson")

# Generate data
set.seed(1)

x = c(runif(10000))
y = c(runif(10000))
c = c(runif(10000))

dots = data.frame(x, y, c)

# Plot
theme_art <- theme(
  legend.position = 'none',
  aspect.ratio = 1,
  panel.background = element_rect(fill = 'white'),
  panel.grid = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  panel.border = element_rect(colour = 'white', fill=NA, size=10)
)

ggplot(dots, aes(x, y, color = c)) +
  geom_point(alpha = 0.5, size = 3) +
  scale_color_gradientn(colors = wes_palette("Moonrise3", type = 'continuous')) +
  theme_art

ggsave('./Day01/thousand_points.png', width = 5, height = 5)

