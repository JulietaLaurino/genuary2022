library(ggplot2)

squarePoints <- function(width, height, pos_x, pos_y) {
  x_min = c(pos_x)
  x_max = c(pos_x + width)
  y_min = c(pos_y)
  y_max = c(pos_y + height)
  return(data.frame(x_min, x_max, y_min, y_max))
}

ribbon_a = squarePoints(1,5, 0.5, 0)
ribbon_b = squarePoints(1,5, 3.5, 0)
ribbons_1 = bind_rows(ribbon_a, ribbon_b)

ribbon_c = squarePoints(5,1, 0, 0.5)
ribbon_d = squarePoints(5,1, 0, 3.5)
ribbons_2 = bind_rows(ribbon_c, ribbon_d)

ribbons_3 = squarePoints(1,5, 2, 0)

ribbon_e = squarePoints(0.5, 1, 0, 2)
ribbon_f = squarePoints(2, 1, 1.5, 2)
ribbon_g = squarePoints(0.5, 1, 4.5, 2)
ribbons_4 = bind_rows(ribbon_e, ribbon_f, ribbon_g)

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
  aspect.ratio = 1,
  panel.background = element_rect(fill = '#F2EADF'),
  panel.grid = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  panel.border = element_rect(colour = '#F2EADF', fill=NA, size=12)
) 

ggplot() +
  geom_rect(data = ribbons_1, 
            aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
            fill = '#F25270') +
  geom_rect(data = ribbons_2, 
            aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
            fill = '#F2CF63') +
  geom_rect(data = ribbons_3, 
            aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
            fill = '#F25270') +
  geom_rect(data = ribbons_4, 
            aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
            fill = '#A7DBE9') +
  coord_equal() +
  theme_art

ggsave('./Day17/3colors.png', width = 5, height = 5)

