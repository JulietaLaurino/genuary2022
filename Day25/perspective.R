library(tidyverse)
library(gganimate)

squarePoints <- function(width, height, pos_x, pos_y){
  x = c(pos_x, pos_x + width, pos_x, pos_x + width)
  y = c(pos_y, pos_y, pos_y + height, pos_y + height)
  return(data.frame(x, y))
}

cubePoints <- function(width, height, pos_x, pos_y, depth, angle) {
  square1 = squarePoints(width, height, pos_x, pos_y)
  pos_x_2 = pos_x + sin(angle) * depth
  pos_y_2 = pos_x + cos(angle) * depth
  square2 = squarePoints(width, height, pos_x_2, pos_y_2)
  
  cube = square1 %>% bind_rows(square2)
  
  return(cube)
}

width = 2
height = 2
pos_x = 1
pos_y = 1
depth = 0.8

theme_art <- theme(
  legend.position = 'none',
  #aspect.ratio = 1,
  panel.background = element_rect(fill = '#F2EADF'),
  #panel.grid = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  panel.border = element_rect(colour = '#F2EADF', fill=NA, size=12)
) 

cubes = data.frame(id = character(),
                  x = double(),
                  y= double(),
                  angle = double())
for (i in seq(-2*pi,2*pi, length.out = 100)) {
  point_id = cubePoints(width, height, pos_x, pos_y, depth, i) %>%
    mutate(point_id = row_number())
  
  this_cube = point_id %>%
    slice(rep(1:n(), each = 3)) %>%
    mutate(point2_id = c(2, 3, 5,
                         6, 1, 4,
                         1, 4, 7,
                         3, 2, 8,
                         7, 1, 6,
                         8, 5, 2,
                         3, 8, 5,
                         7, 4, 6)) %>%
    mutate(id = paste(row_number(),i, sep = '_')) %>%
    select(-c(x, y)) %>%
    pivot_longer(
      names_to = 'group_id',
      values_to = 'point_id',
      cols = c(point_id, point2_id)
    ) %>%
    left_join(point_id) %>%
    select(id, x, y)
  this_cube$angle = i
  cubes = cubes %>% bind_rows(this_cube)
}

p1 <- ggplot(cubes, aes(x, y)) +
  geom_polygon(aes(group = id, color = angle),
               #color = 'black',
               size = 2) + 
  # Here comes the gganimate code
  transition_states(
    angle
  ) +
  theme_art

animate(p1, fps = 20)

anim_save('./Day25/perspective.gif')
               