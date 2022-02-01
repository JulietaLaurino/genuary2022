library(tidyverse)
library(magick)
library("wesanderson")

# Create cube data

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
depth = 1
angle = 45

point_id = cubePoints(width, height, pos_x, pos_y, depth, angle) %>% 
  mutate(point_id = row_number())

cube_f1_front = point_id %>% 
  filter(point_id != 5) %>% 
  slice(rep(1:n(), each = 3))%>% 
  mutate(point2_id = c(2,3, NA, 1,4,6,1,4,7,3,2,8,NA,8,2,8,3,NA, 4,7,6)) %>% 
  mutate(id = row_number()) %>% 
  select(-c(x,y)) %>% 
  pivot_longer(names_to = 'group_id', values_to = 'point_id', cols = c(point_id, point2_id)) %>% 
  left_join(point_id) %>% 
  select(id, x, y)

cube_f1_back = point_id %>% 
  filter(point_id == 5) %>% 
  slice(rep(1:n(), each = 3)) %>% 
  mutate(point2_id = c(6,7,1)) %>% 
  mutate(id = row_number()) %>% 
  select(-c(x,y)) %>% 
  pivot_longer(names_to = 'group_id', values_to = 'point_id', cols = c(point_id, point2_id)) %>% 
  left_join(point_id) %>% 
  select(id, x, y)

cube_f2 = point_id %>% 
  slice(rep(1:n(), each = 3)) %>% 
  mutate(point2_id = c(2,3,5,1,1,6,1,4,7,3,3,8,6,7,1,5,8,2,8,3,5,4,7,6)) %>% 
  mutate(id = row_number()) %>% 
  select(-c(x,y)) %>% 
  pivot_longer(names_to = 'group_id', values_to = 'point_id', cols = c(point_id, point2_id)) %>% 
  left_join(point_id) %>% 
  select(id, x, y)

# Create points data
set.seed(3)

x = c(runif(1000, min = 1.1, max = 3.7))
y = c(runif(1000, min = 1.1, max = 3.4))
c = c(runif(1000, min = 1.1, max = 3.5))

dots = data.frame(x, y, c)
slope = cos(angle) / (sin(angle) + pos_x)
dots = dots[dots$y < dots$x *(slope) + (pos_y + height*0.9),]
dots = dots[dots$y > dots$x *(slope) +0.4,]

x = c(runif(1000, min = 0, max = 5))
y = c(runif(1000, min = 0, max = 5))
c = c(runif(1000, min = 0, max = 5))

dots_out = data.frame(x, y, c)

# Plot
color_palette = c( '#F2A71B', '#D96F32', '#305E8C')

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


ggplot(cube_f1_back, aes(x, y, group = id)) +
  geom_polygon(size = 3,
               color = 'black') +
  geom_point(
    data = dots,
    aes(x, y, color = c),
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_polygon(data = cube_f1_front,
               aes(x, y, group = id),
               size = 3,
               color = 'black') +
  scale_color_gradientn(colors = wes_palette("Moonrise3", type = 'continuous')) +
  theme_art +
  scale_x_continuous(limits = c(0, 4.7)) +
  scale_y_continuous(limits = c(0, 4.7))

ggsave('./Day05/img/cube1.png',
       width = 5,
       height = 5)

ggplot(cube_f2, aes(x, y, color = id, group = id)) +
  geom_curve(data = data.frame(x = c(3, 3), y = c(1, 3)),
             aes(x, y),
             xend = c(2.2, 2.2),
             yend = c(1.8, 2.2),
             size = 3,
             inherit.aes = FALSE,
             curvature = -0.2
  ) +
  geom_polygon(
    size = 3,
    color = 'black'
  )+
  geom_point(
    data = dots_out,
    aes(x, y, color = c),
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  scale_color_gradientn(colors = wes_palette("Moonrise3", type = 'continuous'))+
  theme_art+
  scale_x_continuous(limits = c(0, 4.7))+
  scale_y_continuous(limits = c(0, 4.7))

ggsave('./Day05/img/cube2.png',
       width = 5,
       height = 5)

## list file names and read in
imgs <- list.files('./Day05/img', full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 1)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "./Day05/broken_cube.gif")



