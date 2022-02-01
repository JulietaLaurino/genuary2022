library(ggplot2)


squarePoints <- function(width, height, pos_x, pos_y) {
  x_min = c(pos_x)
  x_max = c(pos_x + width)
  y_min = c(pos_y)
  y_max = c(pos_y + height)
  return(data.frame(x_min, x_max, y_min, y_max))
}

# Front view----
# First layer
roof = squarePoints(4, 0.1, 1, 0.8)
roof2 = squarePoints(4, 0.05, 1, 0.9)
floor = squarePoints(4, 0.1, 1, 0.25)
porch = squarePoints(3, 0.1, 0, 0.1)

layer1_f = bind_rows(
  'roof' = roof,
  'roof2' = roof2,
  'floor' = floor,
  'porch' = porch,
  .id = 'id'
)


# Second layer
window1 = squarePoints(0.35, 0.45, 2.1, 0.35)
window2 = squarePoints(0.55, 0.45, 2.45, 0.35)
window3 = squarePoints(0.55, 0.45, 3, 0.35)
window4 = squarePoints(0.55, 0.45, 3.55, 0.35)
window5 = squarePoints(0.55, 0.45, 4.1, 0.35)
window6 = squarePoints(0.35, 0.45, 4.65, 0.35)

layer2_f = bind_rows(
  'window1' = window1,
  'window2' = window2,
  'window3' = window3,
  'window4' = window4,
  'window5' = window5,
  'window6' = window6,
  .id = 'id'
)

# Third layer
beam1 = squarePoints(0.05, 0.75, 1.325, 0.2)
beam2 = squarePoints(0.05, 0.75, 2.425, 0.2)
beam3 = squarePoints(0.05, 0.9, 3.525, 0.05)
beam4 = squarePoints(0.05, 0.9, 4.625, 0.05)

layer3_f = bind_rows(
  'beam1' = beam1,
  'beam2' = beam2,
  'beam3' = beam3,
  'beam4' = beam4,
  .id = 'id'
)

# Fourth layer
beam5 = squarePoints(0.05, 0.15, 0.225, 0.05)
beam6 = squarePoints(0.05, 0.15, 1.325, 0.05)
beam7 = squarePoints(0.05, 0.15, 2.425, 0.05)

layer4_f = bind_rows(
  'beam5' = beam5,
  'beam6' = beam6,
  'beam7' = beam7,
  .id = 'id'
)

# Fifth layer

## To porch
step1 = squarePoints(0.4, 0.025, 1.7, 0.075)
step2 = squarePoints(0.4, 0.025, 1.7, 0.1)
step3 = squarePoints(0.4, 0.025, 1.7, 0.125)
step4 = squarePoints(0.4, 0.025, 1.7, 0.150)

## To the house
step5 = squarePoints(0.4, 0.025, 1.7, 0.225)
step6 = squarePoints(0.4, 0.025, 1.7, 0.25)
step7 = squarePoints(0.4, 0.025, 1.7, 0.275)
step8 = squarePoints(0.4, 0.025, 1.7, 0.3)

layer5_f = bind_rows(
  'step1' = step1,
  'step2' = step2,
  'step3' = step3,
  'step4' = step4,
  'step5' = step5,
  'step6' = step6,
  'step7' = step7,
  'step8' = step8,
  .id = 'id'
)

# Back view----
y_shift = 1.5
x_lim = 5
# First layer
roof = squarePoints(-4, 0.1, x_lim - 1, 0.8 - y_shift)
roof2 = squarePoints(-4, 0.05, x_lim - 1, 0.9 - y_shift)
floor = squarePoints(-4, 0.1, x_lim - 1, 0.25 - y_shift)
porch = squarePoints(-3, 0.1, x_lim - 0, 0.1 - y_shift)

layer1_b = bind_rows(
  'roof' = roof,
  'roof2' = roof2,
  'floor' = floor,
  'porch' = porch,
  .id = 'id'
)


# Second layer
window1 = squarePoints(-0.35, 0.45, x_lim - 2.1, 0.35 - y_shift)
window2 = squarePoints(-0.55, 0.45, x_lim - 2.45, 0.35 - y_shift)
window3 = squarePoints(-0.55, 0.45, x_lim - 3, 0.35 - y_shift)
window4 = squarePoints(-0.55, 0.45, x_lim - 3.55, 0.35 - y_shift)
window5 = squarePoints(-0.55, 0.45, x_lim - 4.1, 0.35 - y_shift)
window6 = squarePoints(-0.35, 0.45, x_lim - 4.65, 0.35 - y_shift)

layer2_b = bind_rows(
  'window1' = window1,
  'window2' = window2,
  'window3' = window3,
  'window4' = window4,
  'window5' = window5,
  'window6' = window6,
  .id = 'id'
)

# Third layer
beam1 = squarePoints(-0.05, 0.75, x_lim - 1.325, 0.2 - y_shift)
beam2 = squarePoints(-0.05, 0.75, x_lim - 2.425, 0.2 - y_shift)
beam3 = squarePoints(-0.05, 0.9, x_lim - 3.525, 0.05 - y_shift)
beam4 = squarePoints(-0.05, 0.9, x_lim - 4.625, 0.05 - y_shift)

layer3_b = bind_rows(
  'beam1' = beam1,
  'beam2' = beam2,
  'beam3' = beam3,
  'beam4' = beam4,
  .id = 'id'
)

# Fourth layer
beam5 = squarePoints(-0.05, 0.15, x_lim - 0.225, 0.05 - y_shift)
beam6 = squarePoints(-0.05, 0.15, x_lim - 1.325, 0.05 - y_shift)
beam7 = squarePoints(-0.05, 0.15, x_lim - 2.425, 0.05 - y_shift)

layer4_b = bind_rows(
  'beam5' = beam5,
  'beam6' = beam6,
  'beam7' = beam7,
  .id = 'id'
)

# Fifth layer

## To porch
step1 = squarePoints(-0.4, 0.025, x_lim - 1.7, 0.075 - y_shift)
step2 = squarePoints(-0.4, 0.025, x_lim - 1.7, 0.1 - y_shift)
step3 = squarePoints(-0.4, 0.025, x_lim - 1.7, 0.125 - y_shift)
step4 = squarePoints(-0.4, 0.025, x_lim - 1.7, 0.150 - y_shift)

## To the house
step5 = squarePoints(-0.4, 0.025, x_lim - 1.7, 0.225 - y_shift)
step6 = squarePoints(-0.4, 0.025, x_lim - 1.7, 0.25 - y_shift)
step7 = squarePoints(-0.4, 0.025, x_lim - 1.7, 0.275 - y_shift)
step8 = squarePoints(-0.4, 0.025, x_lim - 1.7, 0.3 - y_shift)

layer5_b = bind_rows(
  'step1' = step1,
  'step2' = step2,
  'step3' = step3,
  'step4' = step4,
  'step5' = step5,
  'step6' = step6,
  'step7' = step7,
  'step8' = step8,
  .id = 'id'
)

# Plot----

ggplot() +
  geom_line(data = data.frame(x = c(-0.5, 5.5), y = c(0.05)), aes(x,y))+
  geom_rect(data = layer1_f, 
            aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
                size = c(0.25,0.5,0.25,0.25),
            fill = NA,
            color = 'black') +
  geom_rect(data = layer2_f, 
            aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
            fill = NA,
            color = 'black',
            size = 0.5) +
  geom_rect(data = layer3_f, 
            aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
            fill = '#F2EADF',
            color = 'black',
            size = 0.5) +
  geom_rect(data = layer4_f, 
            aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
            fill = '#F2EADF',
            color = 'black',
            size = 0.5) +
  geom_rect(data = layer5_f, 
            aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
            fill = '#F2EADF',
            color = 'black',
            size = 0.5) + 
  geom_rect(data = layer5_b, 
            aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
            fill = '#F2EADF',
            color = 'black',
            size = 0.5) +
  geom_rect(data = layer1_b, 
            aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
            size = c(0.25,0.5,0.25,0.25),
            fill = '#F2EADF',
            color = 'black') +
  geom_rect(data = layer2_b, 
            aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
            fill = NA,
            color = 'black',
            size = 0.5) +
  geom_rect(data = layer3_b, 
            aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
            fill = '#F2EADF',
            color = 'black',
            size = 0.5) +
  geom_rect(data = layer4_b, 
            aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
            fill = '#F2EADF',
            color = 'black',
            size = 0.5) +
  geom_line(data = data.frame(x = c(-0.5, 5.5), y = c(-1.45)), aes(x,y)) +
  ylim(c(-1.75,1.25))+
  annotate(geom="text", x=-0.3, y=0.2, label="NORTE", size = 3.5, family = 'mono')+
  annotate(geom="text", x=-0.35, y=-1.3, label="SUR", size = 3.5, family = 'mono')+
  coord_fixed() +
  theme_void()+
  theme(panel.background = element_rect(fill = '#F2EADF'),  
        panel.border = element_rect(colour = 'white', fill=NA, size=10))


ggsave('./Day09/casa_farnsworth.png')

  
