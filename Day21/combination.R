# Packing function based on Ijeamaka A's code https://github.com/Ijeamakaanyene/circle_packing

library(tidyverse)
library(ggplot2)
library(purrr)


distance_formula = function(x2, x1, y2, y1){
  dist = sqrt((x2 - x1)^2 + (y2 - y1)^2)
  
  return(dist)
}

pack_circles = function(config, 
                        panel_size, 
                        max_attempts){
  
  # Some nice reminders for inputs
  if(is.data.frame(config) == FALSE){
    usethis::ui_stop("config must be a dataframe")
  }
  if(is.list(panel_size) == FALSE){
    usethis::ui_stop("panel_size must be a list")
  }
  if(length(panel_size$x) == 0 | length(panel_size$y) == 0){
    usethis::ui_stop("panel_size must contain vectors x and y")
  }
  if(is.numeric(config$radius) == FALSE){
    usethis::ui_stop("config file must include a radius column")
  }
  
  
  # count number of circles and sort by decreasing radius
  num_circles = config %>%
    count(radius) %>%
    arrange(-radius)
  
  # set up 
  xt = vector()
  yt = vector()
  rt = vector()
  
  # first placement
  rt[1] = num_circles$radius[1]
  xt[1] = runif(1, min = rt + panel_size$x[1], max = panel_size$x[2] - rt)
  yt[1] = runif(1, min = rt + panel_size$y[1], max = panel_size$y[2] - rt)
  
  
  for(i in 1:nrow(num_circles)){
    
    for(j in 1:num_circles$n[i]){
      
      # overlapping until proven not overlapping
      overlap = 1
      attempts = 0
      
      while(overlap == 1){
        
        #print(attempts)
        
        # place new points
        r_temp = num_circles$radius[i]
        x_temp = runif(1, min = r_temp + panel_size$x[1], max = panel_size$x[2] - r_temp)
        y_temp = runif(1, min = r_temp + panel_size$y[1], max = panel_size$y[2] - r_temp)
        
        
        # get length of already saved points
        df_length = length(xt)
        
        # check for collisions - distance must be greater than sum of radii
        collision_df = map_dfr(1:df_length,
                               ~bind_cols(
                                 dist = distance_formula(xt[.x], x_temp,
                                                         yt[.x], y_temp),
                                 radii_sum = (rt[.x] + r_temp) 
                               ))
        
        check_collision_val = collision_df %>%
          filter(dist > radii_sum) %>%
          nrow()
        
        if(check_collision_val == df_length){
          
          xt = c(xt, x_temp)
          yt = c(yt, y_temp)
          rt = c(rt, r_temp)
          overlap = 0
          
        } else {
          
          if(attempts > max_attempts){
            overlap = 0
            
          } else {
            overlap = 1
            attempts = attempts + 1
            
          }
        }
        
      }
    }
  }
  
  return(tibble(x = xt, y = yt, r = rt))
  
}

#----

panel_size = list(x = c(0, 8),
                  y = c(0, 5))

config = tibble(radius = c(rep(0.1, 1000), 
                           rep(0.25, 200),
                           rep(0.5, 15)))

set.seed(15)
circle_coords = pack_circles(config,
                             panel_size,
                             max_attempts = 25)

# Generate background df
createDots <- function(seed, n, xmin, xmax, ymin, ymax) {
  set.seed(seed)
  x = c(runif(n, xmin, xmax))
  y = c(runif(n, ymin, ymax))
  df = data.frame(x, y)
  return(df)
}

background_dots = createDots(1, 1000, panel_size$x[1], panel_size$x[2],
                             panel_size$y[1], panel_size$y[2])

# Convert to noisy circle
noisyCircle <- function(xpos, ypos, r, noise = 0.09) {
  T <- seq(0, 2*pi, length.out = 100)
  xnoise = runif(100, min = -noise*r, max = noise*r)
  ynoise = runif(100, min = -noise*r, max = noise*r)
  x = cos(T) * r + xpos + xnoise
  y = sin(T) * r + ypos + ynoise
  
  df = data.frame(x, y)
  
  return(df)
}

# Generate spiral df
noisy_circle_coords = data.frame(x = double(),
                           y = double(),
                           n = double(),
                           r = double())
for (i in 1:nrow(circle_coords)) {
  this_circle = noisyCircle(circle_coords$x[i], 
                             circle_coords$y[i],
                             circle_coords$r[i])
  this_circle$n = i
  this_circle$r = circle_coords$r[i]
  noisy_circle_coords = noisy_circle_coords %>% bind_rows(this_circle)
}


# Plot

pal = c('#5A70E8', '#8AE843', '#E89072')

theme_art <- theme(
  legend.position = 'none',
  #aspect.ratio = 1,
  panel.background = element_rect(fill = '#F2EADF'),
  panel.grid = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  panel.border = element_rect(colour = '#F2EADF', fill=NA, size=12)
) 


library(ggforce)
ggplot(data = noisy_circle_coords) +
  geom_point(
    data = background_dots,
    aes(x, y),
    color = 'white',
    alpha = 0.8,
    size = 0.05
  ) +
  geom_polygon(aes(
    x,
    y,
    group = as.factor(n),
    fill = as.factor(r)
  )) +
  scale_fill_manual(values = pal) +
  coord_equal() +
  theme_art 

ggsave('./Day21/combination.png')


