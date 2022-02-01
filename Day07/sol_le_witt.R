library(ggplot2)

# Generate data
set.seed(1)

createDots <- function(n, q) {
x = c(runif(n))
y = c(runif(n))
q = c(rep(q, n))

return(data.frame(x, y, q))
}

n = 50
q = 16

dots = data.frame()
for (quadrant in 1:q) {
  if (quadrant == 16) {
    for (times in 1:10) {
      dots_q = createDots(n, quadrant)
      dots = dots %>%
        bind_rows(dots_q)
    }
  } else {
    dots_q = createDots(n, quadrant)
    dots = dots %>%
      bind_rows(dots_q)
  }
}


# Plot
theme_art <- theme(
  legend.position = 'none',
  aspect.ratio = 1,
  panel.background = element_rect(fill = '#F2EADF'),
  panel.grid = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  panel.border = element_rect(colour = '#F2EADF', fill=NA, size=5),
  strip.text.x = element_blank()
)

ggplot(dots, aes(x, y)) +
  geom_path(alpha = 0.8, size = 0.08)+
  theme_art +
  facet_wrap(~q)

ggsave('./Day07/sol_le_witt.png', width = 5, height = 5)
