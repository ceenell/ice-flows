
# prelims -----------------------------------------------------------------

library(tidyverse)
library(particles)
library(ambient)
library(tidygraph)
library(ggforce)

theme_set(theme_void())
my_col <- 'royalblue'

source("src/grid_utils.R")

set.seed(34)

# set grid ----------------------------------------------------------------

rows<- 20
cols <- 20
f_zig <- (sin(y) * cos(pi*x))

grid <- crossing(x = 1:rows,y = 1:cols) %>%
  mutate(angle = (sin(y/x) * cos(pi*y^3)))

# convert to matrix
grid_mat <- matrix(data = grid$angle, nrow = rows, ncol = cols)

# plot it
grid_field <- grid %>%
  mutate(xend = x + cos(angle) * 0.5,
         yend = y + sin(angle) * 0.5) %>%
  ggplot() +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(5, 'pt')),
               alpha = 0.25) +
  coord_equal()
grid_field

# drop particles ----------------------------------------------------------

pts <- tibble(
  x = c(5, 5),
  y = c(2, 15))

grid_field +
  geom_point(data = pts, aes(x = x, y = y), color = my_col, size = 3)

# find nearest pt and angle
next_angle <- pts %>%
  pmap_dbl(get_angle, flow_grid = grid_mat)

# move that way a given dist
move_dist <- rnorm(nrow(pts))
move_dist <- 1
pts_next <- pts %>%
  mutate(angle = next_angle) %>%
  mutate(xend = x + cos(angle) * move_dist,
         yend = y + sin(angle) * move_dist)

# plot it
grid_field+
  geom_segment(data = pts_next, aes(x = x, y = y, xend = xend, yend = yend), color = my_col) +
  geom_point(data = pts_next, aes(x = x, y = y), color = my_col, size = 3) +
  geom_point(data = pts_next, aes(x = xend, y = yend), color = "pink", size = 3)

# repeat - give it yend xend

next_move <- pts_next %>%
  select(x = xend, y = yend) %>%
  pmap_dbl(get_angle, grid_mat)

pts_move <- pts_next %>%
  select(x = xend, y = yend) %>%
  mutate(angle = next_move) %>%
  mutate(xend = x + cos(angle) * move_dist,
         yend = y + sin(angle) * move_dist) %>%
  bind_rows(pts_next)
str(pts_move)

grid_field+
  geom_segment(data = pts_move, aes(x = x, y = y, xend = xend, yend = yend), color = my_col) +
  geom_point(data = pts_move, aes(x = x, y = y), color = my_col, size = 3) +
  geom_point(data = pts_move, aes(x = xend, y = yend), color = "pink", size = 3)

## TODO - turn that into a function that takes a number of iterations


# generate flow field -----------------------------------------------------

library(scico)

# use perlin noise to generate semi-randomness
flow_field_width <- 200
resolution_factor <- 0.0015 # smoothness of output flow field - interacts with width
perlin_scale_factor <- 0.01 # how large perlin noise seq. larger = longer
perlin_seed <- 764
perlin_freq <- 0.51 #granularity

grids <- generate_flow_field(flow_field_width = flow_field_width,
                    resolution_factor = resolution_factor,
                    perlin_scale_factor = perlin_scale_factor,
                    perlin_seed,
                    perlin_freq)
# plot
flow_grid <- grids$flow_field_tidy %>%
ggplot() +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = angle),
               arrow = arrow(length = unit(5, 'pt')),
               alpha = 0.8) +
  coord_equal()+
  theme(legend.position="none")+
  scale_color_scico(palette="romaO")
flow_grid

# start particles
n_out <- 1000
num_steps <- 60
step_length <- 1.2
flow_field <- grids$flow_field

grid_part <- start_particles(n_out,
                flow_field_width,
                num_steps,
                step_length,
                flow_field,
                resolution_factor)

grid_part_2 <- start_particles(n_out = n_out*3,
                             flow_field_width,
                             num_steps = num_steps/2,
                             step_length = 1,
                             flow_field,
                             resolution_factor)

grid_part_3 <- start_particles(n_out = round(n_out*2,0),
                               flow_field_width,
                               num_steps = num_steps*1.5,
                               step_length = 1,
                               flow_field,
                               resolution_factor)

grid_part_4 <- start_particles(n_out = round(n_out/2,0),
                               flow_field_width,
                               num_steps = num_steps+(0.5*num_steps),
                               step_length = 2,
                               flow_field,
                               resolution_factor)

ice_flows<-grid_part %>%
  filter(plot_order < 60) %>%
  ggplot() +
  geom_path(aes(x, y,
                group = row_num,
                alpha = 1/plot_order,
                size = 1/plot_order
                ),
           # size = 0.75,
            color = "orchid"
            )+
  geom_path(data=grid_part_2,
            aes(x, y,
                group = row_num,
                alpha=1/plot_order,
                size = 1/plot_order),
            #size = 1,
            color = "cyan"
  )+
  geom_path(data=grid_part_3,
            aes(x, y,
                group = row_num,
                alpha=1/plot_order,
                size = 1/plot_order),
            #size = 1,
            color = "yellow"
  )+
  geom_path(data=grid_part_4,
            aes(x, y,
                group = row_num,
                alpha=1/plot_order,
                size = 1/plot_order),
            #size = 1,
            color = "dodgerblue"
  )+
  theme(legend.position="none",
        plot.background = element_rect(fill = "darkslategrey")) +
  coord_equal(xlim = c(0, flow_field_width),
              ylim = c(0, flow_field_width))+
  scale_size(range=c(0, 1), trans="log1p")

ice_flows


ggplot2::ggsave(
  sprintf("out/ice_flows_%s.png", str_replace_all(Sys.time(), "[[:punct:]] ", '')),
  units     = "px",
  width     = 3000,
  height    = 3000
)


# make a gif --------------------------------------------------------------



library(gganimate)

grid_part %>%
  ggplot() +
  geom_path(aes(x, y,
                group = row_num,
                alpha = 1/plot_order
  ),
  # alpha = 0.7,
  size = 0.75,
  color = "orangered"
  )+
  geom_path(data=grid_part_2,
            aes(x, y,
                group = row_num,
                alpha=1/plot_order),
            size = 1,
            color = "orange"
  )+
  geom_path(data=grid_part_3,
            aes(x, y,
                group = row_num,
                alpha=2/plot_order),
            size = 1,
            color = "tomato"
  )+
  geom_path(data=grid_part_4,
            aes(x, y,
                group = row_num,
                alpha=1/plot_order),
            size = 1,
            color = "thistle2"
  )+
  theme(legend.position="none",
        plot.background = element_rect(fill = "darkslategrey")) +
  coord_equal(xlim = c(0, flow_field_width),
              ylim = c(0, flow_field_width))
