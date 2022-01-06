# Find closest grid item to current position
get_angle <- function(x, y, flow_grid) {
  max_x <- ncol(flow_grid)
  max_y <- nrow(flow_grid)

  closest_x_index <- which(abs(1:max_x - x) == min(abs(1:max_x - x)))[[1]] # Pick the first one
  closest_y_index <- which(abs(1:max_y - y) == min(abs(1:max_y - y)))[[1]]
  closest_angle <- flow_grid[[closest_y_index, closest_x_index]]

  return(closest_angle)
}
generate_flow_field <- function(flow_field_width = 1000,
                                resolution_factor = 0.025,
                                perlin_scale_factor = 0.005,
                                perlin_seed,
                                perlin_freq) {

  resolution <- flow_field_width * resolution_factor
  num_cols <- flow_field_width %/% resolution
  num_rows <- num_cols

  long_grid_ff <- ambient::long_grid(x = 1:num_cols,
                                     y = 1:num_rows) %>%
    mutate(x = x * perlin_scale_factor,
           y = y * perlin_scale_factor) %>%
    mutate(angle = ambient::gen_perlin(x,
                                       y,
                                       seed = perlin_seed,
                                       frequency = perlin_freq))

  # normalize angles to be between 0 & 2pi
  min_per <- min(long_grid_ff$angle)
  max_per <- max(long_grid_ff$angle)

  long_grid_ff <- long_grid_ff %>%
    mutate(angle = (angle-min_per) / (max_per-min_per) *  (2*pi-0) + 0)

  my_flow_field <- matrix(data = long_grid_ff$angle,
                          ncol = num_cols,
                          nrow = num_rows)

  visualized_flow_field <- crossing(
    x = 1:num_cols,
    y = 1:num_rows
  ) %>%
    mutate(angle = map2_dbl(x, y, ~my_flow_field[[.y, .x]])) %>%
    mutate(xend = x + cos(angle) * 0.5,
           yend = y + sin(angle) * 0.5) %>%
    mutate(x_index = x, y_index = y) %>%
    mutate(across(c(x, y, xend, yend), ~ .x * resolution))

  list('flow_field' = my_flow_field, 'flow_field_tidy' = visualized_flow_field)
}
start_particles <- function(n_out = 800,
                            flow_field_width,
                            num_steps,
                            step_length,
                            flow_field,
                            resolution_factor)
{
  # random starting points
  df <- tibble::tibble(
    start_x = runif(flow_field_width*-0.1, flow_field_width*1.1, n=n_out),
    start_y = runif(flow_field_width*-0.1, flow_field_width*1.1, n=n_out)
  ) %>%
    mutate(row_num = row_number(), #remember each start
           resolution = flow_field_width * resolution_factor,
           num_steps = num_steps,
           step_length = step_length)

  # USE START_X AND START_Y and move num_steps
  # # draw curve
  df %>%
    pmap_dfr(draw_curve,
             flow_field = flow_field)
}
draw_curve <- function(start_x,
                       start_y,
                       row_num,
                       flow_field,
                       resolution,
                       left_x = 1 * resolution,
                       bot_y  = 1 * resolution,
                       num_steps,
                       step_length)
{

  x_container <- vector("numeric", num_steps+1)
  y_container <- vector("numeric", num_steps+1)

  x_container[1] <- start_x
  y_container[1] <- start_y


  # grid dimension range
  x_dim_range <- 1:ncol(flow_field)
  y_dim_range <- 1:nrow(flow_field)

  # With the rest of num_steps, move through the flow field,
  # Each time, stepping towards the closest angle we can grab.
  for (i in 1:num_steps) {

    next_step <- step_into_next_curve_segment(
      start_x     = x_container[i],
      start_y     = y_container[i],
      left_x      = left_x,
      bot_y       = bot_y,
      resolution  = resolution,
      x_dim_range = x_dim_range,
      y_dim_range = y_dim_range,
      flow_field  = flow_field,
      step_length = step_length
    )

    x_container[i+1] <- next_step$x
    y_container[i+1] <- next_step$y

  }

  tibble::tibble(
    x = x_container,
    y = y_container,
    row_num = row_num
  ) %>%
    dplyr::mutate(plot_order = dplyr::row_number())

}
step_into_next_curve_segment <- function(start_x,
                                         start_y,
                                         left_x,
                                         bot_y,
                                         resolution,
                                         x_dim_range,
                                         y_dim_range,
                                         flow_field,
                                         step_length)
{
  # Get the current x/y position (in relation to grid size)
  x_offset <- start_x - left_x
  y_offset <- start_y - bot_y

  # Scale it down, to match grid dimension
  curr_x_index <- round(x_offset / resolution, digits = 0)
  curr_y_index <- round(y_offset / resolution, digits = 0)

  # Find the closest point on the grid at each x/y
  closest_x_index <- which(abs(x_dim_range - curr_x_index) == min(abs(x_dim_range - curr_x_index)))[[1]]
  closest_y_index <- which(abs(y_dim_range - curr_y_index) == min(abs(y_dim_range - curr_y_index)))[[1]]

  # Grab that angle
  closest_angle <- flow_field[[closest_y_index, closest_x_index]]

  # Extend the current line into that angle (scale it up again)
  x_step  <- step_length * cos(closest_angle) * resolution
  y_step  <- step_length * sin(closest_angle) * resolution
  res_x <- start_x + x_step
  res_y <- start_y + y_step

  list(x = res_x, y = res_y)

}
