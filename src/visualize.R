library(readr)
library(ggplot2)
library(ggstar)
library(pubtheme)

# Load model data
model_data <- read.csv("data/model_data.csv")

background_color <- "#ffffff"
gradient_low_color <- "#bedceb"
gradient_high_color <- "#004b71"

model_data$x <- model_data$x_coord
model_data$y <- model_data$y_coord

model_data_hexbin <- hexbin::hexbin(model_data$x, model_data$y, xbins = 21, IDs = TRUE)
model_data_hexbin_df <- data.frame(hexbin::hcell2xy(model_data_hexbin),
                                   cell = model_data_hexbin@cell,
                                   count = model_data_hexbin@count)
model_data$cell <- model_data_hexbin@cID

# Expected goals plot
xg_plot_fill_limits <- c(0, 0.4)
xg_plot_fill_breaks <- c(0, 0.2, 0.4)
xg_plot_fill_labels <- c("0", "0.2", "0.4")

model_plot_data <- model_data %>%
  group_by(cell) %>%
  summarise(shot_count = n(),
            cell_avg_xg = mean(xg)) %>%
  ungroup() %>%
  right_join(model_data_hexbin_df, by = "cell") %>%
  select(cell, x, y, count, shot_count, cell_avg_xg)

xg_plot <- rink +
  geom_star(data = model_plot_data,
            aes(x = x, y = y,
                fill = cell_avg_xg,
                size = shot_count),
            color = NA,
            starshape = "hexagon",
            show.legend = TRUE) +
  scale_fill_gradient(low = gradient_low_color,
                      high = gradient_high_color,
                      na.value = NA,
                      limits = xg_plot_fill_limits,
                      breaks = xg_plot_fill_breaks,
                      labels = xg_plot_fill_labels) +
  labs(title = "Expected goals (xG)",
       subtitle = "Goal probability for shots on goal",
       caption = "",
       fill = "xG",
       size = "Shots on goal") +
  ylim(c(-100.1, -24)) +
  xlim(c(-42.6, 42.6)) +
  theme_pub(type = "map", base_size = 36/3) +
  theme(plot.background = element_rect(fill = background_color),
        panel.background = element_rect(fill = background_color),
        legend.background = element_rect(fill = background_color),
        legend.position = "top",
        legend.box = "horizontal",
        legend.box.just = "left",
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        legend.direction = "horizontal",
        legend.justification = c(0, 0),
        legend.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = "pt")) +
  guides(size = guide_legend(order = 1,
                             nrow = 1,
                             title.position = "top",
                             title.hjust = 0,
                             override.aes = list(fill = pubdarkgray)), 
         fill = guide_colorbar(order = 2,
                               title.position = "top",
                               title.hjust = 0))

# Expected rebounds plot
xrebounds_plot_fill_limits <- c(0, 0.2)
xrebounds_plot_fill_breaks <- c(0, 0.1, 0.2)
xrebounds_plot_fill_labels <- c("0", "0.1", "0.2")

model_plot_data <- model_data %>%
  group_by(cell) %>%
  summarise(shot_count = n(),
            cell_avg_xrebounds = mean(xrebounds)) %>%
  ungroup() %>%
  right_join(model_data_hexbin_df, by = "cell") %>%
  select(cell, x, y, count, shot_count, cell_avg_xrebounds)

xrebounds_plot <- rink +
  geom_star(data = model_plot_data,
            aes(x = x, y = y,
                fill = cell_avg_xrebounds,
                size = shot_count),
            color = NA,
            starshape = "hexagon",
            show.legend = TRUE) +
  scale_fill_gradient(low = gradient_low_color,
                      high = gradient_high_color,
                      na.value = NA,
                      limits = xrebounds_plot_fill_limits,
                      breaks = xrebounds_plot_fill_breaks,
                      labels = xrebounds_plot_fill_labels) +
  labs(title = "Expected rebounds (xRebounds)",
       subtitle = "Rebound probability for shots on goal",
       caption = "",
       fill = "xRebounds",
       size = "Shots on goal") +
  ylim(c(-100.1, -24)) +
  xlim(c(-42.6, 42.6)) +
  theme_pub(type = "map", base_size = 36/3) +
  theme(plot.background = element_rect(fill = background_color),
        panel.background = element_rect(fill = background_color),
        legend.background = element_rect(fill = background_color),
        legend.position = "top",
        legend.box = "horizontal",
        legend.box.just = "left",
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        legend.direction = "horizontal",
        legend.justification = c(0, 0),
        legend.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = "pt")) +
  guides(size = guide_legend(order = 1,
                             nrow = 1,
                             title.position = "top",
                             title.hjust = 0,
                             override.aes = list(fill = pubdarkgray)), 
         fill = guide_colorbar(order = 2,
                               title.position = "top",
                               title.hjust = 0))

# Expected rebound value plot
xrebound_value_plot_fill_limits <- c(0, 0.03)
xrebound_value_plot_fill_breaks <- c(0, 0.03)
xrebound_value_plot_fill_labels <- c("0", "0.03")

model_plot_data <- model_data %>%
  group_by(cell) %>%
  summarise(shot_count = n(),
            cell_avg_xrebound_value = mean(xrebound_value)) %>%
  ungroup() %>%
  right_join(model_data_hexbin_df, by = "cell") %>%
  select(cell, x, y, count, shot_count, cell_avg_xrebound_value)

xrebound_value_plot <- rink +
  geom_star(data = model_plot_data,
            aes(x = x, y = y,
                fill = cell_avg_xrebound_value,
                size = shot_count),
            color = NA,
            starshape = "hexagon",
            show.legend = TRUE) +
  scale_fill_gradient(low = gradient_low_color,
                      high = gradient_high_color,
                      na.value = NA,
                      limits = xrebound_value_plot_fill_limits,
                      breaks = xrebound_value_plot_fill_breaks,
                      labels = xrebound_value_plot_fill_labels) +
  labs(title = "Expected rebound value (xRV)",
       subtitle = "Rebound shot quality for shots on goal",
       caption = "",
       fill = "xRV",
       size = "Shots on goal") +
  ylim(c(-100.1, -24)) +
  xlim(c(-42.6, 42.6)) +
  theme_pub(type = "map", base_size = 36/3) +
  theme(plot.background = element_rect(fill = background_color),
        panel.background = element_rect(fill = background_color),
        legend.background = element_rect(fill = background_color),
        legend.position = "top",
        legend.box = "horizontal",
        legend.box.just = "left",
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        legend.direction = "horizontal",
        legend.justification = c(0, 0),
        legend.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = "pt")) +
  guides(size = guide_legend(order = 1,
                             nrow = 1,
                             title.position = "top",
                             title.hjust = 0,
                             override.aes = list(fill = pubdarkgray)), 
         fill = guide_colorbar(order = 2,
                               title.position = "top",
                               title.hjust = 0))