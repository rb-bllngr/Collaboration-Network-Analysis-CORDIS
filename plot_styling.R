# --- Project Colors ---------------------------------------------------------------------

lmu_colors <- list(
  green = "#00883A",
  black = "#232323",
  white = "#FFFFFF",
  blue = "#0F1987",
  cyan = "#009FE3",
  violet = "#8C4091",
  red = "#D71919",
  orange = "#F18700"
)

okabe_ito <- palette.colors(8, palette = "Okabe-Ito")
set1 <- palette.colors(2, "Set1")

# --- Global Theme -----------------------------------------------------------------------

theme_lmu <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(text = element_text(size = base_size + 0),  # TODO: originally +11

          # Title
          plot.title = element_text(face = "bold", color = lmu_colors$black, size = base_size + 2),
          plot.subtitle = element_text(color = lmu_colors$black, size = base_size),
          plot.caption = element_text(color = lmu_colors$black, size = base_size - 2),

          # Axes
          axis.title = element_text(color = lmu_colors$black),  # TODO: face = "bold"
          axis.text = element_text(color = lmu_colors$black),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),

          # Legend
          legend.title = element_text(color = lmu_colors$black, # TODO: face = "bold"
                                      hjust = 0),
          legend.text = element_text(color = lmu_colors$black),
          legend.position = "right",

          # Grid and Background
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
          panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
          panel.background = element_rect(fill = lmu_colors$white, color = NA),
          plot.background = element_rect(fill = lmu_colors$white, color = NA),
          panel.border = element_rect(fill = NA),

          # Miscellaneous
          plot.title.position = "plot",
          strip.text = element_text(color = lmu_colors$black)  # TODO: face = "bold"
    )
}

# Activate global theme
set_theme_lmu <- function(base_size = 12, base_family = "sans") {
  theme_set(theme_lmu(base_size = base_size, base_family = base_family))
}

# --- Color Helpers ----------------------------------------------------------------------

# Default color for plots without grouping
lmu_default_color <- function() {
  lmu_colors$black
}

# Discrete palette for groups
lmu_discrete_palette <- function() {
  lmu_colors
}

# Discrete palette for groups with colorblind-friendly focus
colorblindfriendly <- function() {
  c(okabe_ito[-1], "#000000")
}

# Optional: Error/Warning color
lmu_error_color <- function() {
  lmu_colors$red
}

# --- Saving Function --------------------------------------------------------------------

save_plot_lmu <- function(plot, filename, folder = "Plots", width = 10, height = 5.625,
                          dpi = 300, bg = lmu_colors$white) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }

  full_path <- file.path(folder, filename)
  ggsave(filename = full_path, plot = plot, width = width, height = height, dpi = dpi, bg = bg)
  message("Plot gespeichert unter: ", full_path)
}