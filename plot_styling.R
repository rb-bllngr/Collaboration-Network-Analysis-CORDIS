# --- Projektfarben ----------------------------------------------------------------------

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

palette_discrete <- RColorBrewer::brewer.pal(8, "Set2")
lmu_sequentiell <- RColorBrewer::brewer.pal(9, "Purples")[2:9]
lmu_divergent <- RColorBrewer::brewer.pal(11, "RdBu")

# Standardpalette für kategoriale Variablen
lmu_palette_discrete <- c(
  palette_discrete[3],
  palette_discrete[2],
  palette_discrete[1],
  palette_discrete[4:8]
)

# --- Globales Theme ---------------------------------------------------------------------

theme_lmu <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(text = element_text(size = 23),

          # Titel
          plot.title = element_text(face = "bold", color = lmu_colors$black, size = base_size + 2),
          plot.subtitle = element_text(color = lmu_colors$black, size = base_size),
          plot.caption = element_text(color = lmu_colors$black, size = base_size - 2),

          # Achsen
          axis.title = element_text(face = "bold", color = lmu_colors$black),
          axis.text = element_text(color = lmu_colors$black),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),

          # Legende
          legend.title = element_text(face = "bold",color = lmu_colors$black),
          legend.text = element_text(color = lmu_colors$black),
          legend.position = "right",

          # Grid und Hintergrund
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
          panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
          panel.background = element_rect(fill = lmu_colors$white, color = NA),
          plot.background = element_rect(fill = lmu_colors$white, color = NA),
          panel.border = element_rect(fill = NA),

          # Sonstiges
          plot.title.position = "plot",
          strip.text = element_text(face = "bold", color = lmu_colors$black)
    )
}

# Theme global aktivieren
set_theme_lmu <- function(base_size = 12, base_family = "sans") {
  theme_set(theme_lmu(base_size = base_size, base_family = base_family))
}

# --- Farbhilfen -------------------------------------------------------------------------

# Standardfarbe für einfache Plots ohne Gruppierung
lmu_default_color <- function() {
  lmu_colors$black
}

# Diskrete Palette für Gruppen
lmu_discrete_palette <- function() {
  lmu_palette_discrete
}

# Optional: Fehler-/Warnfarbe
lmu_error_color <- function() {
  lmu_colors$red
}

# --- Speicherfunktion -------------------------------------------------------------------

save_plot_lmu <- function(plot, filename, folder = "Plots", width = 10, height = 5.625,
                          dpi = 300, bg = lmu_colors$white) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }

  full_path <- file.path(folder, filename)
  ggsave(filename = full_path, plot = plot, width = width, height = height, dpi = dpi, bg = bg)
  message("Plot gespeichert unter: ", full_path)
}