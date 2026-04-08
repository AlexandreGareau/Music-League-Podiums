vapor_cols <- list(
  bg        = "#1a0933",  # deep purple background
  panel     = "#22114a",  # slightly lighter card
  grid      = "#3b2a6d",
  text      = "#f8f9fa",
  muted     = "#b8b8d1",
  
  primary   = "#6f42c1",  # Bootstrap primary (purple)
  secondary = "#ea39b8",  # magenta
  info      = "#3dd5f3",  # cyan
  success   = "#00bc8c",  # teal-green
  warning   = "#f39c12",
  danger    = "#e74c3c"
)

vapor_palette <- c(
  "#ea39b8", # magenta
  "#3dd5f3", # cyan
  "#6f42c1", # purple
  "#00bc8c", # teal
  "#f39c12", # orange
  "#e74c3c", # red
  "#9b5de5", # violet
  "#00f5d4"  # bright aqua
)

vapor_gradient <- c(
  "#6f42c1",
  "#9b5de5",
  "#ea39b8",
  "#ff6ec7",
  "#3dd5f3"
)

library(ggplot2)

theme_vapor <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = vapor_cols$bg, colour = NA),
      panel.background = element_rect(fill = vapor_cols$panel, colour = NA),
      
      panel.grid.major = element_line(color = vapor_cols$grid, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      
      axis.text  = element_text(color = vapor_cols$muted),
      axis.title = element_text(color = vapor_cols$text, face = "bold"),
      
      plot.title = element_text(
        color = vapor_cols$text,
        face = "bold",
        size = rel(1.3)
      ),
      
      plot.subtitle = element_text(color = vapor_cols$muted),
      
      legend.background = element_rect(fill = vapor_cols$panel, colour = NA),
      legend.key        = element_rect(fill = vapor_cols$panel, colour = NA),
      legend.text       = element_text(color = vapor_cols$text),
      legend.title      = element_text(color = vapor_cols$text, face = "bold")
    )
}
scale_color_vapor <- function(...) {
  scale_color_manual(values = vapor_palette, ...)
}
scale_fill_vapor <- function(...) {
  scale_fill_manual(values = vapor_palette, ...)
}
scale_fill_vapor_gradient <- function(...) {
  scale_fill_gradientn(colours = vapor_gradient, ...)
}