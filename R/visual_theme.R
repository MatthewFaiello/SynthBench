# =========================================================
# R/visual_theme.R
# Shared visual constants and ggplot theme helpers
# =========================================================
# Purpose:
#   Keep plot colors, base sizes, and shared plot themes in one place
#   so comparison visuals and coefficient tables stay visually consistent.
#
# Main objects/helpers:
#   - PLOT_SETTINGS
#   - dde_* color constants
#   - theme_modelcomp()
#   - empty_plot()
# =========================================================


PLOT_SETTINGS <- list(
  base_size = 12
)

dde_blue <- "#194a78"
dde_blue_dark <- "#123758"
dde_orange <- "#d98b00"
dde_orange_soft <- "#fff7ea"
dde_bg <- "#f5f7fb"
dde_surface <- "#ffffff"
dde_surface_soft <- "#fbfcfe"
dde_border <- "#d8e2ec"
dde_border_strong <- "#c7d5e2"
dde_text <- "#1f2937"
dde_muted <- "#5b6875"


theme_modelcomp <- function(base_size = PLOT_SETTINGS$base_size) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = dde_bg, color = NA),
      panel.background = ggplot2::element_rect(fill = dde_surface, color = NA),
      panel.grid.major.x = ggplot2::element_line(color = dde_border, linewidth = 0.35),
      panel.grid.major.y = ggplot2::element_line(color = dde_border, linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = dde_border, fill = NA, linewidth = 0.6),
      
      plot.title = ggplot2::element_text(
        face = "bold",
        color = dde_blue_dark,
        size = ggplot2::rel(1.25)
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        color = dde_muted,
        size = ggplot2::rel(0.9),
        lineheight = 1.15,
        margin = ggplot2::margin(b = 8),
        padding = ggplot2::margin(0, 0, 0, 0),
        fill = NA,
        box.color = NA,
        width = grid::unit(1, "npc")
      ),
      plot.caption = ggplot2::element_text(
        color = dde_muted,
        hjust = 0,
        size = ggplot2::rel(0.9)
      ),
      
      axis.title = ggplot2::element_text(color = dde_text, face = "bold"),
      axis.text = ggplot2::element_text(color = dde_text),
      axis.text.y = ggplot2::element_text(
        size = ggplot2::rel(0.92),
        hjust = 1,
        lineheight = 0.95
      ),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(0.92)),
      
      strip.background = ggplot2::element_rect(
        fill = dde_orange_soft,
        color = dde_border_strong,
        linewidth = 0.6
      ),
      strip.text = ggplot2::element_text(face = "bold", color = dde_blue_dark),
      
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(fill = dde_bg, color = NA),
      legend.key = ggplot2::element_rect(fill = dde_bg, color = NA),
      plot.margin = ggplot2::margin(10, 14, 10, 10)
    )
}


empty_plot <- function(label) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 1, y = 1, label = label, color = dde_text) +
    ggplot2::theme_void()
}