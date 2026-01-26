NORD <- list(
  # Polar Night (backgrounds)
  nord0 = "#2e3440", nord1 = "#3b4252", nord2 = "#434c5e", nord3 = "#4c566a",
  # Snow Storm (text/foregrounds)
  nord4 = "#d8dee9", nord5 = "#e5e9f0", nord6 = "#eceff4",
  # Frost (cool accents)
  nord7 = "#8fbcbb", nord8 = "#88c0d0", nord9 = "#81a1c1", nord10 = "#5e81ac",
  # Aurora (semantic colors)
  nord11 = "#bf616a", nord12 = "#d08770", nord13 = "#ebcb8b",
  nord14 = "#a3be8c", nord15 = "#b48ead",
  # Extended znord colors
  znord0 = "#242933",
  znord2 = "#75849E",
  znord3 = "#8B99AC",
  znord6 = "#f8f9fb",
  znord7 = "#8fbca5",
  znord8 = "#9fcddc",
  znord9 = "#6CB5EA",
  znord10 = "#6a8a9d",
  znord11 = "#D24B57",
  znord12 = "",
  znord14 = "#90bc8f",
  znord15 = "#d8a2e2"
)

# Color palettes for different use cases
nord_aurora <- c(NORD$nord11, NORD$nord12, NORD$nord13, NORD$znord14, NORD$znord15)
nord_frost <- c(NORD$nord7, NORD$nord8, NORD$nord9, NORD$nord10)
nord_mixed <- c(NORD$nord8, NORD$znord14, NORD$nord12, NORD$nord15, NORD$nord9)

theme_znord <- function() {
  
  theme_minimal(
    base_family = "Source Sans 3"
  ) %+replace%
    # Theme
    theme(
      # layout
      legend.position = "top",
      plot.background = element_rect(fill = NORD$znord0, color = NA),
      panel.background = element_rect(fill = NORD$znord0, color = NA),
      
      # Grid Lines
      panel.grid.major = element_line(colour = NORD$nord3),
      panel.grid.minor = element_blank(),
      
      # Text elements
      text = element_text(color = NORD$nord4, family = "Source Sans 3"),
      
      # Axes
      axis.text = element_text(color = NORD$nord4, size = rel(0.8)),
      axis.title = element_text(color = NORD$nord5, face = "bold"),
      axis.line = element_blank(),  # No axis lines by default
      axis.ticks = element_line(color = NORD$nord3, linewidth = 0.5),
      
      # Facet elements
      strip.background = element_rect(fill = NORD$nord1, color = NORD$znord3),
      strip.text = element_text(
        color = NORD$nord6,
        face = "bold",
        size = rel(1),
        margin = margin(5, 5, 5, 5)
      )
    )
}




