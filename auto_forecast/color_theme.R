

library(fresh)
library(ggplot2)

# COLORS ----

# * Branding -----
text_color  <- "#000000"
brand_color <- "black"
info_color  <- "#00c0ef"
bg_color    <- "#FFFFFF"
strip_color <- info_color

spotify_white          <- "#FFFFFF"
spotify_red            <- "#FC5185"
spotify_green          <- "#1DB954"
spotify_yellow         <- "#F4D58D"
spotify_purple         <- "#621185"
spotify_blue           <- "#0845D1"
spotify_light_blue     <- info_color
spotify_green_alt      <- "#1EA896"
spotify_dark_red       <- "#A10836"
spotify_gold           <- "#E6A510"

palette_spotify <- c(
    spotify_red,
    text_color,
    # spotify_white,
    spotify_green,
    spotify_yellow,
    spotify_purple,
    spotify_blue,
    spotify_light_blue,
    spotify_green_alt,
    spotify_dark_red,
    spotify_gold
)

# * BS Theme ----
my_theme <- create_theme(
    adminlte_color(
        red        = spotify_red,
        light_blue = info_color,
        green      = spotify_green,
        black      = bg_color
    )
)

# * GGplot Theme ----
theme_ggspotify <- function() {

    theme(
        strip.background = element_rect(fill = strip_color, color = strip_color),
        strip.text = element_text(color = text_color),
        panel.background = element_rect(fill = bg_color),
        plot.background  = element_rect(fill = bg_color),
        text = element_text(colour = text_color),
        axis.line.y = element_line(colour = text_color),
        axis.line.x = element_line(colour = text_color),
        panel.grid.major = element_line(colour = strip_color),
        axis.ticks = element_line(colour = strip_color),
        legend.background = element_rect(fill = strip_color)
    )
}

# * Plotly Font ----
plotly_font <- list(
    family = "Roboto Condensed"
)
