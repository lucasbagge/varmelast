spotify_theme <- function() {
    search_icon <- function(fill = "none") {
        # Icon from https://boxicons.com
        svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"><path fill="%s" d="M10 18c1.85 0 3.54-.64 4.9-1.69l4.4 4.4 1.4-1.42-4.39-4.4A8 8 0 102 10a8 8 0 008 8.01zm0-14a6 6 0 11-.01 12.01A6 6 0 0110 4z"/></svg>', fill)
        sprintf("url('data:image/svg+xml;base64,%s')", jsonlite::base64_enc(svg))
    }

    text_color <- "hsl(0, 0%, 95%)"
    text_color_light <- "hsl(0, 0%, 70%)"
    text_color_lighter <- "hsl(0, 0%, 55%)"
    bg_color <- "hsl(0, 0%, 10%)"

    reactableTheme(
        color = text_color,
        backgroundColor = bg_color,
        borderColor = "hsl(0, 0%, 16%)",
        borderWidth = "1px",
        highlightColor = "rgba(255, 255, 255, 0.1)",
        cellPadding = "10px 8px",
        style = list(
            fontFamily = "Work Sans, Helvetica Neue, Helvetica, Arial, sans-serif",
            fontSize = "14px",
            "a" = list(
                color = text_color,
                "&:hover, &:focus" = list(
                    textDecoration = "none",
                    borderBottom = "1px solid currentColor"
                )
            ),
            ".number" = list(
                color = text_color_light,
                fontFamily = "Source Code Pro, Consolas, Monaco, monospace"
            ),
            ".tag" = list(
                padding = "2px 4px",
                color = "hsl(0, 0%, 40%)",
                fontSize = "12px",
                border = "1px solid hsl(0, 0%, 24%)",
                borderRadius = "2px"
            )
        ),
        headerStyle = list(
            color = text_color_light,
            fontWeight = 400,
            fontSize = "12px",
            letterSpacing = "1px",
            textTransform = "uppercase",
            "&:hover, &:focus" = list(color = text_color)
        ),
        rowHighlightStyle = list(
            ".tag" = list(color = text_color, borderColor = text_color_lighter)
        ),
        # Full-width search bar with search icon
        searchInputStyle = list(
            paddingLeft = "30px",
            paddingTop = "8px",
            paddingBottom = "8px",
            width = "100%",
            border = "none",
            backgroundColor = bg_color,
            backgroundImage = search_icon(text_color_light),
            backgroundSize = "16px",
            backgroundPosition = "left 8px center",
            backgroundRepeat = "no-repeat",
            "&:focus" = list(backgroundColor = "rgba(255, 255, 255, 0.1)", border = "none"),
            "&:hover, &:focus" = list(backgroundImage = search_icon(text_color)),
            "::placeholder" = list(color = text_color_lighter),
            "&:hover::placeholder, &:focus::placeholder" = list(color = text_color)
        ),
        paginationStyle = list(color = text_color_light),
        pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 20%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 24%)")
    )
}
