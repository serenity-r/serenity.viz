geoms <- c("geom-bar", "geom-point", "geom-line", "geom-dotplot", "geom-boxplot")
geoms_ <- stringr::str_replace(geoms, "-", "_")

help_panes <- lapply(geoms, function(x) {
  paste0("<h2>", x, "</h1>
         <div class='axis' id='xaxis'></div>
         <div class='axis' id='yaxis'></div>")
})
names(help_panes) <- geoms_

# GGPLOT
# List of all geom aesthetics: geom_boxplot()$geom$aesthetics() (don't need group - will handle internally)
# List of required aes: geom_boxplot()$geom$required_aes (some are calculated for you)
# List of optional aes: geom_boxplot()$geom$optional_aes
# List of default aes values: geom_boxplot()$geom$default_aes
# List of assigned aes values: geom_boxplot()$aes_params

# List of all geom parameters: geom_boxplot()$geom$parameters()
# List of default parameter values: geom_boxplot()$geom_params

gg_aesthetics <- list(
  "default" = c("x",
                "y",
                "alpha",
                "colour",
                "fill",
                "linetype",
                "size",
                "weight",
                "xmin",
                "xmax",
                "ymin",
                "ymax"),
  "geom_bar" = c("x",
                 "alpha",
                 "colour",
                 "fill",
                 "linetype",
                 "size",
                 "weight"),
  "geom_point" = ggplot2::GeomPoint$aesthetics(),
  "geom_line" = ggplot2::GeomLine$aesthetics(),
  "geom_dotplot" = c("x",
                     "y",
                     "alpha",
                     "colour",
                     "fill"),
  "geom_boxplot" = c("x",
                     "ymax",
                     "ymin",
                     "lower",
                     "middle",
                     "upper",
                     "alpha",
                     "colour",
                     "fill",
                     "linetype",
                     "shape",
                     "size",
                     "weight")
  )

# Set color palette rosetta stone
# http://www.melissaclarkson.com/resources/R_guides/documents/colors_Ver2.pdf
crgb <- col2rgb(cc <- colors())
colnames(crgb) <- cc
colours_tbl <- dplyr::tbl_df(t(crgb)) %>%
  dplyr::mutate(name = cc,
                hex = rgb(red, green, blue, maxColorValue = 255)) %>%
  dplyr::select(name, hex, red, green, blue)
