library(magrittr)
library(dplyr)
library(forcats)
library(ggplot2)
library(miniUI)

## Available for all R processes and UI

geoms <- c("geom-bar", "geom-point", "geom-line", "geom-dotplot", "geom-boxplot")
geoms_ <- stringr::str_replace(geoms, "-", "_")

help_panes <- lapply(geoms, function(x) {
  paste0("<h2>", x, "</h2>
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
  "geom_dotplot" = ggplot2::GeomDotplot$aesthetics(),
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

# Surrounding div for buttons and labels
aes_wrap <- function(content, aes, default='') {
  tagList(
    div(
      id = paste0(aes, '-wrap'),
      class = paste0('aes-wrap ', default),
      content
    )
  )
}

create_aes_empty <- function(aes, default='') {
  tagList(
    span(
      'Not set'
    ) %>%
      aes_wrap(aes, default)
  )
}

# Set color palette rosetta stone
# http://www.melissaclarkson.com/resources/R_guides/documents/colors_Ver2.pdf
crgb <- col2rgb(cc <- colors())
colnames(crgb) <- cc
colours_tbl <- dplyr::tbl_df(t(crgb)) %>%
  dplyr::mutate(name = cc,
                hex = rgb(red, green, blue, maxColorValue = 255)) %>%
  dplyr::select(name, hex, red, green, blue)

# Colour translator
#   Right now assume col is an R colour
colour_to_hex <- function(col) {
  if (!grepl("^#[0-9a-fA-F]{6}", col)) {
    return(dplyr::filter(colours_tbl, name == col)$hex)
  } else {
    return(col)
  }
}

# Create aesthetic input control
# aes_val is assumed to be truthy
create_aes_input <- function(inputId, aes, aes_val, default='') {
  tagList(
    switch(aes,
           'shape' = sliderInput(inputId = inputId,
                                 label = "",
                                 min = 0,
                                 max = 25,
                                 step = 1,
                                 value = aes_val),
           'colour' = ,
           'fill' = colourpicker::colourInput(inputId = inputId,
                                              label = "",
                                              value = colour_to_hex(aes_val)),
           'weight' = ,
           'size' = ,
           'stroke' = sliderInput(inputId = inputId,
                                  label = "",
                                  min = 0.1,
                                  max = 10,
                                  step = 0.1,
                                  value = aes_val),
           'alpha' = sliderInput(inputId = inputId,
                                 label = "",
                                 min = 0,
                                 max = 1,
                                 value = aes_val),
           'linetype' = sliderInput(inputId = inputId,
                                    label = "",
                                    min = 0,
                                    max = 6,
                                    value = aes_val),
           ''
    ) %>%
      aes_wrap(aes, default)
  )
}

# Pull in modules ----

# Do not add local=TRUE!! Was having issues with having to source
#  the module files previously with local=TRUE
lapply(list.files("modules", recursive=TRUE),
       function (module) {
         source(paste("modules", module, sep="/"))
       })
