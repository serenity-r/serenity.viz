library(magrittr)
library(dplyr)
library(forcats)
library(ggplot2)
library(miniUI)
library(dragulaSelectR)

## Available for all R processes and UI

geoms <- c("geom-bar", "geom-point", "geom-line", "geom-dotplot", "geom-boxplot", "geom-violin", "geom-rug")
geoms_ <- stringr::str_replace(geoms, "-", "_")

help_panes <- lapply(geoms, function(x) {
  paste0("<h2>", x, "</h2>
         <div class='axis' id='xaxis'></div>
         <div class='axis' id='yaxis'></div>")
})
names(help_panes) <- geoms_

makeReactiveTrigger <- function(init_val = NULL) {
  rv <- reactiveValues(a = 0)
  val <- init_val
  list(
    get = function() {
      val
    },
    set = function(new_val) {
      val <<- new_val
    },
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}

# GGPLOT
# List of all geom aesthetics: geom_boxplot()$geom$aesthetics() (don't need group - will handle internally)
# List of required aes: geom_boxplot()$geom$required_aes (some are calculated for you)
# List of optional aes: geom_boxplot()$geom$optional_aes
# List of default aes values: geom_boxplot()$geom$default_aes
# List of assigned aes values: geom_boxplot()$aes_params

# List of all geom parameters: geom_boxplot()$geom$parameters()
# List of default parameter values: geom_boxplot()$geom_params

gg_aesthetics <- list(
  "geom-blank" = c("x",
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
  "geom-bar" = c("x",
                 "alpha",
                 "colour",
                 "fill",
                 "linetype",
                 "size",
                 "weight"),
  "geom-point" = ggplot2::GeomPoint$aesthetics(),
  "geom-line" = ggplot2::GeomLine$aesthetics(),
  "geom-dotplot" = ggplot2::GeomDotplot$aesthetics(),
  "geom-boxplot" = c(ggplot2::GeomBoxplot$aesthetics(), "y"),
  "geom-violin" = ggplot2::GeomViolin$aesthetics(),
  "geom-rug" = ggplot2::GeomRug$aesthetics()
)
ordering <- unique(unlist(gg_aesthetics))
gg_aesthetics <- purrr::map(gg_aesthetics, ~ ordering[ordering %in% .])

# Pull in modules ----

# Do not add local=TRUE!! Was having issues with having to source
#  the module files previously with local=TRUE
lapply(list.files("modules", recursive=TRUE),
       function (module) {
         source(paste("modules", module, sep="/"))
       })
