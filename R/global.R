geoms <- c("geom-bar", "geom-point", "geom-line", "geom-dotplot", "geom-boxplot")
geoms_ <- stringr::str_replace(geoms, "-", "_")

help_panes <- lapply(geoms, function(x) {
  paste0("<h2>", x, "</h1>
         <div class='axis' id='xaxis'></div>
         <div class='axis' id='yaxis'></div>")
})
names(help_panes) <- geoms_

aesthetics <- list(
  "default" = c("x",
                "y",
                "alpha",
                "color",
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
                 "color",
                 "fill",
                 "linetype",
                 "size",
                 "weight"),
  "geom_point" = c("x",
                   "y",
                   "alpha",
                   "color",
                   "fill",
                   "shape",
                   "size"),
  "geom_line" = c("x",
                  "y",
                  "alpha",
                  "color",
                  "linetype",
                  "size"),
  "geom_dotplot" = c("x",
                     "y",
                     "alpha",
                     "color",
                     "fill"),
  "geom_boxplot" = c("x",
                     "ymax",
                     "ymin",
                     "lower",
                     "middle",
                     "upper",
                     "alpha",
                     "color",
                     "fill",
                     "linetype",
                     "shape",
                     "size",
                     "weight")
  )

