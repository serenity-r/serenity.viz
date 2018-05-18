geoms <- c("geom-bar", "geom-point", "geom-line", "geom-dotplot", "geom-boxplot")
geoms_ <- stringr::str_replace(geoms, "-", "_")

help_panes <- lapply(geoms, function(x) {
  paste0("<h2>", x, "</h1>
         <div class='axis' id='xaxis'></div>
         <div class='axis' id='yaxis'></div>")
})
names(help_panes) <- geoms_
