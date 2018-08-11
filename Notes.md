# Set mapping variable as a string

```
gg$mapping <- aes_string(x = "Species", y = "Sepal.Length") [or]
  gg$mapping <- aes(x = !!sym("Species"), y = !!sym("Sepal.Width"))
```

Note:  You must update `gg$labels`!!!  See below

[https://edwinth.github.io/blog/dplyr-recipes/](Common quosure operations)

# Get mapping variable as a string

```
as.character(rlang::get_expr(gg$mapping$x))
```

# Get aesthetics programmatically from a string

```
eval(parse(text=paste0(str_replace("geom-bar", "-", "_"), "()")))$geom$aesthetics()
```

Note that there are proto ggplot objects.  So, can also use, for example, `GeomBar$aesthetics()`.

# How to set a mapping component

```
gg$mapping[["y"]] <- aes_string("y" = "Petal.Width")[["y"]]
gg$labels[["y"]] <- "Petal.Width"
```
or
```
gg$mapping[["y"]] <- aes_string("y" = "Petal.Width")[["y"]]
gg <- gg + labs("y" = "Petal.Width")
```

# Adding a layer

```
gg <- gg + geom_point()
```

# Accessing layers

```
gg$layers[[1]]
```

# Naming layers (match id of JS element)

```
names(gg$layers) <- c("layer1", "layer2")
```

# Reordering layers

```
gg$layers[c("layer2","layer1")]
```

# Ready Layer One

```
# Check to make sure layer is ready to render
readyLayerOne <- reactive({
})

# Check to make sure plot is ready to render
readyPlotOne <- reactive({
  # Grabbed from ggplot2::ggplot_build in plot_build.r
  plot <- values$gg
  layers <- plot$layers
  data <- plot$data
  facet <- plot$facet
  coord <- plot$coordinates
  plot_env <- plot$plot_env
  
  layer_data <- lapply(layers, function(y) y$layer_data(data))
  layout <- ggproto(NULL, Layout, facet = facet, coord = coord)
  
  # Apply function to layer and matching data
  by_layer <- function(f) {
    out <- vector("list", length(data))
    for (i in seq_along(data)) {
      out[[i]] <- f(l = layers[[i]], d = data[[i]])
    }
    out
  }
  data <- layout$setup(layer_data, data, plot_env)
  data <- by_layer(function(l, d) l$compute_aesthetics(d, plot))
  data <- by_layer(function(l, d) l$compute_statistic(d, layout))
  data <- by_layer(function(l, d) l$map_statistic(d, plot))
  
  all(unlist(by_layer(function(l, d) length(setdiff(l$geom$required_aes, names(d))) == 0)))
})
```

```
  #
  # Layers need to be a reactive value
  #

  # purrr::walk(unique(unlist(aesthetics)), function(aes) {
  #   ui <- ""
  #
  #   if (aes == 'x') {
  #   } else
  #     if (aes == 'alpha') {
  #       ui <- sliderInput(inputId = paste0(aes, '-slider'),
  #                         label = "",
  #                         min = 0,
  #                         max = 1,
  #                         value = 1)
  #     } else
  #       if (aes == 'size') {
  #         ui <- sliderInput(inputId = paste0(aes, '-slider'),
  #                           label = "",
  #                           min = 0.1,
  #                           max = 10,
  #                           step = 0.1,
  #                           value = 0.5)
  #       } else
  #         if (aes == 'colour') {
  #           ui <- colourpicker::colourInput(inputId = paste0(aes, '-colourpicker'),
  #                                           label = "",
  #                                           value = "black")
  #         } else
  #           if (aes == 'fill') {
  #             ui <- colourpicker::colourInput(inputId = paste0(aes, '-colourpicker'),
  #                                             label = "",
  #                                             value = "black")
  #           }
  #
  #   output[[aes]] <<- renderUI({ui})
  # })
```
