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
