#' Wrapper for shinyWidgets::prettyToggle
#'
#' We need to attach the header-icon class to the outermost div
#'
#' @inheritParams shinyWidgets::prettyToggle
#' @return prettyToggle
#'
prettyToggle <- function (inputId, label_on, label_off, icon_on = NULL, icon_off = NULL,
                          value = FALSE, status_on = "success", status_off = "danger",
                          shape = c("square", "curve", "round"), outline = FALSE, fill = FALSE,
                          thick = FALSE, plain = FALSE, bigger = FALSE, animation = NULL,
                          inline = FALSE, width = NULL) {

  myToggle <- shinyWidgets::prettyToggle(inputId = inputId, label_on = label_on, label_off = label_off, icon_on = icon_on, icon_off = icon_off,
                                         value = value, status_on = status_on, status_off = status_off,
                                         shape = shape, outline = outline, fill = fill,
                                         thick = thick, plain = plain, bigger = bigger, animation = animation,
                                         inline = inline, width = width)
  myToggle$attribs$class <- paste(myToggle$attribs$class, "header-icon")
  myToggle
}

widgetHeader <- function(..., disable = FALSE, .list = NULL)
{
  items <- c(list(...), .list)
  tags$header(class = "widget-header", style = if (disable)
    "display: none;", items)
}

widgetBody <- function(..., class = NULL, .list = NULL)
{
  items <- c(list(...), .list)
  tags$section(class = paste0(c("widget-body", class), collapse = " "), items)
}

# Set inclusion for double vectors
`%din%` <- function(x, y) {
  sapply(x, function(x, y) { any(abs(x-y) < 1e-14) }, y = y)
}

# Set difference for double vectors
dsetdiff <- function (x, y)
{
  x <- as.vector(x)
  y <- as.vector(y)
  unique(if (length(x) || length(y))
    x[x %din% y == 0L]
    else x)
}

#' Wrapper for bsplus::bs_accordion
#'
#' Adds appropriate class for rendering
#'
#' @inheritParams bsplus::bs_accordion
#' @param class   Additional classes to add to accordion
#' @return bs_accordion
#'
bs_accordion <- function(id, panel_type = "default", use_heading_link = TRUE, class = NULL) {
  bsplus::bs_accordion(id) %>%
    bsplus::bs_set_opts(panel_type, use_heading_link = use_heading_link) %>% {
      .$attribs$class <- paste(c(.$attribs$class, "serenity-accordion", class), collapse = " ")
      .
    }
}

# Data UI
dataTypeToUI <- function(var, .icon = FALSE) {
  switch(class(var)[1],
         'integer' =,
         'numeric' = switch(as.character(.icon),
                            'FALSE' = 'numeric',
                            'TRUE' = icon("signal")),
         'character' =,
         'ordered' =,
         'factor' = switch(as.character(.icon),
                           'FALSE' = 'factor',
                           'TRUE' = icon("shapes"))
  )
}

# Useful to make sure code strings don't get invalidated if they don't change
dedupe <- function(r) {
  val <- NULL
  makeReactiveBinding("val")
  observe(val <<- r(), priority = 10)
  reactive(val)
}

reorderElements <- function(x, orderBy = NULL) {
  ordering <- unique(unlist(c(orderBy, x)))
  if (is.list(x)) {
    return(purrr::map(x, ~ ordering[ordering %in% .]))
  } else {
    return(ordering[ordering %in% x])
  }
}

#' Get layer information from namespace string
#'
#' @param ns Namespace function
#'
#' @return List of layer information, including app name, plot id, geom,
#'   layer id, and aesthetic
getLayerInfo <- function(ns) {
  ns_levels <- stringr::str_split(ns(''), '-')[[1]]

  # Get plot_id, geom, layer_id, and aesthetic
  ds_ind <- which(ns_levels == "ds")
  aes_ind <- which(ns_levels == "aesthetics")
  list(
    app = ns_levels[1],
    plot_id = ifelse(length(ds_ind) > 0, paste(ns_levels[3:(ds_ind-1)], collapse="-"), "geom-blank"),
    geom = ifelse(length(ds_ind) > 0, plots[plots$id == paste(ns_levels[3:(ds_ind-1)], collapse="-"), "geom"], "geom-blank"),
    layer_id = ifelse(length(ds_ind) > 0, paste(ns_levels[3:(ds_ind+1)], collapse = "-"), "geom-blank-ds-1"),
    aesthetic = switch(length(aes_ind) > 0, paste(ns_levels[aes_ind+1], collapse = "-"))
  )
}
