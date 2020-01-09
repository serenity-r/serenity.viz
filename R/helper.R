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

bs_accordion <- function(id, panel_type = "default", use_heading_link = TRUE, class = NULL) {
  bsplus::bs_accordion(id) %>%
    bsplus::bs_set_opts(panel_type, use_heading_link = use_heading_link) %>% {
      .$attribs$class <- paste(c(.$attribs$class, "serenity-accordion", class), collapse = " ")
      .
    }
}
