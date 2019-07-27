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
