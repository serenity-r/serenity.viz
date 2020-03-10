set.seed(4)

img_repo <- "inst/www/img"

serenity.fill <- "#54585E"
theme_serenity <- function(ggobj, X = 0, Y = 0, W = 10, H = 10) {
  ggobj +
    scale_x_continuous(limits = c(X, W+1)) +
    scale_y_continuous(limits = c(Y, H+1)) +
    coord_cartesian(xlim = c(X, W), ylim = c(Y, H))
}
save_plot <- function(ggobj, W = 8, H = 8) {
  geom_name <- rlang::quo_name(enquo(ggobj))
  ggsave(file = here::here(paste0(img_repo, "/geom_", geom_name, ".svg")), plot = ggobj, width = W, height = H)
  system2("svgo", args = paste0(img_repo, "/geom_", geom_name, ".svg --disable=minifyStyles,convertStyleToAttrs --precision=0"))
}

theme_layer <- theme_bw() +
  theme(aspect.ratio = 1,
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=6),
        panel.grid.major = element_line(size = 1, colour = "#878787"),
        panel.grid.minor = element_line(size = 1, colour = "#878787"))

## 1. Graphical Primitives ====

# __ geom_ribbon ----

y <- exp(seq(from = 0, to = 2, length.out = 11)) + rnorm(11)
ribbon_data <- data.frame(
  x = 0:10,
  ymin = y - 0.3*exp(seq(from = 0, to = 2, length.out = 11)),
  ymax = y + 0.3*exp(seq(from = 0, to = 2, length.out = 11))
)

a <- ggplot(ribbon_data, aes(x)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = serenity.fill)

ribbon <- theme_serenity(a + theme_layer)

save_plot(ribbon)

# __ geom_blank ----

a <- ggplot(ribbon_data, aes(x, ymin)) +
  geom_blank()

blank <- theme_serenity(a + theme_layer)

save_plot(blank)

# __ geom_curve ----

curve_data <- data.frame(
  x = 2, y = 2, xend = 8, yend = 8
)

a <- ggplot(curve_data, aes(x, y)) +
  geom_curve(aes(xend = xend, yend = yend),
             curvature = -0.5,
             size = 6)

curve <- theme_serenity(a + theme_layer)

save_plot(curve)

# __ geom_path ----

path_data <- data.frame(
  x = c(1, 4, 8, 5, 2, 6, 9),
  y = c(1, 8, 9, 2, 7, 5, 2)
)

a <- ggplot(path_data, aes(x, y)) +
  geom_path(size = 4)

path <- theme_serenity(a + theme_layer)

save_plot(path)

# __ geom_polygon ----

polygon_data <- data.frame(
  x = c(2, 1, 1, 6, 9, 5),
  y = c(1, 4, 7, 9, 3, 1)
)

a <- ggplot(polygon_data, aes(x, y)) +
  geom_polygon(fill = serenity.fill)

polygon <- theme_serenity(a + theme_layer)

save_plot(polygon)

# __ geom_rect ----

rect_data <- data.frame(
  x = c(1, 1, 9, 9),
  y = c(3, 7, 7, 3)
)

a <- ggplot(rect_data, aes(x, y)) +
  geom_polygon(fill = serenity.fill)

rect <- theme_serenity(a + theme_layer)

save_plot(rect)

## 1.1 Lines ====

# __ geom_abline ----

abline_data <- data.frame(
  intercept = 2,
  slope = 0.5
)

a <- ggplot(abline_data) +
  geom_abline(aes(intercept = intercept, slope = slope), size = 6)

abline <- theme_serenity(a + theme_layer)

save_plot(abline)

# __ geom_vline ----

a <- ggplot() +
  geom_vline(aes(xintercept = 4), size = 6)

vline <- theme_serenity(a + theme_layer)

save_plot(vline)

# __ geom_hline ----

a <- ggplot() +
  geom_hline(aes(yintercept = 4), size = 6)

hline <- theme_serenity(a + theme_layer)

save_plot(hline)

# __ geom_segment ----

a <- ggplot() +
  geom_segment(aes(x = 2, y = 3, xend = 8, yend = 7), size = 6)

segment <- theme_serenity(a + theme_layer)

save_plot(segment)

# __ geom_spoke ----

spoke_data <- data.frame(
  angle = seq(from = 0, to = 2*pi, length.out = 6)
)

a <- ggplot(spoke_data, aes(x = 5, y = 5)) +
  geom_spoke(aes(angle = angle),
             radius = 4,
             size = 4)

spoke <- theme_serenity(a + theme_layer)

save_plot(spoke)

## 2. One Variable ====

## 2.1 Continuous ====

# __ geom_area ----

a <- ggplot(ribbon_data, aes(x = x, y = ymax)) +
  geom_area()

area <- theme_serenity(a + theme_layer)

save_plot(area)

# __ geom_density ----

x <- seq(from = 0, to = 10, length.out = 100)

a <- ggplot() +
  geom_line(aes(x = x, y = 30*dnorm(x, mean = 5, sd = 1.5)),
            size = 6)

density <- theme_serenity(a + theme_layer)

save_plot(density)

# __ geom_dotplot ----

dotplot_data <- data.frame(
  x = c(rep(1, 2), rep(3, 6), rep(5, 4), rep(7, 1), rep(9, 5))
)

a <- ggplot(dotplot_data, aes(x)) +
  geom_dotplot(binwidth = 2, dotsize = 0.6, stackratio = 1.35)

dotplot <- theme_serenity(a + theme_layer)

save_plot(dotplot)

# __ geom_freqpoly ----

freqpoly_data <- data.frame(
  x = c(rep(2, 3), rep(3, 7), rep(5, 6), rep(7, 1), rep(9, 5))
)

a <- ggplot(freqpoly_data, aes(x)) +
  geom_freqpoly(binwidth = 2, size = 6)

freqpoly <- theme_serenity(a + theme_layer)

save_plot(freqpoly)

# __ geom_histogram ----

histogram_data <- data.frame(
  x = rnorm(1000, mean = 5, sd = 2)
)

a <- ggplot(histogram_data, aes(x)) +
  geom_histogram(binwidth = 0.8, fill = serenity.fill)

histogram <- theme_serenity(a + theme_layer, H = 200)

save_plot(histogram)

# __ geom_qq ----

# qq_data <- data.frame(
#   x = rnorm(40, mean = 5, sd = 2)
# )
#
# a <- ggplot(qq_data, aes(sample = x)) +
#   geom_qq(size = 5)
#
# qq <- theme_serenity(a + theme_layer, X = -3, Y = -1, W = 3, H = 11)
#
# save_plot(qq, W = 10, H = 10)

qq_data <- data.frame(
  x = rnorm(40)
)

a <- ggplot(qq_data, aes(sample = x)) +
  geom_qq(size = 5)

qq <- theme_serenity(a + theme_layer, X = -2, Y = -2, W = 2, H = 2)

save_plot(qq)


## 2.2 Discrete ====

bar_data <- data.frame(
  x = c(rep(2, 2), rep(4, 4), rep(6, 6), rep(8, 8))
)

a <- ggplot(bar_data, aes(x)) +
  geom_bar(fill = serenity.fill)

bar <- theme_serenity(a + theme_layer)

save_plot(bar)
