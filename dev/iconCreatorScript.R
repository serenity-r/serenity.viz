set.seed(4)

img_repo <- "inst/www/img"

serenity.fill <- "#54585E"
theme_serenity <- function(ggobj, X = 0, Y = 0, W = 10, H = 10) {
  ggobj +
    scale_x_continuous(limits = c(X, W+1)) +
    scale_y_continuous(limits = c(Y, H+1)) +
    coord_cartesian(xlim = c(X, W), ylim = c(Y, H))
}
save_plot <- function(ggobj, data_dim = 0) {
  W = 8
  H = 8
  geom_name <- paste0(rlang::quo_name(enquo(ggobj)), "_", data_dim)
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

# __ geom_point ----

a <- ggplot(mapping = aes(x = 5, y = 5)) +
  geom_point(size = 25)

point <- theme_serenity(a + theme_layer)

save_plot(point)

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

# a <- ggplot(ribbon_data, aes(x, ymin)) +
#   geom_blank()
#
# blank <- theme_serenity(a + theme_layer)
#
# save_plot(blank)

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
  geom_polygon(fill = serenity.fill, alpha = 0.5)

polygon <- theme_serenity(a + theme_layer)

save_plot(polygon)

# __ geom_rect ----

rect_data <- data.frame(
  x = c(1, 1, 9, 9),
  y = c(3, 7, 7, 3)
)

a <- ggplot(rect_data, aes(x, y)) +
  geom_polygon(fill = serenity.fill, alpha = 0.5) +
  geom_point(size = 25, colour = "black")

rect <- theme_serenity(a + theme_layer)

save_plot(rect)

# __ geom_tile ----

tile_data <- data.frame(
  x = 5, y = 5, width = 8, height = 4
)

a <- ggplot(tile_data, aes(x, y, width = width, height = height)) +
  geom_tile(fill = serenity.fill, alpha = 0.5) +
  geom_segment(aes(x = 1, y = 5, xend = 9, yend = 5),
               colour = "#3366FF", size = 10) +
  geom_segment(aes(x = 5, y = 3, xend = 5, yend = 7),
               colour = "#3366FF", size = 10) +
  geom_point(size = 25, fill = "black", shape = 21)

tile <- theme_serenity(a + theme_layer)

save_plot(tile)

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

data_dim <- 1

## 2.1 Continuous ====

# __ geom_density ----

x <- seq(from = 0, to = 10, length.out = 100)

a <- ggplot() +
  geom_line(aes(x = x, y = 30*dnorm(x, mean = 5, sd = 1.5)),
            size = 6)

density <- theme_serenity(a + theme_layer)

save_plot(density, data_dim)

# __ geom_dotplot ----

dotplot_data <- data.frame(
  x = c(rep(1, 2), rep(3, 6), rep(5, 4), rep(7, 1), rep(9, 5))
)

a <- ggplot(dotplot_data, aes(x)) +
  geom_dotplot(binwidth = 2, dotsize = 0.8, stackratio = 1.0)

dotplot <- theme_serenity(a + theme_layer)

save_plot(dotplot, data_dim)

# __ geom_freqpoly ----

freqpoly_data <- data.frame(
  x = c(rep(2, 3), rep(3, 7), rep(5, 6), rep(7, 1), rep(9, 5))
)

a <- ggplot(freqpoly_data, aes(x)) +
  geom_freqpoly(binwidth = 2, size = 6)

freqpoly <- theme_serenity(a + theme_layer)

save_plot(freqpoly, data_dim)

# __ geom_histogram ----

histogram_data <- data.frame(
  x = rnorm(1000, mean = 5, sd = 2)
)

a <- ggplot(histogram_data, aes(x)) +
  geom_histogram(binwidth = 0.8, fill = serenity.fill)

histogram <- theme_serenity(a + theme_layer, H = 200)

save_plot(histogram, data_dim)

# __ geom_qq ----

qq_data <- data.frame(
  x = rnorm(40)
)

a <- ggplot(qq_data, aes(sample = x)) +
  geom_qq(size = 5)

qq <- theme_serenity(a + theme_layer, X = -2, Y = -2, W = 2, H = 2)

save_plot(qq, data_dim)


## 2.2 Discrete ====

# __ geom_bar ----

bar_data <- data.frame(
  x = c(rep(2, 2), rep(4, 4), rep(6, 6), rep(8, 8))
)

a <- ggplot(bar_data, aes(x)) +
  geom_bar(fill = serenity.fill)

bar <- theme_serenity(a + theme_layer)

save_plot(bar, data_dim)

## 3. Two Variables ====

data_dim <- 2

## 3.1 Continuous X, Continuous Y ====

# __ geom_label ----

label_data <- data.frame(
  x = c(3, 4, 7),
  y = c(7, 3, 5),
  label = c("A", "B", "C")
)

a <- ggplot(label_data, aes(x, y)) +
  geom_label(aes(label = label), size = 70,
             label.padding = unit(1.8, "lines"),
             label.r = unit(0.5, "lines"),
             label.size = 1.8)

label <- theme_serenity(a + theme_layer)

save_plot(label, data_dim)

# __ geom_point ----

mean <- c(5, 5)
s <- 1.7
s2 <- s*s
S <- matrix(s2*c(1, 0.8, 0.8, 1), 2, 2)
Z <- mvtnorm::rmvnorm(30, mean, S)
point_data <- data.frame(
  x = Z[,1],
  y = Z[,2]
)

a <- ggplot(point_data, aes(x,y)) +
  geom_point(size = 6)

point <- theme_serenity(a + theme_layer)

save_plot(point, data_dim)

# __ geom_area ----

a <- ggplot(ribbon_data, aes(x = x, y = ymax)) +
  geom_area()

area <- theme_serenity(a + theme_layer)

save_plot(area, data_dim)

# __ geom_quantile ----

S <- matrix(s2*c(0.5, 0.2, 0.2, 4), 2, 2)
Z <- mvtnorm::rmvnorm(3000, mean, S)
quantile_data <- data.frame(
  x = Z[,1],
  y = Z[,2]
)

a <- ggplot(quantile_data, aes(x,y)) +
  geom_quantile(size = 5)

quantile <- theme_serenity(a + theme_layer)

save_plot(quantile, data_dim)

# __ geom_rug ----

S <- matrix(s2*c(1, 0.2, 0.2, 1), 2, 2)
Z <- mvtnorm::rmvnorm(100, mean, S)
rug_data <- data.frame(
  x = Z[,1],
  y = Z[,2]
)

a <- ggplot(rug_data, aes(x,y)) +
  geom_rug(length = unit(0.1, "npc"), size = 1.2)

rug <- theme_serenity(a + theme_layer)

save_plot(rug, data_dim)

# __ geom_smooth ----

S <- matrix(s2*c(0.5, 0.2, 0.2, 4), 2, 2)
Z <- mvtnorm::rmvnorm(200, mean, S)
smooth_data <- data.frame(
  x = Z[,1],
  y = Z[,2]
)

a <- ggplot(smooth_data, aes(x,y)) +
  geom_smooth(size = 5, fill = serenity.fill, alpha = 0.5)

smooth <- theme_serenity(a + theme_layer, X = 2, W = 8)

save_plot(smooth, data_dim)

# __ geom_raster ----

S <- data.frame(
  x = c(1, 1, 2, 2),
  y = c(1, 2, 1, 2),
  z = c(1, 0, 0, 1)
)

a <- ggplot(S, aes(x,y, fill = z)) +
  geom_raster(show.legend = FALSE)

raster <- theme_serenity(a + theme_layer, X = 0.5, Y = 0.5, W = 2.5, H = 2.5)

save_plot(raster, data_dim)

# __ geom_text ----

a <- ggplot(label_data, aes(x,y)) +
  geom_text(aes(label = label), size = 70)

text <- theme_serenity(a + theme_layer) +
  scale_x_continuous(breaks = seq(0, 10, 2.5))

save_plot(text, data_dim)

## 3.2 Discrete X, Continuous Y

# __ geom_boxplot ----

boxplot_data <- data.frame(
  x = c(3, 7),
  lower = c(2, 3),
  middle = c(3, 5),
  upper = c(7, 6),
  ymin = c(1, 1.5),
  ymax = c(9, 7)
)

a <- ggplot(boxplot_data, aes(x)) +
  geom_boxplot(stat = "identity",
               aes(ymin = ymin,
                   lower = lower,
                   middle = middle,
                   upper = upper,
                   ymax = ymax,
                   group = x),
               size = 4,
               width = 2.0) +
  geom_point(aes(x = c(7, 7), y = c(8, 9)), size = 6)

boxplot <- theme_serenity(a + theme_layer)

save_plot(boxplot, data_dim)

# __ geom_violin ----

x <- seq(from = 0, to = 10, length.out = 100)

y1 <- dnorm(x, mean = 3, sd = 0.5) + dnorm(x, mean = 7, sd = 1)
z1 <- y1/(x[2]*sum(y1))
CDF1 <- data.frame(x = x, p = x[2]*cumsum(z1))

y2 <- dnorm(x, mean = 4, sd = 0.5) + dnorm(x, mean = 6, sd = 1)
z2 <- y2/(x[2]*sum(y2))
CDF2 <- data.frame(x = x, p = x[2]*cumsum(z2))

violin_data <- data.frame(
  x = c(rep(3, 100), rep(7, 100)),
  y = c(sapply(runif(100), function(x) { CDF1$x[which.min(CDF1$p < x)] }),
        sapply(runif(100), function(x) { CDF2$x[which.min(CDF2$p < x)] }))
)

a <- ggplot(violin_data, aes(x, y)) +
  geom_violin(trim = FALSE, aes(group = x), fill = serenity.fill)

violin <- theme_serenity(a + theme_layer)

save_plot(violin, data_dim)

## 3.2 Continuous Bivariate Distribution ----

# __ geom_bin2d ----

mean <- c(5, 5)
s <- 1.5
s2 <- s*s
S <- matrix(s2*c(1, 0.7, 0.7, 1), 2, 2)
Z <- mvtnorm::rmvnorm(3000, mean, S)
bin2d_data <- data.frame(
  x = Z[,1],
  y = Z[,2]
)

a <- ggplot(bin2d_data, aes(x, y)) +
  geom_bin2d(breaks = seq(0, 10, 1.25), drop = FALSE, show.legend = FALSE)

bin2d <- theme_serenity(a + theme_layer) +
  coord_cartesian(xlim = c(0.45, 9.45), ylim = c(0.45, 9.45))

save_plot(bin2d, data_dim)

# __ geom_density2d ----

a <- ggplot(bin2d_data, aes(x, y)) +
  geom_density2d(h = 2.5, size = 3)

density2d <- theme_serenity(a + theme_layer)

save_plot(density2d, data_dim)

# __ geom_hex ----

hex_data <- rbind(bin2d_data,
                  expand.grid(x = seq(0, 10, 1), y = seq(0, 10, 1)))

a <- ggplot(hex_data, aes(x, y)) +
  geom_hex(binwidth = 1.25, show.legend = FALSE)

hex <- theme_serenity(a + theme_layer) +
  coord_cartesian(xlim = c(1, 9), ylim = c(1, 9))

save_plot(hex, data_dim)

## 3.3 Continuous Function ====

# __ geom_line ----

a <- ggplot(ribbon_data, aes(x = x, y = ymax)) +
  geom_line(size = 6)

line <- theme_serenity(a + theme_layer)

save_plot(line, data_dim)

# __ geom_step ----

a <- ggplot(ribbon_data, aes(x = x, y = ymax)) +
  geom_step(size = 6)

step <- theme_serenity(a + theme_layer)

save_plot(step, data_dim)

## 3.4 Visualizing Error ====

error_data <- data.frame(
  x = c(2, 5, 8),
  y = c(3.5, 5.5, 7),
  ymin = c(1, 4, 5),
  ymax = c(6, 7, 9)
)

# __ geom_crossbar ----

a <- ggplot(error_data, aes(x, y)) +
  geom_crossbar(aes(ymin = ymin, ymax = ymax), size = 4, fatten = 1.5, width = 2)

crossbar <- theme_serenity(a + theme_layer)

save_plot(crossbar, data_dim)

# __ geom_errorbar ----

a <- ggplot(error_data, aes(x, y)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), size = 4, width = 2)

errorbar <- theme_serenity(a + theme_layer)

save_plot(errorbar, data_dim)

# __ geom_linerange ----

a <- ggplot(error_data, aes(x, y)) +
  geom_linerange(aes(ymin = ymin, ymax = ymax), size = 4, width = 2)

linerange <- theme_serenity(a + theme_layer)

save_plot(linerange, data_dim)

# __ geom_pointrange ----

a <- ggplot(error_data, aes(x, y)) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax), size = 4, width = 2)

pointrange <- theme_serenity(a + theme_layer)

save_plot(pointrange, data_dim)

## A. Create geom icon css file ====

fileConn <- file(here::here("inst", "www", "css", "plot_icons.css"))

icons <- list.files(here::here("inst", "www", "img"), pattern = "geom.*?svg", full.names = TRUE)
css <- sapply(icons, function(x) {
  plot <- paste(strsplit(strsplit(strsplit(x, "/")[[1]][length(strsplit(x, "/")[[1]])], ".svg")[[1]], "_")[[1]], collapse = "-")
  encoding <- htmltools::urlEncodePath(readChar(x, file.info(x)$size))
  paste0('.plot-icon.', plot, ' {\n  background-image: url("data:image/svg+xml,', encoding, '");\n  background-size: 100%;\n}\n')
}, USE.NAMES = FALSE)

writeLines(css, fileConn)
close(fileConn)

## B.1 Miscellaneous ====

save_misc_plot <- function(ggobj, filename, W = 8, H = 8) {
  ggsave(file = here::here(paste0(img_repo, "/", filename, ".svg")), plot = ggobj, width = W, height = H)
  system2("svgo", args = paste0(img_repo, "/", filename, ".svg --disable=minifyStyles,convertStyleToAttrs --precision=0"))
}

### __ linejoin ====

# Zoom function
# "260.00 268.00 40 40"
linejoin_zoom <- function(f, viewBox = "278.00 280.50 20 15") {
  f <- here::here("inst", "www", "img", paste0(f, ".svg"))
  x <- readLines(f)
  y <- gsub('viewBox=\"0 0 576 576\"', paste0('viewBox=\"', viewBox, '\"'), x)
  cat(y, file=f, sep="\n")
}

angle <- 25

# linejoin round
a <- ggplot(mapping = aes(x = 1, y = 1, xend = 1+1e-8, yend = 1)) +
  geom_segment(lineend = "butt", linejoin = "round",
               size = 4, arrow = arrow(angle = angle, length = unit(0.3, "inches"))) +
  xlim(0.5, 1.5) +
  ylim(0.5, 1.5)

linejoin_round <- a + theme_void()

save_misc_plot(linejoin_round, "misc_linejoin_round")
linejoin_zoom("misc_linejoin_round")

 # linejoin mitre
a <- ggplot(mapping = aes(x = 1, y = 1, xend = 1+1e-8, yend = 1)) +
  geom_segment(lineend = "butt", linejoin = "mitre",
               size = 4, arrow = arrow(angle = angle, length = unit(0.3, "inches"))) +
  xlim(0.5, 1.5) +
  ylim(0.5, 1.5)

linejoin_mitre <- a + theme_void()

save_misc_plot(linejoin_mitre, "misc_linejoin_mitre")
linejoin_zoom("misc_linejoin_mitre")

# linejoin bevel
a <- ggplot(mapping = aes(x = 1, y = 1, xend = 1+1e-8, yend = 1)) +
  geom_segment(lineend = "butt", linejoin = "bevel",
               size = 4, arrow = arrow(angle = angle, length = unit(0.3, "inches"))) +
  xlim(0.5, 1.5) +
  ylim(0.5, 1.5)

linejoin_bevel <- a + theme_void()

save_misc_plot(linejoin_bevel, "misc_linejoin_bevel")
linejoin_zoom("misc_linejoin_bevel")

### __ lineend ====

# Zoom function
# "260.00 268.00 40 40"
lineend_zoom <- function(f, viewBox = "278.00 280.50 20 15") {
  f <- here::here("inst", "www", "img", paste0(f, ".svg"))
  x <- readLines(f)
  y <- gsub('viewBox=\"0 0 576 576\"', paste0('viewBox=\"', viewBox, '\"'), x)
  cat(y, file=f, sep="\n")
}

ref_line_colour <- "#660000"
ref_line_size <- 1.0

# lineend butt
a <- ggplot() +
  geom_segment(mapping = aes(x = 1, y = 0, xend = 1, yend = 2), size = ref_line_size,
               colour = ref_line_colour) +
  geom_segment(mapping = aes(x = 1, y = 1, xend = 2, yend = 1),
               lineend = "butt", size = 4) +
  scale_x_continuous(oob = scales::oob_keep, limits = c(0.5, 1.5)) +
  scale_y_continuous(oob = scales::oob_keep, limits = c(0.5, 1.5))

lineend_butt <- a + theme_void()

save_misc_plot(lineend_butt, "misc_lineend_butt")
lineend_zoom("misc_lineend_butt")

# lineend square
a <- ggplot() +
  geom_segment(mapping = aes(x = 1, y = 0, xend = 1, yend = 2), size = ref_line_size,
               colour = ref_line_colour) +
  geom_segment(mapping = aes(x = 1, y = 1, xend = 2, yend = 1),
               lineend = "square", size = 4) +
  scale_x_continuous(oob = scales::oob_keep, limits = c(0.5, 1.5)) +
  scale_y_continuous(oob = scales::oob_keep, limits = c(0.5, 1.5))

lineend_square <- a + theme_void()

save_misc_plot(lineend_square, "misc_lineend_square")
lineend_zoom("misc_lineend_square")

# lineend round
a <- ggplot() +
  geom_segment(mapping = aes(x = 1, y = 0, xend = 1, yend = 2), size = ref_line_size,
               colour = ref_line_colour) +
  geom_segment(mapping = aes(x = 1, y = 1, xend = 2, yend = 1),
               lineend = "round", size = 4) +
  scale_x_continuous(oob = scales::oob_keep, limits = c(0.5, 1.5)) +
  scale_y_continuous(oob = scales::oob_keep, limits = c(0.5, 1.5))

lineend_round <- a + theme_void()

save_misc_plot(lineend_round, "misc_lineend_round")
lineend_zoom("misc_lineend_round")

## B.2 Create misc icon css file ====

fileConn <- file(here::here("inst", "www", "css", "misc_icons.css"))

icons <- list.files(here::here("inst", "www", "img"), pattern = "misc.*?svg", full.names = TRUE)
css <- sapply(icons, function(x) {
  parts <- strsplit(strsplit(strsplit(x, "/")[[1]][length(strsplit(x, "/")[[1]])], ".svg")[[1]], "_")[[1]]
  type <- parts[2]
  subtype <- parts[3]
  encoding <- htmltools::urlEncodePath(readChar(x, file.info(x)$size))
  paste0('.', type, '-icon.', subtype, ' {\n  background-image: url("data:image/svg+xml,', encoding, '");\n  background-size: 100%;\n  background-repeat: no-repeat;\n}\n')
}, USE.NAMES = FALSE)

writeLines(css, fileConn)
close(fileConn)
