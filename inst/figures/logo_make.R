rm(list = ls())
library(ggplot2)
library(png)
library(grid)
library(hexSticker)

set.seed(123)
col_border <- '#a1d99b'
col_bg <- "#F0F9E8"
col_text <- 'steelblue'
url_color <- col_text

bsas <- readPNG('inst/figures/BAS4.png')
bsas_2 <- rasterGrob(bsas,  x = 0.5, y = 0.61,
                     interpolate = TRUE)
gg <- ggplot() +
  annotation_custom(bsas_2) +
  theme_void()

sticker(gg, package="rBAS", p_size = 16,
        p_y = 1.5,
        s_x = 1,
        s_y = 0.8,
        s_width = 1.4,
        s_height = 1.5,
        h_fill = col_bg,
        h_color = col_border,
        #p_family = "Aller_Lt",
        filename="inst/figures/rBAS.png",
        spotlight = TRUE,
        l_x = 1.1,
        l_y = 0.45,
        l_alpha = 0.3,
        #p_color = col_text,
        p_color = "#4E94B5",
        #url="jywang2016.github.io",
        #u_size = 5,
        u_color = url_color)

sticker(gg, package="rBAS", p_size = 16,
        p_y = 1.5,
        s_x = 1,
        s_y = 0.8,
        s_width = 1.6,
        s_height = 1.6,
        h_fill = col_bg,
        h_color = col_border,
        #p_family = "Aller_Lt",
        filename="inst/figures/rBAS.png",
        spotlight = TRUE,
        l_x = 1.1,
        l_y = 0.45,
        l_alpha = 0.3,
        #p_color = col_text,
        p_color = "#4E94B5",
        #url="jywang2016.github.io",
        #u_size = 5,
        u_color = url_color)

RColorBrewer::brewer.pal(n = 12, name = 'GnBu')

