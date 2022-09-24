library(ggplot2)
library(ggpubr)
library(cowplot)
library(CHOIRBM)
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(png)

labeled_cbms <- function(saveme = FALSE, fsize = 12, w = 7.5, h = 8.75) {
  male_cbm <- readPNG("figs/cbm-male-labeled.png")
  female_cbm <- readPNG("figs/cbm-female-labeled.png")

  p_m <- ggdraw() +
    draw_image(male_cbm)
  p_f <- ggdraw() +
    draw_image(female_cbm)

  fig <- plot_grid(
    p_m
    , p_f
    , labels = c("A", "B")
    , nrow = 1
    , label_size = fsize
  )

  if(saveme) {
    save_plot(
      here("figs/labeled-cbms.svg")
      , fig
      , base_height = h
      , base_width = w
    )
    save_plot(
      here("figs/labeled-cbms.png")
      , fig
      , base_height = h
      , base_width = w
      , dpi = 300
    )
    save_plot(
      here("figs/labeled-cbms.tiff")
      , fig
      , base_height = h
      , base_width = w
      , dpi = 300
    )
  }

  return(fig)
}
labeled_cbms(saveme = TRUE, h = 4.5)
