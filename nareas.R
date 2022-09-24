library(ggplot2)
library(ggpubr)
library(cowplot)
library(CHOIRBM)
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(png)

nareas_figure <- function(saveme = FALSE, fsize = 12, w = 7.5, h = 8.75) {
  # load data
  cbms <- read_excel(here("data/survey_data.xlsx")) %>%
    clean_names() %>%
    select(bodymap_regions_csv) %>%
    na.omit() %>%
    pull(var = 1)

  p1 <- plot_nareas_histogram(cbms, binwidth = 1, color = "white") +
    scale_x_continuous(
      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 74)
      , labels = c(0, 10, 20, 30, 40, 50, 60, 70, 74)
    ) +
    theme(
      plot.title = element_text(size = fsize + 4)
      , axis.title = element_text(size = fsize + 2)
      , axis.text = element_text(size = fsize)
      , text = element_text(family = "sans")
    )

  #+ labs(title = "")
  # p2 <- plot_nareas_histogram(cbms, bins = 10) + labs(title = "")
  # p3 <- plot_nareas_histogram(cbms, binwidth = 2) + labs(title = "")

  # fig <- plot_grid(
  #   p1
  #   , p2
  #   , p3
  #   , labels = LETTERS[1:3]
  #   , label_size = fsize
  #   , nrow = 1
  # )
  fig <- p1

  if(saveme) {
    save_plot(
      here("figs/nareas.svg")
      , fig
      , base_height = h
      , base_width = w
    )
    save_plot(
      here("figs/nareas.png")
      , fig
      , base_height = h
      , base_width = w
      , dpi = 300
    )
    save_plot(
      here("figs/nareas.tiff")
      , fig
      , base_height = h
      , base_width = w
      , dpi = 300
    )
  }

  return(fig)
}
nareas_figure(saveme = TRUE, fsize = 8, w = 7.5, h = 4.5)
