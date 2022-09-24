# plotting the co-occurrence
library(ggplot2)
library(ggpubr)
library(cowplot)
library(CHOIRBM)
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(viridis)

# run this to generate the co-occurrence matrix the first time
gen_coorccurrence_matrix <- function() {
  survey_data <- read_excel(here("data/survey_data.xlsx")) %>%
    clean_names() %>%
    select(-contains("psy")) %>%
    rename(
      bodymap = bodymap_regions_csv
      , opi_use = opioids_use_from_choir
    ) %>%
    filter(!is.na(bodymap))
  con_mat <- comp_cooccurrence(survey_data)
  saveRDS(con_mat, here("figs/co-occurrence.rds"))
  return(con_mat)
}

cooccurrence <- function(saveme = FALSE, fsize = 8, w = 7.5, h = 4.5) {
  # load data
  if(!file.exists(here("figs/co-occurrence.rds"))) {
    con_mat <- gen_coorccurrence_matrix()
  } else {
    con_mat <- readRDS(here("figs/co-occurrence.rds"))
  }

  con_mat <- con_mat %>%
    rename(cooccurrence = concurrence)

  top10 <- con_mat %>%
    # rename(cooccurrence = concurrence) %>%
    arrange(desc(cooccurrence)) %>%
    slice(1:20) %>%
    filter(row_number() %% 2 == 1) %>%
    dplyr::rename(`Location 1` = 1, `Location 2` = 2, `Co-occurrence` = 3)
  write_csv(top10, here("tbls/con_mat_t10.csv"))

  # generate the plot and style it accordingly
  p <- plot_cooccurrence(con_mat = con_mat) +
    labs(title = "Co-occurrence matrix", fill = "Co-occurrence") +
    # scale_fill_viridis(option = "H") +
    scale_fill_gradient(low = "#FFDE92", high = "#a100b3") + # light yellow to purple
    theme(
      legend.position = "bottom"
      , plot.title = element_text(hjust = 0.5, size = fsize + 4)
      , axis.text = element_text(size = fsize)
      , axis.text.x.bottom = element_text(hjust = 0.5, vjust = 0.0)
    )

  fig <- plot_grid(
    p
  )

  # save the figure
  if(saveme) {
    save_plot(
      here("figs/figure-7.svg")
      , fig
      , base_height = h
      , base_width = w
    )
    save_plot(
      here("figs/figure-7.png")
      , fig
      , base_height = h
      , base_width = w
      , dpi = 300
    )
    save_plot(
      here("figs/figure-7.tiff")
      , fig
      , base_height = h
      , base_width = w
      , dpi = 300
    )
  }

  return(fig)
}

cooccurrence(saveme = TRUE, fsize = 8, h = 7.5)
