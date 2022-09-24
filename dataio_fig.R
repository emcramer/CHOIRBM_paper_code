# Function to make the data input and output tables/figure

library(ggplot2)
library(ggpubr)
library(cowplot)
library(CHOIRBM)
library(tidyverse)
library(here)
library(viridis)

data_io_fig <- function(saveme = FALSE, fsize = 12, w = 7.3, h = 9) {

  set.seed(123)

  # generate example input data from the validation data set
  sample_indata <- validation %>%
    select(id, bodymap_regions_csv) %>%
    rename(Identifier = 1, CBM = 2)
  sample_indata <- sample_indata[sample(1:nrow(validation), 500), ] %>%
    mutate(Identifier = paste0("PT#", Identifier)) %>%
    mutate(`Additional Variables` = rep("...", 500))

  # generate example output data from the sample input data
  sample_outdata <- sample_indata[["CBM"]] %>%
    lapply(string_to_map) %>%
    agg_choirbm_list() %>%
    mutate(
      percent = 100 * value / nrow(sample_indata)
      , group = as.factor(group)
    )

  # compress the sample out data for figure...
  sample_outdata_top <- sample_outdata[1:2, ]
  sample_outdata_bot <- sample_outdata[73:74, ]

  sample_outdata_fig <- rbind(
    sample_outdata_top
    , rep("...", ncol(sample_outdata))
    , sample_outdata_bot
  )

  # create tables...
  indata_gg <- ggtexttable(
    head(sample_indata, 5)
    , rows = NULL
    , theme = ttheme(
      base_style = "light"
      , base_size = fsize
    )
  ) %>%
    tab_add_title(
      text = "Input Data"
      , face = "bold"
      , size = fsize + 2
      , vjust = 0.1#-0.5
    )
  outdata_gg <- ggtexttable(
    sample_outdata_fig
    , rows = NULL
    , theme = ttheme(
      base_style = "light"
      , base_size = fsize
    )
  ) %>%
    tab_add_title(
      text = "Formatted Output"
      , face = "bold"
      , size = fsize + 2
      , vjust = 0.1#-0.5
      , family = "sans"
    )

  # create sample out plot
  cbm_plot <- plot_male_choirbm(sample_outdata, "percent") +
    # scale_fill_viridis(option = "H") +
    scale_fill_gradient(low = "#FFDE92", high = "#a100b3") + # light yellow to purple
    ggtitle("Generated Body Map") +
    labs(fill = "Percent Endorsement") +
    theme(
      plot.title = element_text(hjust = 0.5, family = "sans")
      , legend.position = "right"
      , text = element_text(size = fsize, family = "sans")
    )
  leg <- get_legend(cbm_plot)

  # prow1 <- plot_grid(
  #   indata_gg
  #   , outdata_gg
  #   , ncol = 2
  #   , rel_widths = c(0.6, 0.4)
  #   , labels = LETTERS[1:2]
  #   , label_size = 12
  # )
  #
  # prow2 <- plot_grid(
  #   cbm_plot + theme(legend.position = "none", aspect.ratio = 1)
  #   , leg
  #   , NULL
  #   , labels = c("C", "", "")
  #   , rel_widths = c(0.75, 0.25, 0.25)
  #   , label_size = 12
  # )

  p1 <- plot_grid(
    indata_gg
    , outdata_gg
    , ncol = 1
    , labels = "AUTO"
  )
  p2 <- plot_grid(
    cbm_plot + theme(legend.position = "bottom")
    , labels = "C")

  p3 <- plot_grid(
    p1
    , p2
    , nrow = 1
    , rel_widths = c(1, 1.4)
  )

  fig <- plot_grid(
    p3
    , NULL
    , ncol = 1
    , rel_heights = c(1, 0.05)
  )

  # fig <- plot_grid(
  #   prow1
  #   , NULL
  #   , prow2
  #   , ncol = 1
  #   , rel_heights = c(0.25, 0.2, 1)
  # )

  # save the figure
  if(saveme) {
    save_plot(
      here("figs/figure-1.svg")
      , fig
      , base_height = h
      , base_width = w
    )
    save_plot(
      here("figs/figure-1.png")
      , fig
      , base_height = h
      , base_width = w
      , dpi = 300
    )
    save_plot(
      here("figs/figure-1.tiff")
      , fig
      , base_height = h
      , base_width = w
      , dpi = 300
    )
    save_plot(
      here("figs/fig2_subfigs/indata.tiff")
      , indata_gg
      , base_height = h
      , base_width = w
      , dpi = 300
    )
    save_plot(
      here("figs/fig2_subfigs/outdata.tiff")
      , outdata_gg
      , base_height = h
      , base_width = w
      , dpi = 300
    )
    save_plot(
      here("figs/fig2_subfigs/sample_plot.tiff")
      , cbm_plot
      , base_height = h
      , base_width = w
      , dpi = 300
    )
  }

  return(fig)
}

# testing
data_io_fig(saveme = TRUE, w = 7.3, h = 7, fsize = 9)
