# Function to illustrate inter-group comparison
# comparing male and  female bodymaps

library(ggplot2)
library(ggpubr)
library(cowplot)
library(CHOIRBM)
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(viridis)

gender_comparison <- function(saveme = FALSE, fsize = 12, w = 7.3, h = 9) {
  # load data
  survey_data <- read_excel(here("data/survey_data.xlsx")) %>%
    clean_names() %>%
    select(-contains("psy")) %>%
    rename(
      bodymap = bodymap_regions_csv
      , opi_use = opioids_use_from_choir
    ) %>%
    filter(!is.na(bodymap))

  # separate data into m/f
  male_data <- survey_data %>%
    filter(gender == "Male")

  female_data <- survey_data %>%
    filter(gender == "Female")

  male_bodymap_list <- lapply(male_data[["bodymap"]], string_to_map)
  male_bodymap_df <- agg_choirbm_list(male_bodymap_list) %>%
    mutate(prop = value / nrow(male_data))

  female_bodymap_list <- lapply(female_data[["bodymap"]], string_to_map)
  female_bodymap_df <- agg_choirbm_list(female_bodymap_list) %>%
    mutate(prop = value / nrow(female_data))

  # compare male and female cbms
  chi_res <- comp_choirbm_chi(
    list("male" = male_bodymap_df, "female" = female_bodymap_df)
    , method = "bonferroni"
  )
  write_csv(chi_res,  here("tbls/gender_chi_res.csv"))

  # z_res <- comp_choirbm_ztest(
  #   df1 = male_data
  #   , df2 = female_data
  #   , tail = "left"
  #   , p.method = "bonferroni"
  # )
  # write_csv(z_res,  here("tbls/gender_z_res.csv"))

  # visualization
  male_plot <- male_bodymap_df %>%
    mutate(perc = 100 * value / nrow(male_data)) %>%
    plot_male_choirbm("perc") +
    # scale_fill_viridis(option = "H") +
    scale_fill_gradient(low = "#FFDE92", high = "#a100b3") + # light yellow to purple
    # scale_fill_gradient2(low = "#FFA538", mid = "#FFA538", high = "#4D000F") + # orange-red-black
    theme(
      legend.position = "bottom"
      , plot.title = element_text(hjust = 0.5)
    ) +
    labs(title = "Male CBM", fill = "Percent Endorsement")
  female_plot <- female_bodymap_df %>%
    mutate(perc = 100 * value / nrow(female_data)) %>%
    plot_female_choirbm("perc") +
    # scale_fill_viridis(option = "H") +
    scale_fill_gradient(low = "#FFDE92", high = "#a100b3") + # light yellow to purple
    theme(legend.position = "bottom"
          , legend.box.margin = margin(b = 5, unit = "pt")
          , plot.title = element_text(hjust = 0.5)
    ) +
    labs(
      title = "Female CBM"
      , fill = "Percent Endorsement")

  md <- male_bodymap_df %>%
    mutate(perc = 100 * value / nrow(male_data))
  fd <- female_bodymap_df %>%
    mutate(perc = 100 * value / nrow(female_data))
  td_plot <- select(md, id, value, group) %>%
    mutate(perc_diff = fd[["perc"]] - md[["perc"]]) %>%
    plot_male_choirbm("perc_diff") +
    # scale_fill_viridis(option = "H") +
    scale_fill_gradient(low = "#FFDE92", high = "#a100b3") + # light yellow to purple
    theme(
      legend.position = "bottom"
      , plot.title = element_text(hjust = 0.5)
    ) +
    labs(title = "Gender Differences on the CBM"
         , fill = "Female % Endorsement - Male % Endorsement")

  # figure generation
  prow1 <- plot_grid(
    male_plot + theme(legend.position = "none")
    , female_plot + theme(legend.position = "none")
    , nrow = 1
    , labels = LETTERS[1:2]
  )

  leg <- get_legend(
    female_plot
  )

  fig <- plot_grid(
    prow1
    , leg
    , plot_grid(
      td_plot + theme(legend.position = "none")
      , NULL
      , labels = c("C", "")
      , rel_widths = c(0.5, 0.5)
    )
    , get_legend(td_plot)
    , NULL
    , rel_heights = c(0.9, 0.1, 0.9, 0.1, 0.05)
    , ncol = 1
  )

  # save the figure
  if(saveme) {
    save_plot(
      here("figs/figure-5.svg")
      , fig
      , base_height = h
      , base_width = w
    )
    save_plot(
      here("figs/figure-5.png")
      , fig
      , base_height = h
      , base_width = w
      , dpi = 300
    )
    save_plot(
      here("figs/figure-5.tiff")
      , fig
      , base_height = h
      , base_width = w
      , dpi = 300
    )
    save_plot(
      here("figs/fig5_subfigs/male.tiff")
      , male_plot
      , base_height = h
      , base_width = w
      , dpi = 300
    )
    save_plot(
      here("figs/fig5_subfigs/female.tiff")
      , female_plot
      , base_height = h
      , base_width = w
      , dpi = 300
    )
    # save_plot(
    #   here("figs/fig5_subfigs/legend.tiff")
    #   , leg
    #   , base_height = h
    #   , base_width = w
    #   , dpi = 300
    # )
    save_plot(
      here("figs/fig5_subfigs/mf_diff.tiff")
      , td_plot
      , base_height = h
      , base_width = w
      , dpi = 300
    )
  }
  return(fig)
}
gender_comparison(saveme = TRUE, fsize = 8, w = 7.3, h = 7)
