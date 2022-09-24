# figure for continuous variable analysis

library(ggplot2)
library(ggpubr)
library(cowplot)
library(CHOIRBM)
library(tidyverse)
library(here)
library(readxl)
library(janitor)

run_logistic <- function(varname) {
  # load data
  survey_data <- read_excel(here("data/survey_data.xlsx")) %>%
    clean_names() %>%
    select(-contains("psy")) %>%
    rename(
      bodymap = bodymap_regions_csv
      , opi_use = opioids_use_from_choir
    ) %>%
    filter(!is.na(bodymap))

  model_output <- comp_choirbm_glm(survey_data, varname
                                   , family = "binomial")
  saveRDS(model_output, here(paste0("tbls/logistic_", varname, ".rds")))
}

lapply(colnames(survey_data)[9:27], run_logistic)

logistic_comparison_plots <- function(w = 7.3, h = 7) {
  log_tbls <- lapply(colnames(survey_data)[9:27], function(x) {
    # load data
    if(!file.exists(paste0("tbls/logistic_", x, ".rds"))) {
      run_logistic(x)
    } else {
      readRDS(here(paste0("tbls/logistic_", x, ".rds")))
    }
  })

  log_df <- do.call(rbind, log_tbls) %>%
    mutate(
      group = ifelse(as.numeric(id) < 200, "Front", "Back")
      , signif = ifelse(
        p.value < 0.05
        , ifelse(
          p.value < 0.001
          , ifelse(
            p.value < 0.0001
            , "***"
            , "**"
          )
          , "*")
        , "ns")
    )

  # just create plots for the average pain intensity and the promis emotional support
  avg_pn_int_plot <- log_df %>%
    filter(term == "pain_intensity_average") %>%
    mutate(
      group = ifelse(as.numeric(id) < 200, "Front", "Back")
      , signif = ifelse(
        p.value < 0.05
        , ifelse(
          p.value < 0.001
          , ifelse(
            p.value < 0.0001
            , "***"
            , "**"
          )
          , "*")
        , "ns")
    ) %>%
    plot_male_choirbm("signif") +
    ggtitle("Average Pain Intensity") +
    ggplot2::theme(legend.position = "bottom"
                   , plot.title = element_text(hjust = 0.5))
    save_plot(
      here("figs/fig6_subfigs/pain_intensity_average_a.tiff")
      , avg_pn_int_plot
      , base_height = h
      , base_width = w
      , dpi = 300
    )
    saveRDS(avg_pn_int_plot, here("figs/fig6_subfigs/pain_intensity_average_a.rds"))

    # emotional support
    promis_emot_supp_plot <- log_df %>%
      filter(term == "promis_emot_support_v2_0") %>%
      mutate(
        group = ifelse(as.numeric(id) < 200, "Front", "Back")
        , signif = ifelse(
          p.value < 0.05
          , ifelse(
            p.value < 0.001
            , ifelse(
              p.value < 0.0001
              , "***"
              , "**"
            )
            , "*")
          , "ns")
      ) %>%
      plot_male_choirbm("signif") +
      ggtitle("PROMIS Emotional Support") +
      ggplot2::theme(legend.position = "bottom"
                     , plot.title = element_text(hjust = 0.5))
      save_plot(
        here("figs/fig6_subfigs/promis_emotional_support_b.tiff")
        , promis_emot_supp_plot
        , base_height = h
        , base_width = w
        , dpi = 300
      )
    saveRDS(promis_emot_supp_plot, here("figs/fig6_subfigs/promis_emotional_support_b.rds"))


  # plot_list <- list()
  # for(te in unique(log_df[["term"]])) {
  #   print(te)
  #   p <- log_df %>%
  #     filter(term == te) %>%
  #     plot_male_choirbm("signif") +
  #     ggtitle(te) +
  #     ggplot2::theme(
  #       legend.position = "top"
  #       , plot.title = element_text(hjust = 0.5)
  #     )
  #   saveRDS(p, here(paste0("figs/", te, ".rds")))
  #   # ggsave(p, here(paste0("figs/", te, ".jpg")), device = "jpeg", dpi = 300)
  #   plot_list[[te]] <- p
  # }
  #
  #
  #
  #
  # %>%
  #   plot_male_choirbm("signif") +
  #   facet_grid(~term + group) +
  #   theme(legend.position = "right")

  lapply(log_tbls, function(x) {
    x %>%
      mutate(
        group = ifelse(as.numeric(id) < 200, "Front", "Back")
        , signif = ifelse(
          p.value < 0.05
          , ifelse(
            p.value < 0.001
            , ifelse(
              p.value < 0.0001
              , "***"
              , "**"
            )
            , "*")
          , "ns")
      ) %>%
      plot_male_choirbm("signif") +
      ggtitle(x[["term"]][1]) +
      ggplot2::theme(legend.position = "top"
                     , plot.title = element_text(hjust = 0.5)) %>%
      saveRDS(here(paste0("figs/", x[["term"]][1], ".rds")))
  })
}
logistic_comparison_plots()

plot_log_subfig <- function(saveme = FALSE, fsize = 12, w = 7.3, h = 9, varname = "promis_pain_interference") {
  # plot a bodymap with the significance levels of each segment
  plot_data <- log_tbl

  fig <- plot_grid(

    , labels = "A"
  )

  # save the figure
  if(saveme) {
    save_plot(
      here("figs/figure-2.svg")
      , fig
      , base_height = h
      , base_width = w
    )
    save_plot(
      here("figs/figure-2.png")
      , fig
      , base_height = h
      , base_width = w
      , dpi = 300
    )
  }

  return(fig)
}


