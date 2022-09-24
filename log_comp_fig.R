# figure for continuous variable analysis

library(ggplot2)
library(ggpubr)
library(cowplot)
library(CHOIRBM)
library(tidyverse)
library(here)
library(readxl)
library(janitor)

logistic_comparison_figure <- function(saveme = FALSE, fsize = 8, w = 7.5, h = 4.5) {
  avg_pn_mod <- readRDS(here(paste0("tbls/logistic_pain_intensity_average.rds"))) %>%
    select(id, estimate, p.value) %>%
    mutate(
      group = factor(ifelse(as.numeric(id) < 200, "Front", "Back"), levels = c("Front", "Back"))
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
  avg_pn_mod %>%
    select(-group) %>%
    write_csv(here("tbls/avg_pn_mod.csv"))

  promis_es_mod <- readRDS(here(paste0("tbls/logistic_promis_emot_support_v2_0.rds"))) %>%
    select(id, estimate, p.value) %>%
    mutate(
      group = factor(ifelse(as.numeric(id) < 200, "Front", "Back"), levels = c("Front", "Back"))
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
  promis_es_mod %>%
    select(-group) %>%
    write_csv(here("tbls/promis_es_mod.csv"))

  avg_pn <- plot_male_choirbm(avg_pn_mod, "signif") +
    ggtitle("Average Pain Intensity") +
    ggplot2::theme(
      legend.position = "top"
      , plot.title = element_text(size = fsize + 4, hjust = 0.5)
    )

  promis_es <- plot_male_choirbm(promis_es_mod, "signif") +
    ggtitle("PROMIS Emotional Support") +
    ggplot2::theme(
      legend.position = "top"
      , plot.title = element_text(size = fsize + 4, hjust = 0.5)
    )

  leg <- get_legend(promis_es + labs(fill = "Significance Level"))
  fig <- plot_grid(
    plot_grid(
      avg_pn + theme(legend.position = "none")
      , promis_es + theme(legend.position = "none")
      , labels = LETTERS[1:2]
      , label_size = fsize
      , nrow = 1
    )
    , leg
    , ncol = 1
    , rel_heights = c(0.95, 0.05)
  )

  if(saveme) {
    save_plot(
      here("figs/log_reg.svg")
      , fig
      , base_height = h
      , base_width = w
    )
    save_plot(
      here("figs/log_reg.png")
      , fig
      , base_height = h
      , base_width = w
      , dpi = 300
    )
    save_plot(
      here("figs/log_reg.tiff")
      , fig
      , base_height = h
      , base_width = w
      , dpi = 300
    )
  }

  saveRDS(fig, here("figs/log_reg.rds"))
  return(fig)
}
logistic_comparison_figure(saveme = TRUE)
