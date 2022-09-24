# Format data for easier visualization with CHOIRBM pipeline

library(readxl)
library(tidyverse)
library(here)
library(CHOIRBM)

raw_data <- read_excel(here("data/cbm-validation-20180730.xlsx")) %>%
  mutate(gender = ifelse(gender == 1, "Male", "Female")) %>%
  mutate(id = paste0(name_first, last_name))

# separate into male and female data sets for processing, then recombine
male_data <- raw_data %>%
  filter(gender == "Male") %>%
  select(id, gender, contains("pain_location_male__"))
colnames(male_data) <- gsub("b", "2",
                            gsub("f", "1",
                                 gsub("pain_location_male___", "", colnames(male_data))))
male_data2 <- male_data %>%
  pivot_longer(cols = -c(id, gender), names_to = "area", values_to = "present") %>%
  filter(present > 0) %>%
  select(-present) %>%
  group_by(id) %>%
  summarise(bodymap = toString(area), gender = toString(unique(gender))) %>%
  ungroup()

# female
female_data <- raw_data %>%
  filter(gender == "Female") %>%
  select(id, gender, contains("pain_location_female___"))
colnames(female_data) <- gsub("b", "2",
                            gsub("f", "1",
                                 gsub("pain_location_female___", "", colnames(female_data))))
female_data2 <- female_data %>%
  pivot_longer(cols = -c(id, gender), names_to = "area", values_to = "present") %>%
  filter(present > 0) %>%
  select(-present) %>%
  group_by(id) %>%
  summarise(bodymap = toString(area), gender = toString(unique(gender))) %>%
  ungroup()

# combine
proc_data <- rbind(
  male_data2, female_data2
)
saveRDS(proc_data, here("data/mf_data.rds"))
