library(tidyverse)
library(magrittr)

file <- "attendance_2022.csv"

raw <- 
  readr::read_csv(file)

wide <- 
  raw %>% 
  rename(
    name = `...2`
  ) %>% 
  select(
    name, `...4`:`...39`
  )
  
dates <-
  tibble(
    name = wide %>% select(-name) %>% slice(1) %>% tidyr::pivot_longer(everything()) %>% pull(value),
    date =  wide %>% select(-name) %>% slice(2) %>% tidyr::pivot_longer(everything()) %>% pull(value)
  ) %>% 
  tidyr::drop_na() %>% 
  mutate(
    type = 
      case_when(
        str_detect(tolower(name), "tryout") ~ "tryout",
        str_detect(tolower(name), "prac") ~ "practice",
        str_detect(tolower(name), "retreat") ~ "retreat",
        TRUE ~ "tournament"
      ),
    weight = 
      case_when(
        type == "tournament" ~ 1.5
        TRUE ~ 0.9
      ),
    practice_number = row_number()
  ) %>% 
  select(practice_number, type, weight)

long <- 
  wide %>% 
  slice(4:nrow(.)) %>% 
  tidyr::pivot_longer(
    -name,
    names_to = "practice_number"
  ) %>% 
  mutate(
    practice_number = 
      practice_number %>% 
      str_remove("...") %>% 
      as.integer() %>% 
      subtract(3L),
    value = 
      case_when(
        is.na(value) | value == "No" ~ 0,
        value == "Maybe" ~ 0.5,
        value == "Yes" ~ 1
      )
  ) %>% 
  filter(
    !name %in% c("Shanye", "Jolene", "Margo")
  )

joined <- 
  long %>% 
  left_join(dates) %>% 
  mutate(
    value_weighted = value * weight
  )

per_person <- 
  joined %>% 
  group_by(name) %>% 
  summarise(
    n_attended = sum(value),
    n_attended_weighted = sum(value_weighted),
    perc_attended = n_attended/max(.$practice_number),
    perc_attended_weighted = n_attended_weighted/max(.$practice_number),
  )

team_wide <- 
  per_person %>% 
  summarise(
    mean_attended = mean(perc_attended),
    median_attended = median(perc_attended),
    sd_attended = sd(perc_attended),
    mean_attended_weighted = mean(perc_attended),
    median_attended_weighted = median(perc_attended),
    sd_attended_weighted = sd(perc_attended)
  )

quantiles <- 
  per_person %>% 
  summarise(
    quantile = scales::percent(c(0.25, 0.5, 0.75)),
    value = quantile(perc_attended, c(0.25, 0.5, 0.75))
  )




