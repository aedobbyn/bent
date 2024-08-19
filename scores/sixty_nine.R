library(tidyverse)

# BENT
team_number <- "5003235896066048"

base_url <- "http://www.ultianalytics.com"
team_url <- glue::glue("{base_url}/rest/view/team/{team_number}")
team_data_url <- glue::glue("{team_url}/stats/export")

url <- glue::glue(team_data_url)

raw <-
  httr::GET(url) %>%
  httr::content(encoding = "utf-8") %>%
  readr::read_csv()

trimmed <-
  raw %>%
  janitor::clean_names() %>%
  distinct(
    tournamemnt,
    opponent,
    our_score_end_of_point,
    their_score_end_of_point
  )

labeled <-
  trimmed %>%
  mutate(
    nice = (our_score_end_of_point == 6 & their_score_end_of_point == 9) |
      (our_score_end_of_point == 9 & their_score_end_of_point == 6)
  )

counts <-
  labeled %>%
  count(nice)

perc <-
  (counts %>% filter(nice) %>% pull(n) / counts %>%
    filter(!nice) %>%
    pull(n)) * 100

perc
