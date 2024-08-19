library(tidyverse)
library(magrittr)

pul_teams <- 
  readr::read_csv("https://raw.githubusercontent.com/dfiorino/ultianalyticspull/2ebb3bf4f2a761e69665565660bcf17867f8e010/ultianalyticspull/data/pul/supplemental/pul_teams.csv") %>%
  janitor::clean_names() %>% 
  select(team, url) %>% 
  mutate(
    team_number = str_extract(url, "[0-9]+")
  )

# BENT
team_number <- "5003235896066048"

build_team_url <- function(team_number) {
  base_url <- "http://www.ultianalytics.com"
  team_url <- glue::glue("{base_url}/rest/view/team/{team_number}")
  glue::glue("{team_url}/stats/export")
}

get_labeled <- function(url) {
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
  
  trimmed %>%
    mutate(
      nice = (our_score_end_of_point == 6 & their_score_end_of_point == 9) |
        (our_score_end_of_point == 9 & their_score_end_of_point == 6)
    )
}

get_percent <- function(tbl) {
  counts <-
    labeled %>%
    count(nice)
  
    (counts %>% filter(nice) %>% pull(n) / 
       counts %>% filter(!nice) %>% pull(n)) * 100
}

out <- tibble()

for (i in 1:nrow(pul_teams)) {
  message(glue::glue("{i}/{nrow(pul_teams)}: {pul_teams$team[i]}"))
  
  url <- build_team_url(pul_teams$team_number[i])
  
  labeled <- get_labeled(url)
  
  percent <- get_percent(labeled)
  
  message(round(percent, 2))
  
  out %<>%
    bind_rows(
      tibble(
        percent = percent,
        team = pul_teams$team[i]
      )
    )
  
  Sys.sleep(runif(1, 0, 1))
}

out %>% 
  summarise(
    mean = mean(percent)
  )

