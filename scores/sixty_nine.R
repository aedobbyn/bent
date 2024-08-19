library(tidyverse)
library(magrittr)

pul_teams <-
  readr::read_csv(
    "https://raw.githubusercontent.com/dfiorino/ultianalyticspull/2ebb3bf4f2a761e69665565660bcf17867f8e010/ultianalyticspull/data/pul/supplemental/pul_teams.csv"
  ) %>%
  janitor::clean_names() %>%
  select(team, url) %>%
  mutate(
    team_number = str_extract(url, "[0-9]+")
  )

# BENT
team_number <- "5003235896066048"

build_team_url <-
  function(team_number) {
    base_url <- "http://www.ultianalytics.com"
    team_url <- glue::glue("{base_url}/rest/view/team/{team_number}")
    glue::glue("{team_url}/stats/export")
  }

get_raw <-
  function(url) {
    httr::GET(url) %>%
      httr::content(encoding = "utf-8") %>%
      readr::read_csv(
        col_types = readr::cols(.default = "c")
      ) %>%
      janitor::clean_names() %>%
      distinct(
        tournament = tournamemnt,
        # lol
        opponent,
        date_time,
        our_score_end_of_point,
        their_score_end_of_point
      ) %>%
      distinct()
  }

label <-
  function(tbl) {
    tbl %>%
      janitor::clean_names() %>%
      mutate(
        date = date_time %>% lubridate::ymd_hm() %>% lubridate::as_date(),
        our_score_end_of_point = as.integer(our_score_end_of_point),
        their_score_end_of_point = as.integer(their_score_end_of_point)
      ) %>%
      mutate(
        nice = (our_score_end_of_point == 6 & their_score_end_of_point == 9) |
          (our_score_end_of_point == 9 & their_score_end_of_point == 6)
      ) %>%
      select(-date_time)
  }

get_percent_at_any_moment <-
  function(tbl) {
    counts <-
      tbl %>%
      count(nice) %>%
      tidyr::drop_na()

    yes <-
      counts %>%
      filter(nice) %>%
      pull(n)
    no <-
      counts %>%
      filter(!nice) %>%
      pull(n)

    yes / (no + yes) * 100
  }

get_percent_for_game <-
  function(tbl) {
    nice_games <-
      tbl %>%
      filter(nice) %>%
      select(
        team,
        opponent,
        tournament,
        date
      )

    not_nice_games <-
      tbl %>%
      anti_join(nice_games) %>%
      distinct(
        team,
        opponent,
        tournament,
        date
      )

    yes <- nrow(nice_games)
    no <- nrow(not_nice_games)

    yes / (no + yes) * 100
  }

dat <- tibble()

# Grab data
for (i in 1:nrow(pul_teams)) {
  message(
    glue::glue("{i}/{nrow(pul_teams)}: {pul_teams$team[i]}")
  )

  url <- build_team_url(pul_teams$team_number[i])

  this <-
    get_raw(url) %>%
    mutate(
      team = pul_teams$team[i]
    ) %>%
    select(
      team, opponent, everything()
    )

  dat %<>%
    bind_rows(this)

  Sys.sleep(runif(1, 0, 1))
}

labeled <- label(dat)

percent_any_moment <- get_percent_at_any_moment(labeled)

percent_for_game <- get_percent_for_game(labeled)
