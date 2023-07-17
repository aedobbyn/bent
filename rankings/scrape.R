library(tidyverse)
library(foreach)

url <- "https://play.usaultimate.org/events/TCT-Pro-Elite-Challenge-East-2023/schedule/Women/Club-Women/"

raw <- 
  rvest::read_html(url) %>% 
  rvest::html_table() %>% 
  purrr::map(janitor::clean_names)

clean <- function(tbl) {
  nms <- 
    tbl[1, ] %>% 
    purrr::as_vector() %>% 
    snakecase::to_snake_case()
  
  names(tbl) <- nms
  
  tbl %>% 
    slice(2:nrow(tbl)) %>% 
    filter(status == "Final") %>% 
    select(team_1, team_2, score) %>% 
    mutate(
      team_1 = team_1 %>% str_remove_all("\\(.*\\)"),
      team_2 = team_2 %>% str_remove_all("\\(.*\\)"),
      score = score %>% str_remove_all("[\\\r\\\n\\\t]+")
    ) %>% 
    tidyr::separate(
      score,
      into = c("score_1", "score_2"),
      sep = "-"
    ) %>% 
    mutate_all(str_squish)
}

pool_play <- 
  foreach(
    i = 1:length(raw),
    .combine = bind_rows
  ) %do% {
    if (names(raw[[i]])[1] != "team") {
      raw[[i]] %>% 
        clean()
    }
  }

# Bracket play

tops <- 
  rvest::read_html(url) %>% 
  rvest::html_nodes(".top_area") %>% 
  rvest::html_text() %>% 
  str_replace_all("[\\\r\\\n\\\t]+", " - ") %>% 
  str_remove_all("\\(.*\\)") %>% 
  str_remove_all("^ - ") %>% 
  str_remove_all(" - $")

bottoms <- 
  rvest::read_html(url) %>% 
  rvest::html_nodes(".btm_area") %>% 
  rvest::html_text() %>% 
  str_replace_all("[\\\r\\\n\\\t]+", " - ") %>% 
  str_remove_all("\\(.*\\)") %>% 
  str_remove_all("^ - ") %>% 
  str_remove_all(" - $")

bracket_play <- 
  tibble(
    game_1 = tops,
    game_2 = bottoms
  ) %>% 
  tidyr::separate(
    game_1,
    into = c("score_1", "team_1"),
    sep = "-"
  ) %>% 
  tidyr::separate(
    game_2,
    into = c("score_2", "team_2"),
    sep = "-"
  ) %>% 
  mutate_all(str_squish)

all <- 
  bind_rows(
    pool_play,
    bracket_play
  ) %>% 
  filter(
    !str_detect(team_1, "[WL] of ") &
      !(score_1 == 0 & score_2 == 0)
  )

