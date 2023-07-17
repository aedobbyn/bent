library(tidyverse)
library(foreach)

url <- "https://play.usaultimate.org/events/TCT-Pro-Elite-Challenge-East-2023/schedule/Women/Club-Women/"


### Pool Play ###

# Read in pool play tables
raw <- 
  rvest::read_html(url) %>% 
  rvest::html_table() %>% 
  purrr::map(janitor::clean_names)

# Make the first row of the table the column names & separate into cols 
# we care about
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

# Filter out seeding tables, clean each pool play table and bind them into
# one dataframe
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

### Bracket Play ###

# Grab a vector of scores listed on the top & bottom of an individual matchup,
# clean them up to keep just the score and team name
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

game_status <- 
  rvest::read_html(url) %>% 
  rvest::html_nodes(".game-status") %>% 
  rvest::html_text() %>% 
  str_remove_all("[\\\r\\\n\\\t]+") 

# Put into a dataframe and separate team name and score
bracket_play <- 
  tibble(
    game_1 = tops,
    game_2 = bottoms,
    game_status = game_status
  ) %>% 
  filter(game_status == "Final") %>% 
  select(-game_status) %>% 
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
    # Make sure it's a game that counts in case game status still says final
    # for some reason
    !str_detect(team_1, "[WL] of ") &
      !(score_1 == 0 & score_2 == 0)
  )

