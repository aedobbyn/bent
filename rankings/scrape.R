library(tidyverse)
library(foreach)
library(magrittr)

# List of all women's division club sanctioned events
event_list_url_reg_szn <- "https://play.usaultimate.org/events/league/?ViewAll=false&IsLeagueType=false&IsClinic=false&FilterByCategory=AE&CompetitionLevelId=22&GenderDivisionId=2&EventTypeId=16"

event_list_url_post_szn <- "https://play.usaultimate.org/events/league/?ViewAll=false&IsLeagueType=false&IsClinic=false&FilterByCategory=AE&CompetitionLevelId=22&EventTypeId=8"

grab_event_url_tbl <- function(url) {
  event_html <- 
    url %>% 
    rvest::read_html()
  
  # Grab the url hrefs
  event_urls <- 
    event_html %>% 
    # Just past events
    rvest::html_nodes(".alt-style-2 td") %>% 
    rvest::html_elements("a") %>% 
    rvest::html_attr("href") %>% 
    str_c("/schedule/Women/Club-Women/") %>% 
    c("https://play.usaultimate.org/events/2023-US-Open-Club-Championships---ICC/schedule/Women/Club-Women/")
  
  # Grab table text
  event_html %>% 
    rvest::html_table() %>% 
    .[[2]] %>% 
    janitor::clean_names() %>% 
    select(event_name, dates) %>% 
    add_row(
      tibble(
        event_name = "US Open",
        dates = "Aug 04, 2023 - Aug 06, 2023"
      )
    ) %>% 
    mutate(
      start_date = dates %>% str_remove(" - .*") %>% lubridate::mdy(),
      month = start_date %>% lubridate::month(),
      year = start_date %>% lubridate::year(),
      # Attach the urls
      url = event_urls
    ) %>% 
    # We just want events that happened in June or after of this year
    filter(
      year == lubridate::year(lubridate::today()) &
        month >= 6
    )
}

# Grab regular and post season url tbls and combine
event_url_tbl_reg_szn <- grab_event_url_tbl(event_list_url_reg_szn)
event_url_tbl_post_szn <- grab_event_url_tbl(event_list_url_post_szn)

event_url_tbl <- 
  bind_rows(
    event_url_tbl_reg_szn,
    event_url_tbl_post_szn
  ) %>% 
  distinct()

# TODO filter out scores from non-US teams

# Get a dataframe of all the scores on the page
scrape_scores <- function(url) {
  ### Pool Play ###
  
  # Read in pool play tables
  raw <- 
    rvest::read_html(url) %>% 
    rvest::html_table() %>% 
    purrr::map(janitor::clean_names)
  
  if (length(raw) == 0) {
    message("‼️ Could not scrape this page.")
    return(tibble())
  }
  
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
      mutate_all(str_squish) %>% 
      # In a W-F situation, wins into 15 and forfeits into 0
      mutate(
        score_1 = 
          case_when(
            score_1 == "W" ~ "15",
            score_1 == "F" ~ "0",
            TRUE ~ score_1
          ),
        score_2 = 
          case_when(
            score_2 == "W" ~ "15",
            score_2 == "F" ~ "0",
            TRUE ~ score_2
          )
      ) 
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
  
  # hehe
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

  # Put pool & bracket play together
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
}

# Trycatch
try_scrape_scores <- purrr::possibly(scrape_scores)

out <- tibble()

# Loop through all the event urls
for (i in 1:nrow(event_url_tbl)) {

  url <- event_url_tbl$url[i]
  event_name <- event_url_tbl$event_name[i]
  start_date <- event_url_tbl$start_date[i]
  
  message(glue::glue("{i} of {nrow(event_url_tbl)}: {event_name} ({url})"))
  
  this <- try_scrape_scores(url) 
  
  # If something went wrong, move on
  if (length(this) == 0) next()
  
  this %<>% 
    mutate(
      event = event_name,
      date = start_date
    )
  
  out %<>% bind_rows(this)
  
  Sys.sleep(runif(1, 1, 2))
}

out %<>% 
  arrange(date, event)

# Write to csv
readr::write_csv(out, glue::glue(here::here("rankings/scores.csv")))

