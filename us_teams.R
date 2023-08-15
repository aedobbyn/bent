library(dplyr)
library(stringr)

URL <- "https://www.frisbee-rankings.com/usau/club/women"

raw <- 
  rvest::read_html(URL) %>% 
  rvest::html_node("table") %>% 
  rvest::html_table()

out <- 
  raw %>% 
  select(team = Team) %>% 
  transmute(
    team = 
      team %>% 
      str_remove_all("[A-Z][A-Z] [0-9]$") %>% 
      str_squish()
  )

readr::write_csv(out, "rankings/us_teams.csv")
