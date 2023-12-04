library(tidyverse)

SS <- "https://docs.google.com/spreadsheets/d/1o3asTrVC6t0XUQ8p8wvw59cuxKrhMLe1AoO-dqIyEhg/edit#gid=191383701"

raw <- 
  googlesheets4::read_sheet(SS, sheet = 1) %>% 
  janitor::clean_names()

unselect_cols <- "player_" %>% str_c(7:27)

# We don't need any columns after this
last_col <- which(names(raw) == "elapsed_time_secs")

clean <- 
  raw %>% 
  select(1:last_col) %>% 
  select(-any_of(unselect_cols)) %>% 
  rename(
    tournament = tournamemnt
  ) %>% 
  mutate(
    point_number = our_score_end_of_point + their_score_end_of_point
  )

actions_per_game <- 
  clean %>% 
  count(date_time, tournament, opponent)

points_per_game <- 
  clean %>% 
  distinct(date_time, tournament, opponent, point_number) %>% 
  count(date_time, tournament, opponent)

long <- 
  clean %>% 
  select(
    point_number, tournament, opponent, line, player_0:player_6,
    event_type, action, passer, receiver, defender
  ) %>% 
  filter(!str_detect(action, "Pull")) %>% 
  tidyr::pivot_longer(
    player_0:player_6,
    names_to = "player_number",
    values_to = "player"
  ) %>% 
  select(-player_number) 

points_per_player <- 
  long %>% 
  distinct(point_number, tournament, opponent, player) %>% 
  count(player, sort = TRUE)

actions <- 
  long %>% 
  filter(
    player == passer | player == receiver | player == defender
  )

actions_per_player <- 
  actions %>% 
  count(player, sort = TRUE)

actions_per_type_per_player <- 
  actions %>% 
  count(player, action, sort = TRUE)
