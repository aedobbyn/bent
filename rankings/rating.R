library(tidyverse)

# TODO date weights
# TODO blowout rule

scores_raw <- 
  readr::read_csv(
    glue::glue(here::here("rankings/scores.csv")),
    col_types = "ccii"
  )

# Rating differential function
diff_function <- function(r) {
  125 + 475 * ((sin(min(1, (1 - r)/0.5) * 0.4 * pi)) / sin(0.4 * pi))
}

# Score weight function where w is winner score and l is loser score
score_weight_function <- function(w, l) {
  min(1, sqrt((w + max(l, floor((w - 1)/2))) / 19))
}

r_function <- function(w, l) {
  l / (w - 1)
}

scores_weighted <- 
  scores_raw %>% 
  rowwise() %>% 
  mutate(
    team_1_won = score_1 > score_2,
    score_weight = 
      case_when(
        # If either team scored >= 13 points or combined score was >= 19, 
        # the score weight is 1
        score_1 >= 13 | score_2 >= 13 | score_1 + score_2 >= 19 ~ 1,
        # Otherwise if team 1 won, apply score weight function
        team_1_won ~ score_weight_function(score_1, score_2),
        # Or if team 2 won
        TRUE ~ score_weight_function(score_2, score_1)
      ),
    # Get r variable used in rating differential function
    r = 
      case_when(
        team_1_won ~ r_function(score_1, score_2),
        TRUE ~ r_function(score_2, score_1)
      ),
    rating_diff_absolute = diff_function(r)
  )

# Pivot to one row per team
scores_long <- 
  scores_weighted %>% 
  ungroup() %>% 
  mutate(
    game_number = row_number()
  ) %>% 
  tidyr::pivot_longer(
    team_1:team_2,
    names_to = "team_number",
    values_to = "team"
  ) 

# Start at baseline rating 1000 for everything and apply the rating differential
scores_initial <- 
  scores_long %>% 
  mutate(
    rating_diff = 
      case_when(
        team_1_won & team_number == "team_1" | 
          !team_1_won & team_number == "team_2" ~ rating_diff_absolute,
        TRUE == 1 ~  rating_diff_absolute * -1
      ),
    rating_baseline = 1000,
    rating_game = rating_baseline + rating_diff
  ) %>% 
  select(game_number, team, score_weight, rating_baseline, rating_diff, rating_game)

# Weighted average of ratings per team
ratings_initial <- 
  scores_initial %>% 
  group_by(team) %>% 
  # TODO add in weights
  summarise(
    rating_team = mean(rating_game)
  ) %>% 
  arrange(desc(rating_team))

# TODO need to use opponent's rating_baseline instead of own team's when recompute

scores <- 
  scores_initial %>%
  select(-rating_baseline) %>% 
  inner_join(
    ratings_initial %>% 
      rename(
        rating_baseline = rating_team
      ),
    by = "team"
  )

iterate <- function() {
  ratings_old <- ratings_initial
  ratings_new <- tibble()
  
  scores <- scores_initial
  
  while (! identical(ratings_old, ratings_new)) {
    
    if (!identical(tibble(), ratings_new)) {
      ratings_old <- ratings_new
    }
    
    scores %<>% 
      # Attach each team's new rating_avg
      select(-rating_baseline) %>% 
      inner_join(
        ratings_old %>% 
          rename(
            rating_baseline = rating_team
          ),
        by = "team"
      ) %>% 
      mutate(
        rating_game = rating_baseline + rating_diff
      )
    
    ratings_new <- 
      scores %>% 
      group_by(team) %>% 
      summarise(
        rating_team = mean(rating_game)
      ) %>% 
      arrange(desc(rating_team)) 
  }
}


               