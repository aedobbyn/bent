library(tidyverse)

# TODO date weights
# TODO blowout rule

max_iterations <- 10000

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
    # Get the rating differential for this game
    rating_diff_absolute = diff_function(r)
  ) %>% 
  ungroup() %>% 
  mutate(
    game_number = row_number()
  ) 

# Make 2 rows per game -- one per team
scores_long <- 
  bind_rows(
    scores_weighted %>% 
      rename(
        team = team_1,
        opponent = team_2,
        score_team = score_1,
        score_opponent = score_2
      ) %>% 
      select(-team_1_won),
    scores_weighted %>% 
      rename(
        team = team_2,
        opponent = team_1,
        score_team = score_2,
        score_opponent = score_1
      ) %>% 
      select(-team_1_won)
  ) %>% 
  arrange(game_number)

scores_rated <- 
  scores_long %>% 
  # Start at baseline rating 1000 for everything and apply the rating
  # differential for each game (positive for winners, negative for losers)
  mutate(
    rating_opponent = 1000,
    rating_diff = 
      case_when(
        score_team > score_opponent ~ rating_diff_absolute,
        TRUE ~ rating_diff_absolute * -1
      ),
    rating_game = rating_opponent + rating_diff
  ) %>% 
  select(
    game_number, team, opponent, score_weight, 
    rating_opponent, rating_diff, rating_game
  )

# Weighted average of ratings per team
ratings_initial <- 
  scores_rated %>% 
  group_by(team) %>% 
  # TODO add in weights
  summarise(
    rating_team = rating_game %>% mean() %>% round()
  ) %>% 
  arrange(desc(rating_team))

scores_inital <- 
  scores_rated %>%
  select(-rating_opponent) %>% 
  inner_join(
    # Attach each opponent's new average rating
    ratings_initial %>% 
      rename(
        rating_opponent = rating_team,
        opponent = team
      ),
    by = "opponent"
  )

# Initialize
ratings_old <- ratings_initial
ratings_new <- tibble()

scores <- scores_inital
i <- 1

# Keep looping through and re-rating until the ratings stabilize and 
# `ratings_old` is the same as `ratings_new`
while (i < max_iterations & !identical(ratings_old, ratings_new)) {
  
  message(glue::glue("On iteration {i}"))
  
  if (!identical(tibble(), ratings_new)) {
    ratings_old <- ratings_new
  }
  
  # Attach each opponent's latest rating
  scores %<>% 
    select(-rating_opponent) %>% 
    inner_join(
      ratings_old %>% 
        rename(
          rating_opponent = rating_team,
          opponent = team
        ),
      by = "opponent"
    ) %>% 
    mutate(
      rating_game = rating_opponent + rating_diff
    )
  
  # Keep `ratings_new` as a global variable
  ratings_new <- 
    scores %>% 
    group_by(team) %>% 
    summarise(
      # Round to nearest whole number
      rating_team = rating_game %>% mean() %>% round()
    ) %>% 
    arrange(desc(rating_team)) 
  
  print(ratings_new)
  
  i <- i + 1
}

rankings <- 
  ratings_new %>% 
  mutate(
    rank = row_number()
  )

readr::write_csv(rankings, glue::glue(here::here("rankings/rankings.csv")))

