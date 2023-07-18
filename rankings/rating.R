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

blowout_function <- function(w, l) {
  
}

# For each game add score weights and get the abs value of the rating differential
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
    rating_diff_absolute = diff_function(r),
    blowout_score = 
      case_when(
        team_1_won & score_1 > score_2 * 2 + 1 ~ TRUE,
        !team_1_won & score_2 > score_1 * 2 + 1 ~ TRUE,
        TRUE ~ FALSE
      )
  ) %>% 
  ungroup() %>% 
  mutate(
    game_number = row_number()
  )

# Make 2 rows per game -- one per team -- so now we have team and opponent
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

# Apply the positive or negative rating differential to get an initial
# game rating for each game
scores_initial <- 
  scores_long %>% 
  # Start at baseline rating 1000 for every team
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
    rating_opponent, rating_diff, rating_game,
    blowout_score
  )

# Initial weighted average of game ratings per team to get team ratings
ratings_initial <- 
  scores_initial %>% 
  group_by(team) %>% 
  summarise(
    # Round to nearest whole number
    rating_team = rating_game %>% weighted.mean(score_weight) %>% round(),
    n_games = n()
  ) %>% 
  arrange(desc(rating_team))

# Initialize ratings and scores for the loop
ratings_old <- ratings_initial
ratings_new <- tibble()
scores <- scores_initial
i <- 1

# Keep looping through and re-rating until the ratings stabilize and 
# `ratings_old` is the same as `ratings_new`
while (i < max_iterations & !identical(ratings_old, ratings_new)) {
  
  message(glue::glue("On iteration {i}"))
  
  # If this isn't the first iteration, set the team ratings we just calculated
  # to `ratings_old` so we can re-calculate new ratings and see if they match
  if (!identical(tibble(), ratings_new)) {
    ratings_old <- ratings_new
  }
  
  # Attach each opponent's latest rating to game scores
  scores %<>% 
    select(-rating_opponent) %>% 
    inner_join(
      ratings_old %>% 
        select(-n_games) %>% 
        rename(
          rating_opponent = rating_team,
          opponent = team
        ),
      by = "opponent"
    ) %>% 
    # Attach team's latest rating for blowout rule calc
    inner_join(
      ratings_old,
      by = "team"
    ) %>% 
    # Get the latest game rating given the new opponent team rating
    # (game `rating_diff` stays constant)
    mutate(
      rating_game = rating_opponent + rating_diff,
      # Add a boolean for whether each game was a blowout
      blowout = 
        case_when(
          rating_team - rating_opponent > 600 & blowout_score ~ TRUE,
          TRUE ~ FALSE
        )
    ) %>% 
    # TODO only remove blowouts when team will still have >= 5 games after they're removed
    # Remove blowouts
    filter(!blowout)
  
  # Re-calc the team average ratings
  ratings_new <- 
    scores %>% 
    group_by(team) %>% 
    summarise(
      rating_team = rating_game %>% weighted.mean(score_weight) %>% round(),
      n_games = n()
    ) %>% 
    arrange(desc(rating_team)) 
  
  print(ratings_new)
  
  i <- i + 1
}

# Add ranking number
rankings <- 
  ratings_new %>% 
  mutate(
    rank = row_number()
  )

readr::write_csv(rankings, glue::glue(here::here("rankings/rankings.csv")))
