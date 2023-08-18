library(tidyverse)
library(magrittr)

max_iterations <- 10000

# Assign each week a date weight. First week of the season gets 0.5 and last
# week gets 1
first_week <- lubridate::week("2023-06-01")
last_week <- lubridate::week("2023-09-19")
week_diff <- last_week - first_week
date_weight_diff <- 0.5 / week_diff
week_n <- length(first_week:last_week)
date_weight_tbl <-
  tibble(
    week = first_week:last_week,
    date_weight = scales::rescale(exp(1:week_n / week_n), c(.5, 1))
  )

scores_raw <-
  readr::read_csv(
    glue::glue(here::here("rankings/scores.csv")),
    col_types = "ccii"
  )

score_projections <- 
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1e9ZnURe13xdfcqiiB5pxn1WU9QW0nIFEVf4MOXm5bsk/edit#gid=0") %>% 
  mutate(
    date = lubridate::as_date(date)
  )

scores_raw %<>% 
  bind_rows(score_projections)

us_teams <- readr::read_csv(here::here("rankings/us_teams.csv"))

# Filter games to just US teams
scores_us <-
  scores_raw %>%
  filter(
    team_1 %in% us_teams$team &
      team_2 %in% us_teams$team
  )

# Rating differential function
diff_function <-
  function(r) {
    125 + 475 * ((sin(min(1, (1 - r) / 0.5) * 0.4 * pi)) / sin(0.4 * pi))
  }

# Used in rating differential
r_function <- function(w, l) {
  l / (w - 1)
}

# Score weight function where w is winner score and l is loser score
score_weight_function <-
  function(
    w,
    l
  ) {
    min(1, sqrt((w + max(l, floor((w - 1) / 2))) / 19))
  }

# For each game, add score weights, date weights, and get the abs value of the
# rating differential
scores_weighted <-
  scores_us %>%
  rowwise() %>%
  mutate(
    team_1_won = score_1 > score_2,
    week = lubridate::week(date),
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
  ) %>%
  # Attach date weights
  inner_join(date_weight_tbl) %>%
  mutate(
    # Game weight is product of score weight and date weight
    weight = score_weight * date_weight
  ) %>%
  select(
    game_number,
    starts_with("team"),
    starts_with("score"),
    ends_with("weight"),
    rating_diff_absolute,
    everything()
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
    game_number,
    team,
    opponent,
    weight,
    rating_opponent,
    rating_diff,
    rating_game,
    blowout_score
  )

# Initial weighted average of game ratings per team to get team ratings
ratings_initial <-
  scores_initial %>%
  group_by(team) %>%
  summarise(
    # Round to nearest whole number
    rating_team = rating_game %>% weighted.mean(weight) %>% round(),
    n_games = n()
  ) %>%
  arrange(desc(rating_team))

# Initialize ratings and scores for the loop
ratings_old <- ratings_initial %>% select(-n_games)
ratings_new <-
  tibble(
    team = character(),
    rating_team = double(),
    n_games = integer()
  )
rankings_old <-
  ratings_old %>%
  mutate(
    rank = row_number()
  )
rankings_new <-
  tibble(team = character(), rating_team = double(), rank = integer())
scores <-
  scores_initial %>%
  inner_join(
    ratings_initial %>%
      select(team, n_games)
  )
i <- 1
mean_ratings_diff <- 0

# Keep looping through and re-rating until the ratings stabilize
while (i < max_iterations & !between(mean_ratings_diff, .9999, 1.0001)) {
  # See how many differences in rankings this last iteration produced
  n_diffs <-
    anti_join(
      rankings_old,
      rankings_new,
      by = c("team", "rank")
    ) %>%
    nrow()

  message(
    glue::glue(
      "On iteration {i}. % difference: {n_diffs}/{nrow(ratings_old)} = {round(n_diffs/nrow(ratings_old), 2) * 100}%"
    )
  )

  # If this isn't the first iteration, set the team ratings we just calculated
  # to `ratings_old` so we can re-calculate new ratings and see if they match
  if (nrow(ratings_new) > 0) {
    ratings_old <- ratings_new
    rankings_old <- rankings_new
  }

  # Attach each opponent's latest rating to game scores
  scores_joined <-
    scores %>%
    select(-rating_opponent, -rating_game) %>%
    # Attach each opponent's ratings from the last iteration
    inner_join(
      ratings_old %>%
        select(
          rating_opponent = rating_team,
          opponent = team
        ),
      by = "opponent"
    ) %>%
    # Attach team's latest rating for blowout rule calc & n games
    inner_join(
      ratings_old %>%
        select(
          team,
          rating_team
        ),
      by = "team"
    ) %>%
    # Get the latest game rating given the new opponent team rating
    # (game `rating_diff` stays constant)
    mutate(
      rating_game = rating_opponent + rating_diff,
      # Add a boolean for whether each game was a blowout given
      # each team's new rating
      blowout =
        case_when(
          abs(rating_team - rating_opponent) > 600 & blowout_score ~
            TRUE,
          TRUE ~ FALSE
        )
    )

  # There are new games that are designated as blowouts because we've
  # re-calculated every team's rating. So count number of new blowouts per team
  n_blowouts <-
    scores_joined %>%
    group_by(team) %>%
    summarise(n_blowouts = sum(blowout))

  # Get a dataframe of how many blowouts to remove per team, if not 0
  n_blowouts_to_remove <-
    scores_joined %>%
    inner_join(n_blowouts, by = "team") %>%
    # Need to have at least 5 games that aren't blowouts
    filter(
      n_games - n_blowouts >= 5
    )

  # Dataframe of blowout games to get rid of
  blowouts_to_remove <-
    if (nrow(n_blowouts_to_remove) > 0) {
      scores_joined %>%
        semi_join(n_blowouts_to_remove, by = c("team")) %>%
        filter(blowout) %>%
        select(
          game_number
        )
    } else {
      tibble(team = character(), game_number = integer())
    }

  message(
    glue::glue(
      "Removing {nrow(blowouts_to_remove)} blowout games."
    )
  )

  # Use the `game_number` to remove blowouts
  scores_filtered <-
    scores_joined %>%
    anti_join(blowouts_to_remove, by = c("game_number"))

  # Re-calc the team average ratings
  ratings_new <-
    scores_filtered %>%
    group_by(team) %>%
    summarise(
      rating_team = rating_game %>% weighted.mean(weight) %>% round()
    ) %>%
    arrange(desc(rating_team))

  # Add ranking number
  rankings_new <-
    ratings_new %>%
    mutate(
      rank = row_number()
    )

  print(ratings_new)

  mean_ratings_diff <-
    mean(
      arrange(ratings_new, team)$rating_team /
        arrange(ratings_old, team)$rating_team,
      na.rm = TRUE
    )

  i <- i + 1
}

rankings <- rankings_new

readr::write_csv(
  rankings,
  glue::glue(here::here("rankings/rankings.csv"))
)
