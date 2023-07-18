library(tidyverse)

# TODO date weights

scores <- readr::read_csv(glue::glue(here::here("rankings/scores.csv")))

diff_function <- function(r) {
  125 + 475 * ((sin(min(1, (1 - r)/0.5) * 0.4 * pi)) / sin(0.4 * pi))
}

scores_weighted <- 
  scores %>% 
  rowwise() %>% 
  mutate(
    team_1_won = score_1 > score_2,
    team_2_won = !team_1_won,
    score_weight = 
      case_when(
        score_1 >= 13 | score_2 >= 13 | score_1 + score_2 >= 19 ~ 1,
        team_1_won ~ min(1, sqrt( (score_1 + max(score_2, floor((score_1 - 1)/2))) / 19)),
        TRUE ~ min(1, sqrt( (score_2 + max(score_1, floor((score_2 - 1)/2))) / 19))
      ),
    r = 
      case_when(
        team_1_won ~ score_2 / (score_1 - 1),
        TRUE ~ score_1 / (score_2 - 1)
      ),
    rating_diff_absolute = diff_function(r)
  )

scores_long <- 
  scores_weighted %>% 
  tidyr::pivot_longer(
    team_1:team_2,
    names_to = "team_number",
    values_to = "team"
  ) 

scores_initial <- 
  scores_long %>% 
  mutate(
    rating_diff = 
      case_when(
        team_1_won & team_number == "team_1" | team_2_won & team_number == "team_2" ~ rating_diff_absolute,
        TRUE == 1 ~  rating_diff_absolute * -1
      ),
    rating_baseline = 1000,
    rating = rating_initial + rating_diff
  ) %>% 
  select(team, score_weight, rating_baseline, rating_diff, rating)

ratings_initial <- 
  scores_initial %>% 
  group_by(team) %>% 
  # TODO add in weights
  summarise(
    rating_avg = mean(rating)
  )

iterate <- function() {
  ratings_old <- ratings_initial
  ratings_new <- tibble()
  
  scores <- scores_initial
  
  while (! identical(ratings_old, ratings_new)) {
    ratings_old <- ratings_new
    
    scores %<>% 
      # Attach rating_avg
      select(-rating_baseline) %>% 
      inner_join(
        ratings_old %>% 
          rename(
            rating_baseline = rating_avg
          )
      ) %>% 
      mutate(
        rating = rating_baseline + rating_diff
      )
    
    ratings_new <- 
      scores %>% 
      group_by(team) %>% 
      summarise(
        rating_avg = mean(rating)
      )
  }
}



               