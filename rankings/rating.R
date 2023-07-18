library(tidyverse)

# TODO date weights

scores <- readr::read_csv(glue::glue(here::here("rankings/scores.csv")))

scores_weighted <- 
  scores %>% 
  rowwise() %>% 
  mutate(
    team_1_won = score_1 > score_2,
    team_2_won = !team_1_won,
    r = 
      case_when(
        team_1_won ~ score_2 / (score_1 - 1),
        TRUE ~ score_1 / (score_2 - 1)
      ),
    score_weight = 
      case_when(
        score_1 >= 13 | score_2 >= 13 | score_1 + score_2 >= 19 ~ 1,
        team_1_won ~ min(1, sqrt( (score_1 + max(score_2, floor((score_1 - 1)/2))) / 19)),
        TRUE ~ min(1, sqrt( (score_2 + max(score_1, floor((score_2 - 1)/2))) / 19))
      )
  )


diff_function <- 
  125 + 475 * ((sin(min(1, (1 - r)/0.5) * 0.4 * pi)) / sin(0.4 * pi))


               