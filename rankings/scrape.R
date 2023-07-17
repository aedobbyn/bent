url <- "https://play.usaultimate.org/events/TCT-Pro-Elite-Challenge-East-2023/schedule/Women/Club-Women/"

raw <- 
  rvest::read_html(url) %>% 
  rvest::html_table() %>% 
  purrr::map(janitor::clean_names)

binded <- 
  raw %>% 
  bind_rows() %>% 
  filter(
    is.na(w_l)
  )

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


binded <- 
  foreach(
    i = 1:length(raw),
    .combine = bind_rows
  ) %do% {
    if (names(raw[[i]])[1] != "team") {
      raw[[i]] %>% 
        clean()
    }
  }
