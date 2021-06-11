
clean_full <- function(tbl) {
  tbl %>% 
    janitor::clean_names() %>% 
    filter(!str_detect(first_name, "Total")) %>% 
    rename_at(
      vars(matches("tryout")),
      ~ str_extract(., "june.*")
    ) %>% 
    select(
      contains("name"),
      contains("june"),
      email
    ) %>% 
    tidyr::unnest() %>% 
    tidyr::pivot_longer(
      contains("june"),
      names_to = "date",
      values_to = "attending"
    ) %>% 
    mutate(
      attending = 
        case_when(
          attending == "Yes" ~ TRUE,
          attending == "No" ~ FALSE,
          TRUE ~ FALSE
        ),
      date = 
        date %>% 
        snakecase::to_sentence_case() %>% 
        str_c(" 2021") %>% 
        lubridate::mdy()
    )
}

nice_bools <- function(x) {
  
  if (is.na(x) || !is.logical(x)) return(x)
  
  if (x == TRUE) return("yes")
  
  if (x == FALSE) return("no")
}

pull_emails <- function(tbl){
  tbl %>% 
    pull(email) %>% 
    extract(!is.na(.)) %>% 
    str_c(collapse = ", ")
}
