library(tidyverse)
library(googlesheets4)

source("utils.R")

SS_DEST <- "https://docs.google.com/spreadsheets/d/1VT6emqnMNFeSR46E_buGbTERK3gu-Ja6PYgEVvB0keg/edit#gid=0"

SS_FULL_DOC <- "https://docs.google.com/spreadsheets/d/1vCg_6Y8fFbAsutROBmh5Jvkpd20xMRSWJu7_lVGVV0k/edit#gid=0"

SS_INSURANCE <- "insurance.csv"

# Full list of tryouts
full_source <- 
  read_sheet(SS_FULL_DOC)

# Take tryout data to long and filter to people just going to this `DATE`'s tryout
full <- 
  full_source %>% 
  clean_full() %>% 
  distinct(first_name, last_name) %>% 
  mutate_all(tolower) 

insurance_source <- 
  readr::read_csv(SS_INSURANCE) 

insurance <- 
  insurance_source %>%
  janitor::clean_names() %>% 
  mutate_all(tolower) %>% 
  transmute(
    first_name, 
    last_name,
    signed_usau_waiver = !is.na(usau_waiver_2021_waiver_signed),
    signed_disease_waiver = !is.na(infectious_diseases_waiver_2021_waiver_signed),
    has_membership = !is.na(products)
  )
  
joined <- 
  full %>% 
  left_join(insurance) %>% 
  arrange(first_name, last_name) %>% 
  mutate_all(
    tidyr::replace_na, 
    FALSE
  ) %>% 
  mutate(
    good_to_go = 
      case_when(
        signed_usau_waiver & signed_disease_waiver & has_membership ~ "yes",
        TRUE ~ "no"
      )
  ) %>% 
  select(
    contains("name"),
    good_to_go,
    everything()
  )

names(joined) <- names(joined) %>% snakecase::to_title_case() %>% tolower()

write_sheet(
  joined,
  SS_DEST,
  sheet = "insurance"
)
