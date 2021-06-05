library(tidyverse)
library(googlesheets4)

source("utils.R")

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
    date_usau_waiver_signed = usau_waiver_2021_waiver_signed,
    date_disease_waiver_signed = infectious_diseases_waiver_2021_waiver_signed,
    has_membership = !is.na(products)
  )
  
joined <- 
  full %>% 
  left_join(insurance)


