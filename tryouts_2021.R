library(tidyverse)
library(googlesheets4)
library(magrittr)

gs4_auth()
1

source("utils.R")
source("not_getting_vaxed.R")

# Date of the tryout is tomorrow (assuming we run this the day before)
DATE <- lubridate::today() + 1

# We want people to have taken the health screening no more than 3 days before the tryout
HS_MAX_DAYS_BEFORE_TRYOUT <- 5

### Spreadsheets
SS_DEST <- "https://docs.google.com/spreadsheets/d/1VT6emqnMNFeSR46E_buGbTERK3gu-Ja6PYgEVvB0keg/edit#gid=0"

SS_HEALTH_SCREENING <- "https://docs.google.com/spreadsheets/d/1Zs5NBQpKyVlaw_KZyBM2DvRo2Y-qDT6bzbyzt1d_--o/edit?resourcekey#gid=438601480"

SS_VACCINES <- "https://docs.google.com/spreadsheets/d/1-rTV3YHh_D9btU2lnp5uUxI5pgVs5FE3hvV895nnYu0/edit?resourcekey#gid=208667669"

SS_FULL_DOC <- "https://docs.google.com/spreadsheets/d/1vCg_6Y8fFbAsutROBmh5Jvkpd20xMRSWJu7_lVGVV0k/edit#gid=0"

SS_INSURANCE <- "insurance.csv"

# Full list of tryouts
full_source <- 
  read_sheet(SS_FULL_DOC)

# Health screening form
hs_source <- 
  read_sheet(SS_HEALTH_SCREENING) 

# Vaccine response form
vax_source <- 
  read_sheet(SS_VACCINES)

# DiscNY info
insurance_source <- 
  readr::read_csv(SS_INSURANCE) %>% 
  janitor::clean_names()

# Take tryout data to long and filter to people just going to this `DATE`'s tryout
full <- 
  full_source %>% 
  clean_full() 

full_distinct <- 
  full %>% 
  distinct(first_name, last_name, email) 

insurance_selected <- 
  insurance_source %>%
  transmute(
    first_name = 
      case_when(
        first_name == "Abagael" & last_name == "Cheng" ~ "Abby", 
        first_name == "Melanie" & last_name == "Sawyer" ~ "Mel", 
        TRUE ~ first_name
      ), 
    last_name = 
      case_when(
        first_name == "Cory" & last_name == "Salé" ~ "Sale", 
        TRUE ~ last_name
      ),
    signed_usau_waiver = !is.na(usau_waiver_2021_waiver_signed),
    signed_disease_waiver = !is.na(infectious_diseases_waiver_2021_waiver_signed),
    has_usau_membership = !is.na(products)
  )

insurance <- 
  full_distinct %>% 
  left_join(insurance_selected) %>% 
  arrange(first_name, last_name) %>% 
  mutate_all(
    tidyr::replace_na, 
    FALSE
  ) %>% 
  mutate(
    insurance_good = 
      # signed_usau_waiver & 
      # signed_disease_waiver & 
      has_usau_membership
  ) %>% 
  select(
    contains("name"),
    insurance_good,
    everything()
  )

full_today <- 
  full %>% 
  filter(
    (date == DATE) & attending
  ) %>% 
  select(first_name, last_name, email)

# Give people a `health_screening_good` of TRUE if they filled out the form within `HS_MAX_DAYS_BEFORE_TRYOUT` days of the tryout and answered all the questions correctly
hs <- 
  hs_source %>% 
  janitor::clean_names() %>% 
  transmute(
    first_name, 
    last_name,
    health_screening_date = timestamp %>% lubridate::as_date(),
    date_ok = (DATE - health_screening_date) < HS_MAX_DAYS_BEFORE_TRYOUT,
    symptoms_ok = in_the_past_10_days_have_you_experienced_any_symptoms_of_covid_19 %>% str_detect("^NO"),
    test_ok = in_the_past_10_days_have_you_tested_positive_for_covid_19 %>% str_detect("^NO"),
    contact_ok = in_the_past_14_days_have_you_been_in_close_contact_with_any_confirmed_or_suspected_covid_19_cases %>% str_detect("^NO"),
    travel_ok = in_the_past_10_days_i_have_followed_all_nys_travel_restrictions %>% str_detect("^YES"),
    health_screening_good = date_ok & symptoms_ok & test_ok & contact_ok & travel_ok
  ) %>% 
  mutate(
    first_name = 
      case_when(
        last_name == "Whelan" ~ "Jess",
        TRUE ~ first_name
      )
  ) %>% 
  group_by(first_name, last_name) %>% 
  arrange(desc(health_screening_date)) %>% 
  slice(1) %>% 
  ungroup()

# Give people a `vax_good` of TRUE if they're at least 2 weeks out from their final shot and uploaded proof of vaccination
vax <- 
  vax_source %>% 
  janitor::clean_names() %>% 
  transmute(
    first_name, 
    last_name, 
    last_shot_date = if_you_have_been_vaccinated_please_provide_the_date_you_received_or_will_receive_your_shot_if_johnson_johnson_or_your_second_shot_if_moderna_or_pfizer %>% lubridate::as_date(),
    vax_has_img = !is.na(if_you_have_been_vaccinated_please_upload_an_image_of_your_vaccine_card_your_ny_excelsior_pass_or_other_proof_of_vaccination),
    vax_date_full = last_shot_date + lubridate::weeks(2),
    vax_date_good = vax_date_full < DATE,
    vax_good = 
      # (vax_date_good & 
      vax_has_img
  )

# Take the full set of tryouts for this date and keep around the columns that would tell us why someone might not be `fully_good` to go
joined <- 
  full_today %>% 
  left_join(hs) %>% 
  left_join(vax) %>% 
  left_join(insurance %>% select(-email)) %>% 
  mutate() %>% 
  transmute(
    across(
      matches("_ok|_good|has_|signed_"),
      ~ .x %>% coalesce(FALSE)
    ),
    non_vax = email %in% non_vax,
    first_name, 
    last_name,
    email = tolower(email),
    health_screening_good,
    vax_good = vax_good | non_vax,
    insurance_good = insurance_good | (tolower(email) %in% did_insurance),
    fully_good = health_screening_good & vax_good & insurance_good,
    health_screening_date,
    vax_date_full,
    vax_has_img,
    signed_usau_waiver,
    signed_disease_waiver,
    has_usau_membership
  ) %>% 
  rowwise() %>% 
  mutate_all(
    nice_bools
  ) %>% 
  ungroup() %>% 
  group_by(first_name, last_name) %>% 
  # There are a few dupes, so if a person is duped, take the version of them that has signed things in 2021
  arrange(
    desc(signed_usau_waiver),
    desc(signed_disease_waiver),
    desc(has_usau_membership),
    desc(vax_has_img)
  ) %>% 
  distinct(first_name, last_name, .keep_all = TRUE) %>% 
  ungroup() %>% 
  select(
    ends_with("name"),
    email,
    fully_good, 
    vax_good,
    insurance_good,
    health_screening_good
  ) %>% 
  arrange(first_name)

out <- 
  joined %>% 
  rename_all(
    snakecase::to_sentence_case
  )

# Write out to destination
write_sheet(
  out,
  SS_DEST,
  sheet = as.character(DATE)
)
