library(tidyverse)
library(googlesheets4)
library(magrittr)

key <- Sys.getenv("GSHEETS_API_KEY")
gs4_auth_configure(api_key = key)

gs4_auth_configure(path = here::here("config.json"))

# Date of the tryout is tomorrow (assuming we run this the day before)
DATE <- lubridate::today() + lubridate::days(1)

# We want people to have taken the health screening no more than 3 days before the tryout
HS_MAX_DAYS_BEFORE_TRYOUT <- 3

### Spreadsheets
SS_DEST <- "https://docs.google.com/spreadsheets/d/1VT6emqnMNFeSR46E_buGbTERK3gu-Ja6PYgEVvB0keg/edit#gid=0"

SS_HEALTH_SCREENING <- "https://docs.google.com/spreadsheets/d/1Zs5NBQpKyVlaw_KZyBM2DvRo2Y-qDT6bzbyzt1d_--o/edit?resourcekey#gid=438601480"

SS_VACCINES <- "https://docs.google.com/spreadsheets/d/1-rTV3YHh_D9btU2lnp5uUxI5pgVs5FE3hvV895nnYu0/edit?resourcekey#gid=208667669"

SS_FULL_DOC <- "https://docs.google.com/spreadsheets/d/1vCg_6Y8fFbAsutROBmh5Jvkpd20xMRSWJu7_lVGVV0k/edit#gid=0"

# Full list of tryouts
full_source <- 
  read_sheet(SS_FULL_DOC)

# Health screening form
hs_source <- 
  read_sheet(SS_HEALTH_SCREENING) 

# Vaccine response form
vax_source <- 
  read_sheet(SS_VACCINES)

# Take tryout data to long and filter to people just going to this `DATE`'s tryout
full <- 
  full_source %>% 
  janitor::clean_names() %>% 
  filter(!str_detect(first_name, "Total")) %>% 
  rename_at(
    vars(matches("tryout")),
    ~ str_extract(., "june.*")
  ) %>% 
  select(
    contains("name"),
    contains("june")
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
  ) %>% 
  filter(
    (date == DATE) & attending
  ) %>% 
  select(first_name, last_name)

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
  )

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
    vax_good = vax_date_good & vax_has_img
  )

# Take the full set of tryouts for this date and keep around the columns that would tell us why someone might not be `fully_good` to go
joined <- 
  full %>% 
  left_join(hs) %>% 
  left_join(vax) %>% 
  transmute(
    first_name, 
    last_name,
    health_screening_good = health_screening_good %>% coalesce(FALSE),
    vax_good = vax_good %>% coalesce(FALSE),
    fully_good = health_screening_good & vax_good,
    health_screening_date,
    vax_date_full,
    vax_has_img
  ) %>% 
  select(
    ends_with("name"),
    fully_good, 
    everything()
  ) %>% 
  arrange(first_name) %>% 
  rename_all(
    snakecase::to_sentence_case
  )

# Write out to destination
write_sheet(
  joined,
  SS_DEST,
  sheet = as.character(DATE)
)
