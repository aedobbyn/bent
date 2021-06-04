library(tidyverse)
library(googlesheets4)
library(magrittr)

DATE <- lubridate::today() + lubridate::days(1)

HS_MAX_DAYS_BEFORE_TRYOUT <- 3

SS_DEST <- "https://docs.google.com/spreadsheets/d/1VT6emqnMNFeSR46E_buGbTERK3gu-Ja6PYgEVvB0keg/edit#gid=0"

SS_HEALTH_SCREENING <- "https://docs.google.com/spreadsheets/d/1Zs5NBQpKyVlaw_KZyBM2DvRo2Y-qDT6bzbyzt1d_--o/edit?resourcekey#gid=438601480"

SS_VACCINES <- "https://docs.google.com/spreadsheets/d/1-rTV3YHh_D9btU2lnp5uUxI5pgVs5FE3hvV895nnYu0/edit?resourcekey#gid=208667669"

hs_source <- 
  read_sheet(SS_HEALTH_SCREENING) 

vax_source <- 
  read_sheet(SS_VACCINES)

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
    health_screening_good = date_ok + symptoms_ok & test_ok & contact_ok & travel_ok
  )

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

joined <- 
  full_join(
    hs,
    vax,
    by = c("first_name", "last_name")
  ) %>% 
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
  )

write_sheet(
  joined,
  SS_DEST,
  sheet = as.character(DATE)
)


