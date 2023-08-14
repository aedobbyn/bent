library(googlesheets4)
library(tidyverse)

SS <- "https://docs.google.com/spreadsheets/d/1J6dv50C9kCEVqnUVQUPoL5D1EUxOHPM3unLg5LrtHps/edit#gid=129998304"

raw <- googlesheets4::read_sheet(SS, sheet = "Sheet2")

names(raw) <- c("email", "text")

dat <- 
  raw %>% 
  tidyr::drop_na(email) %>% 
  mutate(
    date = str_split(text, pattern = ",")
  ) %>% 
  tidyr::unnest(date) %>%
  select(-text) %>% 
  mutate(
    date = str_squish(date)
  )

grouped <- 
  dat %>% 
  count(date, sort = TRUE)

googlesheets4::write_sheet(dat, ss = SS, sheet = "clean")
googlesheets4::write_sheet(grouped, ss = SS, sheet = "counts")
