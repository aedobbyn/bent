library(tidyverse)
library(magrittr)

# Path to directory of  slack json messages
dir <- here::here("slack", "bent_2022_slack", "teampics")
# List all the json files in here
fls <- fs::dir_ls(dir)

# Path to where we'll save pics
pics_dir <- here::here("slack", "pics")
# Create the pics dir
fs::dir_create(pics_dir)

# Read in all the json files into one dataframe
tbl <- tibble()

for (i in 1:length(fls)) {
  message(glue::glue("{i}/{length(fls)}"))

  this <- jsonlite::fromJSON(fls[i])

  tbl %<>% bind_rows(this)
}

# Unnest the `files` column to grab just the `url_private_download`s
urls <-
  tbl %>%
  select(files) %>%
  tidyr::unnest() %>%
  select(url_private_download) %>%
  pull(url_private_download)

# Rexex of file extension (we need this for the vids to be saved correctly)
ext_reg <- "\\.gif|\\.jpe?g|\\.mov|\\.mp4|\\.png|\\.tiff|\\.webp"

# Download each of the urls and give it the right extension
# Just name it `i` for now
for (i in 1:length(urls)) {
  message(glue::glue("{i}/{length(urls)}"))

  if (is.na(urls[i])) next()

  ext <- urls[i] %>% str_extract(ext_reg)

  if (is.na(ext)) stop("No extension found")

  local_file <- glue::glue("{pics_dir}/{i}{ext}")

  download.file(urls[i], local_file)
}
