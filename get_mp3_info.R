library(tidyverse)
library(jsonlite)
library(tidyjson)
library(stringr)
library(furrr)
plan("multisession", workers = availableCores() - 1)

old_csv <- read_csv("historique_choi.csv")

mp3_to_id <- old_csv %>%
  distinct(url) %>%
  mutate(mp3 = basename(url)) %>%
  filter(!is.na(mp3))


mp3_files <- paste0("/mnt/ironwolf_4tb/maurais/audio/",mp3_to_id$mp3)

get_id3tag <- function(file){
  system( paste0("id3info " , file), intern = TRUE)
}
#

id3tagz <- future_map(mp3_files, get_id3tag, .progress = TRUE)
titles <-  map(id3tagz, function(x){
  whichone <- which(str_detect(x, "=== TIT2"))
  if (length(whichone)>0) {
    x[[whichone]]
    } else NA_character_
  }
)
clean_titles <- str_replace_all(titles, "===\\s+TIT2\\s+\\(Title\\/songname\\/content\\s+description\\):\\s+", "")
title_date <- str_extract(clean_titles, "\\d\\d\\d\\d-\\d\\d-\\d\\d")


mp3_to_id$mp3_title <- clean_titles
mp3_to_id$mp3_title_date <- title_date

left_join_table <- mp3_to_id %>% select(-mp3)

new_csv <- old_csv %>%
  left_join(left_join_table)

write_csv(new_csv, "historique_choi.csv")
