library(tidyverse)
library(jsonlite)
library(tidyjson)
library(stringr)
library(furrr)
library(googlesheets4)
plan("multisession", workers = availableCores() - 1)


read_spread_json <- function(id){
  spread_all(jsonlite::read_json(paste0("/mnt/ironwolf_4tb/maurais/json/", id, ".json"))) %>%

    mutate(id = id,
           url =
             str_replace(
               str_sub(src,3), # enlever les 2 premières lettres
               "\\?[^\\?]*$", "" #  un?, suivi du plus de caractère possible sauf un autre ?, bref le dernier ? et tout ce qui suit
             )
    ) %>%
    select(id, everything())
}


download_json <- function(id){
  download.file(paste0("https://api.podboxx.com/api//embed/", id),
                destfile = paste0("/mnt/ironwolf_4tb/maurais/json/",
                                  id, ".json")
  )
  return(TRUE)
}


download_mp3 <- function(url){
  download.file(url, destfile = paste0("/mnt/ironwolf_4tb/maurais/audio/", basename(url)))
  return(TRUE)
}
possibly_read_json = possibly(.f = read_spread_json, otherwise = "Error")
possibly_download_json <- possibly(.f = download_json, otherwise = "Error")
possibly_download_mp3 <- possibly(.f = download_mp3, otherwise = "Error")

# get last json saved in the csv
old_csv <- read_csv("historique_choi.csv") %>%
  mutate(mp3_title_date = as.character(mp3_title_date))
last_json <- max(old_csv$id)
#get last downloaded  json
#last_json <- max(as.numeric(stringr::str_replace(list.files("/mnt/ironwolf_4tb/maurais/json"), ".json", "")))


# try next 1000
json_id_to_try <- seq(last_json +1, last_json+1000)
download_all_json <- furrr::future_map(json_id_to_try, possibly_download_json)
read_all_json <- furrr::future_map(json_id_to_try, possibly_read_json)
worked <- map_lgl(read_all_json, function(x){is_tibble(x)})
ready_for_csv <- bind_rows(read_all_json[worked]) %>% # only keep rows for which a json exist
  janitor::clean_names() %>%
  as_tibble() %>%
  select(-json)

mp3_urls_in_new_json <-unique(ready_for_csv$url)
mp3_in_new_json <-  basename(mp3_urls_in_new_json)
mp3_already_downloaded <- list.files("/mnt/ironwolf_4tb/maurais/audio/")
missing_files <- mp3_urls_in_new_json[!(mp3_in_new_json %in% mp3_already_downloaded)]
download_results <- furrr::future_map(missing_files, possibly_download_mp3, .progress= TRUE)  # download new mp3s

# add id3 tag information

mp3_to_id <- tibble(url = missing_files[unlist(download_results)]) %>%
  mutate(mp3 = basename(url))

mp3_files <- paste0("/mnt/ironwolf_4tb/maurais/audio/",mp3_to_id$mp3)

get_id3tag <- function(file){
  system( paste0("id3info " , file), intern = TRUE)
}

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

new_csv <-   ready_for_csv %>% left_join(left_join_table) %>%
  mutate(mp3_title_date =as.character(mp3_title_date))



#write_rds(read_all_json, "read_all_json.rds")
historique_choi <-
  bind_rows(old_csv,
            new_csv
  )

write_csv(historique_choi, paste0("historique_choi_", as.character(Sys.Date()), ".csv"))
write_csv(historique_choi, "historique_choi.csv")
#
# ss2 <- gs4_create(
#   "griffe-a-jambon",
#   sheets = c("historique")
# )


sheet_write(
  historique_choi %>%
    filter(type =="podcast") %>%
    mutate(duration = as.character(duration)) %>%
    select(id, document_id, channel_title, title, duration, desc, mp3_title_date, url  )
  ,
  ss= "1ZyKtcac0ILOw_TTazrKSAABT8iXi9Bi2sKoQA5EVRyI",
  sheet = "historique")