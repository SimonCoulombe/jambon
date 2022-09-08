library(tidyverse)
library(jsonlite)
library(tidyjson)
library(stringr)
library(furrr)
plan("multisession", workers = availableCores() - 1)
bind_rows(
  spread_all(jsonlite::read_json("https://api.podboxx.com/api//embed/18")),
  spread_all(jsonlite::read_json("https://api.podboxx.com/api//embed/34100"))
) %>%
  View()

read_spread_json <- function(id){
  #message(id)
  #spread_all(jsonlite::read_json(paste0("https://api.podboxx.com/api//embed/", id))) %>%
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

possibly_read_json = possibly(.f = read_spread_json, otherwise = "Error")


download_json <- function(id){
  #message(id)
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

possibly_download_json <- possibly(.f = download_json, otherwise = "Error")
possibly_download_mp3 <- possibly(.f = download_mp3, otherwise = "Error")

test <- c(18:34100)

download_all_json <- furrr::future_map(test, possibly_download_json)

#downloaded <- test[(unlist(download_all_json)== TRUE)]
#read_all_json <- read_all_json[downloaded]
read_all_json <- furrr::future_map(test, possibly_read_json)
write_rds(read_all_json, "read_all_json.rds")

worked <- map_lgl(read_all_json, function(x){is_tibble(x)})
binded_json <- bind_rows(read_all_json[worked]) %>% janitor::clean_names()

for_csv <- binded_json %>% as_tibble() %>% select(-json)
write_csv(for_csv, "for_csv.csv")
for_csv <- read_csv("for_csv.csv")
unique_urls <- unique(for_csv$url)
unique_basenames <- basename(unique_urls)
list_files <- list.files("/mnt/ironwolf_4tb/maurais/audio/")
missing_files <- unique_basenames[!(unique_basenames %in% list_files)]
download_results <- furrr::future_map(missing_files, possibly_download_mp3, .progress= TRUE)


for_csv %>% select(id, document_id, media_id, type, channel_id,  channel_title,  title, link, duration, desc, url) %>%
  write_csv("liste_choix.csv")

#
#
# mp3_list <- list.files("/mnt/ironwolf_4tb/maurais/audio", full.names= TRUE)
#
# get_id3tag <- function(file){
#   system( paste0("id3info " , file), intern = TRUE)
# }
#
# library(purrr)
# library(furrr)
# id3tags <- lapply(head(mp3_list), get_id3tag)
# id3tagz <- future_map(mp3_list, get_id3tag)
#
# mytags <- map(head(mp3_list, function(file){ system( paste0("id3info " , file), intern = TRUE)}))
# get_id3tag("/mnt/ironwolf_4tb/maurais/audio/po0aaf95d4d2.mp3")
#
# map(c(1,2,3) ,function(x){ x+ 3})
