library(tidyverse)
library(jsonlite)
library(tidyjson)
library(stringr)
library(furrr)
library(googlesheets4)
plan("multisession", workers = availableCores() - 1)


#path <- "/disks/ironwolf_4tb/maurais/json/"
path_root <- "/mnt/simon/maurais/"

read_spread_json <- function(id){
  spread_all(jsonlite::read_json(paste0(path_root, "json/", id, ".json"))) %>%

    mutate(id = id,
           src = as.character(src),
           url =
             str_replace(
               src, #str_sub(src,3), # enlever les 2 premières lettres (obsolete, avant le as.character(src) il y avait des caractères avant http.)
               "\\?[^\\?]*$", "" #  un?, suivi du plus de caractère possible sauf un autre ?, bref le dernier ? et tout ce qui suit
             )
    ) %>%
    select(id, everything())
}


download_json <- function(id){
  download.file(paste0("https://api.podboxx.com/api//embed/", id),
                destfile = paste0(path_root, "json/",
                                  id, ".json")
  )
  return(TRUE)
}


download_mp3 <- function(url){
  download.file(url, destfile = paste0(path_root, "audio/", basename(url)))
  return(TRUE)
}
possibly_read_json = possibly(.f = read_spread_json, otherwise = "Error")
possibly_download_json <- possibly(.f = download_json, otherwise = "Error")
possibly_download_mp3 <- possibly(.f = download_mp3, otherwise = "Error")

# get last json saved in the csv
old_csv <- read_csv(paste0(path_root, "historique_choi.csv")) %>%
  mutate(mp3_title_date = as.character(mp3_title_date),
         duration = as.character(duration))
last_json <- max(old_csv$id) # 45076 le 6 septembre   -- 45100 avant ma patch de 10000 du 6 septembre
#get last downloaded  json
#last_json <- max(as.numeric(stringr::str_replace(list.files("/disks/ironwolf_4tb/maurais/json"), ".json", "")))


# try next 1000
# 45077 45078 45079 45080 45081 45082 45083 45084 45085 45086
json_id_to_try <- seq(last_json +1, last_json+10000)
download_all_json <- furrr::future_map(json_id_to_try, possibly_download_json)
read_all_json <- furrr::future_map(json_id_to_try, possibly_read_json)
worked <- map_lgl(read_all_json, function(x){is_tibble(x)})

if(sum(worked)>0){
  ready_for_csv <- bind_rows(read_all_json[worked]) %>% # only keep rows for which a json exist
    janitor::clean_names() %>%
    as_tibble() %>%
    select(-json)

  mp3_urls_in_new_json <-unique(ready_for_csv$url)
  mp3_in_new_json <-  basename(mp3_urls_in_new_json)
  mp3_already_downloaded <- list.files(paste0(path_root, "audio/"))
  already_downloaded_files <- mp3_urls_in_new_json[(mp3_in_new_json %in% mp3_already_downloaded)]
  missing_files <- mp3_urls_in_new_json[!(mp3_in_new_json %in% mp3_already_downloaded)]
  download_results <- furrr::future_map(missing_files, possibly_download_mp3, .progress= TRUE)  # download new mp3s, takes a while

  # add id3 tag information

  mp3_to_id <- tibble(
#    url = c(already_downloaded_files, missing_files[unlist(download_results)]) ##  6 septembre-- ouain des fois download_results j'ai "Error". j'imagine que ce serait mieux un "FALSE??
    url = c(already_downloaded_files, missing_files[unlist(download_results %in% TRUE)]) # 6 septembre remplacé avec un %in% TRUE pour gérer le "error"..
  ) %>%
    mutate(mp3 = basename(url))

  mp3_files <- paste0(path_root, "audio/",mp3_to_id$mp3)

  get_id3tag <- function(file){
    system( paste0("id3info " , file), intern = TRUE) # 6 septembre: nécessite sudo  apt-get install libid3-tools
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

  write_csv(historique_choi, paste0(path_root,"historique_choi_", as.character(Sys.Date()), ".csv"))
  write_csv(historique_choi, paste0(path_root, "historique_choi.csv"))
  # debutétait déja comment avant 6 septembre
  # ss2 <- gs4_create(
  #   "griffe-a-jambon",
  #   sheets = c("historique")
  # )
  # fin  était déja comment avant 6 septembre

  # début mis en commentaire le 6 septembre
  # sheet_write(
  #   historique_choi %>%
  #     filter(type =="podcast", channel_title %in% c("JeffFillion.com", "RADIO X BEST OF")) %>%
  #     mutate(duration = as.character(duration)) %>%
  #     arrange(desc(id), document_id) %>%
  #     select(channel_title, title, duration, desc, date = mp3_title_date, url  )
  #   ,
  #   ss= "1ZyKtcac0ILOw_TTazrKSAABT8iXi9Bi2sKoQA5EVRyI",
  #   sheet = "historique")
  #
  # sheet_write(
  #   historique_choi %>%
  #     filter(type =="podcast") %>%
  #     mutate(duration = as.character(duration)) %>%
  #     filter(str_detect(toupper(desc), "DUHAIME") | str_detect(toupper(title), "DUHAIME")) %>%
  #     arrange(desc(id), document_id) %>%
  #     select(channel_title, title, duration, desc, date = mp3_title_date, url  )
  #   ,
  #   ss= "1ZyKtcac0ILOw_TTazrKSAABT8iXi9Bi2sKoQA5EVRyI",
  #   sheet = "duhaime")
  # fin mis en commentaire le 6 septembre
}
