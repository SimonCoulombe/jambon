duhaime_liste <- historique_choi %>%
  filter(type =="podcast") %>%
  mutate(duration = as.character(duration)) %>%
  filter(str_detect(toupper(desc), "DUHAIME") | str_detect(toupper(title), "DUHAIME")) %>%
  mutate(filenames = str_replace(url, "cdn001.podboxx.com/audios/", "/disks/ironwolf_4tb/maurais/audio/")) %>%
  pull(filenames)

zip("duhaime.zip", files = duhaime_liste)
