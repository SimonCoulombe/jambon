library(tidyverse)
library(rtweet)

auth_as(
  rtweet_bot(
    api_key =Sys.getenv("perroquetdejeff_api_key"),
    api_secret =   Sys.getenv("perroquetdejeff_api_secret_key"),
    access_token = Sys.getenv("perroquetdejeff_access_token"),
    access_secret = Sys.getenv("perroquetdejeff_access_token_secret")
  )
)



vieux_jambon <- read_csv("tweet_jeff.csv",
                         col_types =
                           cols(
                             status_id = col_character(),
                             created_at = col_datetime(format = ""),
                             user_id = col_character(),
                             screen_name = col_character(),
                             text = col_character(),
                             source = col_character(),
                             display_text_width = col_integer(),
                             reply_to_status_id = col_character(),
                             reply_to_user_id = col_character(),
                             reply_to_screen_name = col_character(),
                             is_quote = col_logical(),
                             is_retweet = col_logical(),
                             favorite_count = col_integer(),
                             retweet_count = col_integer(),
                             quote_count = col_integer(),
                             reply_count = col_integer(),
                             hashtags = col_character(),
                             symbols = col_character(),
                             urls_url = col_character(),
                             urls_t.co = col_character(),
                             urls_expanded_url = col_character(),
                             media_url = col_character(),
                             media_t.co = col_character(),
                             media_expanded_url = col_character(),
                             media_type = col_character(),
                             ext_media_url = col_character(),
                             ext_media_t.co = col_character(),
                             ext_media_expanded_url = col_character(),
                             ext_media_type = col_character(),
                             ext_alt_text = col_character(),
                             mentions_user_id = col_character(),
                             mentions_screen_name = col_character(),
                             lang = col_character(),
                             quoted_status_id = col_character(),
                             quoted_text = col_character(),
                             quoted_created_at = col_datetime(format = ""),
                             quoted_source = col_character(),
                             quoted_favorite_count = col_integer(),
                             quoted_retweet_count = col_integer(),
                             quoted_user_id = col_character(),
                             quoted_screen_name = col_character(),
                             quoted_name = col_character(),
                             quoted_followers_count = col_integer(),
                             quoted_friends_count = col_integer(),
                             quoted_statuses_count = col_integer(),
                             quoted_location = col_character(),
                             quoted_description = col_character(),
                             quoted_verified = col_logical(),
                             retweet_status_id = col_character(),
                             retweet_text = col_character(),
                             retweet_created_at = col_datetime(format = ""),
                             retweet_source = col_character(),
                             retweet_favorite_count = col_integer(),
                             retweet_retweet_count = col_integer(),
                             retweet_user_id = col_character(),
                             retweet_screen_name = col_character(),
                             retweet_name = col_character(),
                             retweet_followers_count = col_integer(),
                             retweet_friends_count = col_integer(),
                             retweet_statuses_count = col_integer(),
                             retweet_location = col_character(),
                             retweet_description = col_character(),
                             retweet_verified = col_logical(),
                             place_url = col_character(),
                             place_name = col_character(),
                             place_full_name = col_character(),
                             place_type = col_character(),
                             country = col_character(),
                             country_code = col_character(),
                             geo_coords = col_character(),
                             coords_coords = col_character(),
                             bbox_coords = col_character(),
                             status_url = col_character()
                           )
)

jambon_deja_retweete <- get_timeline(user = "perroquetdejeff", n = 25000) %>%
  pull(text) %>%
  stringr::word(1)


jambon_a_tweeter <- vieux_jambon %>%
  filter(!(status_id  %in% jambon_deja_retweete)) %>%
  arrange(status_id) %>%
  pull(status_id)

if (length(jambon_a_tweeter) > 0 ){
  message(Sys.time(), " - on retweete")
  purrr::map(jambon_a_tweeter, ~  post_tweet(
    status = .x,
    media = paste0("/home/simon/git/jambon/screenshots/",  .x, ".png")
  ))
} else {message(Sys.time(), " - rien a  retweeter")}
