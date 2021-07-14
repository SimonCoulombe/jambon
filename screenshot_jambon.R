#devtools::install_github("gadenbuie/tweetrmd")

library(magrittr, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(readr, quietly = TRUE)
library(rtweet, quietly = TRUE)
library(purrr, quietly = TRUE)
#plan("multisession", workers = 2)
auth_as(
  rtweet_bot(
    api_key =Sys.getenv("perroquetdejeff_api_key"),
    api_secret =   Sys.getenv("perroquetdejeff_api_secret_key"),
    access_token = Sys.getenv("perroquetdejeff_access_token"),
    access_secret = Sys.getenv("perroquetdejeff_access_token_secret")
  )
)
#
# delete_all <- function(userid){
#
#   destroyme <- rtweet::get_timeline(userid,n=2000)
#   if (length(destroyme$status_id)>0) map(destroyme$status_id, ~ post_destroy( .x))
# }
# delete_all("perroquetdejeff")
#
# get_screenshot_and_tweet <- function(id){
#   tweet_screenshot(tweet_url("jefffillion", id), file = paste0("/home/simon/git/jambon/screenshots/",  id, ".png"))
#   post_tweet(
#     status = id,
#     media = paste0("/home/simon/git/jambon/screenshots/",  id, ".png")
#   )
# }

jambon_frais1 <- get_timeline(user = "jefffillionfun", n = 20000) %>%
  mutate(across(where(is.list), as.character))

jambon_frais2  <- get_timeline(user = "e_duhaime", n = 20000) %>%
  mutate(across(where(is.list), as.character))


# chr_vars <- jambon_frais %>% select_if(is_character) %>% colnames()
# num_vars <- jambon_frais %>% select_if(is_numeric) %>% colnames()
# lgl_vars <- jambon_frais %>% select_if(is_logical) %>% colnames()

jambon_frais <- bind_rows(jambon_frais1, jambon_frais2)
vieux_jambon <- read_csv("/home/simon/git/jambon/tweet_jeff.csv",
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

jambon_total <- bind_rows(
  jambon_frais %>% filter(!(status_id %in%  vieux_jambon$status_id))   , # new jambon
  vieux_jambon  # vieux jambon
) %>%
  arrange(desc(status_id))

jambon_total %>%  write_csv(., paste0("tweet_jeff", as.character(Sys.Date()),".csv"))
jambon_total %>%  write_csv(., "tweet_jeff.csv")

jambon_deja_screenshotte <- basename(list.files("/home/simon/git/jambon/screenshots")) %>% stringr::str_sub(1,-5L)
jambon_a_screenshotter <- jambon_total[ !(jambon_total$status_id %in% jambon_deja_screenshotte),]
if(nrow(jambon_a_screenshotter > 0)){

  purrr::map2(jambon_a_screenshotter$status_id,
                     jambon_a_screenshotter$user_id,
                     ~ tweetrmd::tweet_screenshot(
                       tweetrmd::tweet_url(.y, .x),
                       file = paste0("/home/simon/git/jambon/screenshots/",  .x, ".png"))
                     )
} else{message(Sys.Date(), Sys.time()," - no new tweet to screenshot")}


# ## datetime plus vieux tweet disponible
# oldest_available_datetime <- jambon_frais %>% summarise(old = min(created_at))
#
# manually_deleted_tweets <- jambon_total %>%
#   filter(as.numeric(created_at) > as.numeric(oldest_available_datetime))  %>% # should be available
#   filter(!(status_id %in% jambon_frais$status_id)) # but isnt
#
