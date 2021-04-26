# une option c'est sauvegarder le code blockquote que me retourne publish.twitter dans un html
# ouvrir le html et sauvegarder dans uns pdf.. mais c'est de l'ouvrage.
# https://checkoway.net/musings/tweet-images/


#devtools::install_github("gadenbuie/tweetrmd")
library(tweetrmd)


library(tidyverse)
#library(RSelenium)
#library(magick)
library(rtweet)
#https://developer.twitter.com/en/portal/projects-and-apps
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

get_screenshot_and_tweet <- function(id){
  # #url=paste0("https://twitter.com/Jefffillion/status/", id)
  # url=paste0("https://publish.twitter.com/?query=https%3A%2F%2Ftwitter.com%2Fjefffillion%2Fstatus%2F", id, "&widget=Tweet")
  #
  # remdr$navigate(url)
  # Sys.sleep(3)
  # remdr$screenshot(display = TRUE)
  # remdr$screenshot(file = "/home/simon/git/jambon/test.png")
  #
  # #ele = remdr$findElement(using = "xpath",'//*[contains(concat( " ", @class, " " ), concat( " ", "r-184en5c", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "css-1dbjc4n", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "css-1dbjc4n", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "css-1dbjc4n", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "css-1dbjc4n", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "css-1dbjc4n", " " ))]//div//div[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "r-1ny4l3l", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "r-i023vh", " " ))]')
  # #ele = remdr$findElement(using = "xpath",'//*[contains(concat( " ", @class, " " ), concat( " ", "r-i023vh", " " ))]')
  # ele = remdr$findElement(using = "xpath",'//*[(@id = "twitter-widget-0")]')
  #
  # eleLocation  <- ele$getElementLocation()
  #
  # frink <- image_read("/home/simon/git/jambon/test.png")
  # image_info(frink)
  # #eleLocation$height
  # cropped  <- image_crop(frink, paste0(eleLocation$width,"x",eleLocation$height,"+",eleLocation$x, "+",eleLocation$y ))
  # image_crop(frink, paste0(eleLocation$width, "x",(image_info(frink)$height - eleLocation$y),"+",eleLocation$x, "+", eleLocation$y))
  # cropped
  # image_write(cropped , "/home/simon/git/jambon/cropped.png")



  tweet_screenshot(tweet_url("jefffillion", id), file = paste0("/home/simon/git/jambon/screenshots/",  id, ".png"))

  post_tweet(
    status = id,
    media = paste0("/home/simon/git/jambon/screenshots/",  id, ".png")
  )

}


jambon_frais <- get_timeline(user = "jefffillion", n = 2000)

chr_vars <- jambon_frais %>% select_if(is_character) %>% colnames()

jambon_retweete <- get_timeline(user = "perroquetdejeff") %>% pull(text)
jambon_save <- read_csv("tweet_jeff.csv" ) %>%
  mutate(across(all_of(chr_vars), as.character))


jambon_total <- bind_rows(
  jambon_frais %>% filter(!(status_id %in%  jambon_save$status_id))  %>% mutate(across(where(is.list), as.character))# , # new jambon
  #jambon_save  # vieux jambon
) %>%
  arrange(desc(status_id))



jambon_total %>% mutate(across(where(is.list), as.character)) %>% write_csv(., paste0("tweet_jeff", as.character(Sys.Date()),".csv"))
jambon_total %>% mutate(across(where(is.list), as.character)) %>% write_csv(., "tweet_jeff.csv")


jambon_a_tweeter <- jambon_total %>%
  filter(!(status_id  %in% jambon_retweete)) %>%
  arrange(status_id) %>%
  pull(status_id)



if (length(jambon_a_tweeter) > 0 ){


  # start selenium
  #http://ritsokiguess.site/docs/2018/09/01/scraping-icelandic-soccer-results-with-rvest-and-selenium/
  #sudo docker run -d  --restart=unless-stopped --name=selenium -p 4445:4444 selenium/standalone-firefox
#
#   remdr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
#                                    port = 4445L,
#                                    browserName = "firefox")
#   remdr$open()
#   Sys.sleep(3)

  # map that shit
  map(jambon_a_tweeter, get_screenshot_and_tweet)
#  remdr$quit()
}





