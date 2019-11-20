## code to prepare `wapo` dataset

# Connecting to twitter:
library(twitteR)
library(rtweet)

consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#### Washington Post:

# Extracting data from WaPo's Twitter account:

# extracted Sept. 10, 2019
# tweets.wapo<- userTimeline("washingtonpost", n = 1000, excludeReplies = TRUE, includeRts=FALSE)
# save(tweets.wapo, file="wapo-raw-091019")
load(file="wapo-raw-091019")

wapo <-  twListToDF(tweets.wapo)[c("text", "retweetCount", "favoriteCount", "created")]
str(wapo)

# Remove URLs, carriage returns
wapo$text <- gsub("http[^[:space:]]*", "", wapo$text )
wapo$text <- gsub("\n", " ", wapo$text)

head(wapo)

usethis::use_data(wapo, overwrite = TRUE)
