## code to prepare `trolls` dataset

load("/Users/tara/Box/Troll Tweets/tt_individualmodels/trolls")
removeRetweets <- function(x){gsub("^rt.*", "rt", x)} # remove reposts

# Remove retweets
trolls.clean <- lapply(trolls,function(x){
  x<-x[which(x$retweet == 0),]
} )

# clean data
trolls.clean <- lapply(trolls.clean, function(y){
  content<- y$content
  content<- stringi::stri_trans_general(content, "latin-ascii")
  content<- gsub("&amp", "and", content)
  content<- tolower(content)
  content<- removeRetweets(content)

  results <- cbind(content, y[,c("author", "publish_date", "followers", "updates","account_type")])
  results<- results[which(results$account_type=="Left"|
                            results$account_type=="Right"),]
})

trolls<-do.call(rbind, trolls.clean)
trolls$content<- as.character(trolls$content)

usethis::use_data(trolls, overwrite = TRUE)
