## code to prepare `trolls` dataset

### not run ###
# trolls_raw <- vector("list", ndoc)
# for(i in 1:ndoc){
#   trolls_raw[[i]]<- read.csv(paste0("../russian-troll-tweets-master/IRAhandle_tweets_", i,".csv"),
#               stringsAsFactors = FALSE)
# }
# trolls<-vector("list",ndoc)
# for(i in 1:ndoc){
#   trolls[[i]]<- trolls_raw[[i]][which(trolls_raw[[i]]$language == "English"),]
# }
# save(data=trolls, file="trolls")


load("/Users/tara/Box/Troll Tweets/tt_individualmodels/trolls")

# Remove retweets
trolls.clean <- lapply(trolls,function(x){
  x<-x[which(x$retweet == 0),]
} )

# clean data
trolls.clean <- lapply(trolls.clean, function(y){
  content<- y$content
  content<- stringi::stri_trans_general(content, "latin-ascii")
  content<- gsub("&amp", "and", content)
  content<- gsub("http[^[:space:]]*", "", content) # remove links
  content<- tolower(content)
  content<- gsub("^rt.*", "", content) # remove reposts

  results<- cbind(content, y[,c("author", "publish_date", "followers", "updates","account_type")])
  results<- results[which(results$account_type=="Left"|
                            results$account_type=="Right"),]
  results<- subset(results, content !="") #remove empty tweets
})

trolls<-do.call(rbind, trolls.clean)
trolls$content<- as.character(trolls$content)

usethis::use_data(trolls, overwrite = TRUE)

set.seed(1)
tinyTrolls <- trolls[which(trolls$author%in%sample(unique(trolls$author), size=50)),]
usethis::use_data(tinytrolls, overwrite = TRUE)
