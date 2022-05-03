## code to prepare `emotion` dataset
library(dplyr)

emotion.lexicon.token <- read.table("DepecheMood_english_token_full_2022.tsv", sep="\t",
                                    header=TRUE, comment.char = "", stringsAsFactors = FALSE)

#Extracting the emotion with the highest frequency
dm_cols <- c("AFRAID", "AMUSED", "ANGRY", "ANNOYED", "DONT_CARE", "HAPPY",
             "INSPIRED", "SAD")
emotions<- emotion.lexicon.token
emotions[dm_cols] <- as.data.frame(t(apply(emotions[dm_cols] , 1, function(x){
  ifelse(x == max(x), 1, 0)
})))

emotions[dm_cols][which(rowSums(emotions[dm_cols]) == 7),] <- rep(0, length(dm_cols))

emotions <- emotions %>%
  rename(word = X)

# Export

class(emotions) <- append(class(emotions),"emoxicon")
usethis::use_data(emotions, overwrite = TRUE)
