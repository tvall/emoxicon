## code to prepare `emotion` dataset

emotion.lexicon.lemma <- read.table("data-raw/DepecheMood_english_lemma_full.tsv", sep="\t", header=TRUE, comment.char = "", stringsAsFactors = FALSE)

#Extracting the emotion with the highest frequency
emotions <- data.frame(words = emotion.lexicon.lemma[,1])

for(i in 1:nrow(emotion.lexicon.lemma)){
  emotions[i,2] <- names(which.max(emotion.lexicon.lemma[i,c(-1,-10) ]))
  emotions[i,3] <- emotion.lexicon.lemma[i,which.max(emotion.lexicon.lemma[i,c(-1,-10) ])+1]
}

emotions[,1] <- as.character(emotions[,1])
emotions[,2] <- as.factor(emotions[,2])
emotions[,3] <- 1

colnames(emotions) <- c("word","emotion","value")
emotions<-reshape2::dcast(emotions, word~ emotion, value.var = "value")
emotions[is.na(emotions)] <- 0

class(emotions) <- append(class(emotions),"emoxicon")

usethis::use_data(emotions, overwrite = TRUE)
