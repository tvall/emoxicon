## code to prepare `weights` dataset

weights <- read.table("data-raw/DepecheMood_english_lemma_full.tsv", sep="\t", header=TRUE, comment.char = "", stringsAsFactors = FALSE)

weights<- weights[c("X", "AFRAID", "AMUSED", "ANGRY", "ANNOYED", "DONT_CARE", "HAPPY",
                                      "INSPIRED", "SAD")]
colnames(weights) <- c("word", "AFRAID", "AMUSED", "ANGRY", "ANNOYED", "DONT_CARE", "HAPPY",
                                "INSPIRED", "SAD")


class(weights) <- append(class(weights),"emoxicon")

usethis::use_data(weights, overwrite = TRUE)
