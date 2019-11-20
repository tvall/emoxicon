## code to prepare `weights` dataset

weights <- read.table("data-raw/DepecheMood_english_lemma_full.tsv", sep="\t", header=TRUE, comment.char = "", stringsAsFactors = FALSE)

weightsfreq <- subset(weights, freq>=10)

weightsfreq<- weightsfreq[c("X", "AFRAID", "AMUSED", "ANGRY", "ANNOYED", "DONT_CARE", "HAPPY",
                                      "INSPIRED", "SAD")]
colnames(weightsfreq) <- c("word", "AFRAID", "AMUSED", "ANGRY", "ANNOYED", "DONT_CARE", "HAPPY",
                                "INSPIRED", "SAD")

# usethis::use_data(weightsfreq, overwrite = TRUE)
