## code to prepare `weights` dataset

weights <- read.table("DepecheMood_english_token_full_2022.tsv", sep="\t",
                                    header=TRUE, comment.char = "", stringsAsFactors = FALSE)
weights <- weights %>%
  dplyr::rename(word = X)

class(weights) <- append(class(weights),"emoxicon")

usethis::use_data(weights, overwrite = TRUE)
