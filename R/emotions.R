#' Emotions Data
#'
#' A matrix containing words (n = 175,592) and the emotion category most frequently associated with each word.
#' This dataset is a dichotomized version of the 'DepecheMood++' token lexicon developed by
#' Araque, Gatti, Staiano, and Guerini (2018). For proper scoring, text should not be
#' stemmed prior to using this lexicon.
#' As this is the token lexicon, it does not require part of speech tagging.
#'
#'
#'
#' @name emotions
#'
#' @docType data
#'
#' @usage data(emotions)
#'
#' @format A data frame with 175,592 rows and 9 columns.
#' \describe{
#'    \item{word}{An entry in the lexicon, in English}
#'    \item{AFRAID, AMUSED, ANGRY, ANNOYED, DONT_CARE, HAPPY, INSPIRED, SAD}{The emotional category. All emotions contain either a 0 or 1. If the
#'    category is most likely to be associated with the word, it recieves a 1, otherwise, 0.
#'    Words are only associated with one category.}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data('emotions')
#'
#' @references
#' Araque, O., Gatti, L., Staiano, J., and Guerini, M. (2018).
#' DepecheMood++: a Bilingual Emotion Lexicon Built Through Simple Yet Powerful Techniques.
#' \emph{ArXiv} preprint is available at https://arxiv.org/abs/1810.03660.
#'

NULL
#----
