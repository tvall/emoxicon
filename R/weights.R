#' Emotion Weights Data
#'
#' A matrix containing words (n = 175,592) and the emotion weights associated with each word.
#' This dataset is the raw version of the 'DepecheMood++' lexicon developed by
#' Araque, Gatti, Staiano, and Guerini (2018). For proper scoring, text should not be
#' stemmed prior to using this lexicon. This version of the lexicon does not
#' rely on part of speech tagging.
#'
#'
#'
#' @name weights
#'
#' @docType data
#'
#' @usage data(weights)
#'
#' @format A data frame with 175592 rows and 9 columns.
#' \describe{
#'    \item{word}{An entry in the lexicon, in English}
#'    \item{AFRAID, AMUSED, ANGRY, ANNOYED, DONT_CARE, HAPPY, INSPIRED, SAD}{The emotional category.
#'    Each column contains the probability weight of that category being associated with that word.}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data('weights')
#'
#' @references
#' Araque, O., Gatti, L., Staiano, J., and Guerini, M. (2018).
#' DepecheMood++: a Bilingual Emotion Lexicon Built Through Simple Yet Powerful Techniques.
#' \emph{ArXiv} preprint is available at https://arxiv.org/abs/1810.03660.
#'

NULL
#----
