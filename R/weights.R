#' Emotion Weights Data
#'
#' A matrix containing words (n = 175,592) and the emotion weights associated with each word.
#' This dataset is the raw version of the 'DepecheMood++' token lexicon developed by
#' Araque, Gatti, Staiano, and Guerini (2018). For proper scoring, text should not be
#' stemmed prior to using this lexicon. This version of the lexicon does not
#' rely on part of speech tagging.
#'
#' Care should be taken when analyzing data with this version of the lexicon.
#' Rows are made up of probability weights and sum to 1.
#' This is therefore compositional data.
#' Compositional data commonly produces spurious correlations between variables in standard univariate and multivariate analyses
#' and should be handled with care (Greenacre, 2021; Pearson, 1896).
#' Row sums of probability weights are still compositional.
#' As far as we know, the impact of the compositional nature of DepecheMood++ has yet to be
#' addressed or its impact on results thuroughly examined.
#' For now, it is our belief that unless you wish to analyze your results with proper compositional methods,
#' most researchers are better off using the binary version of the token lexicon (`emotions`).
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
#' Greenacre, M. (2021). Compositional data analysis. Annual Review of Statistics and Its
#' Application, 8(1), 271–299. https://doi.org/10.1146/annurev-statistics-042720-124436
#'
#' Pearson, K. (1896). Mathematical contributions to the theory of evolution. — On a form of
#' spurious correlation which may arise when indices are used in the measurement of organs.
#' Proceedings of the Royal Society of London, 60 (359-367), 489–498.

NULL
#----
