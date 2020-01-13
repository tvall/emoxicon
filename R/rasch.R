#' Estimation of Rasch Models for Emoxicon
#'
#' Runs a Rasch model using the emotion scores from \code{emoxicon}.
#' If the data is not already dichotomous, a mean split will be performed.
#'
#' @details
#' When the data is generated using \code{emoxicon} and the default emotions lexicon,
#' the entire emoxicon object can be entered.
#'
#'
#' @param scores A dataframe containing the emotion scores
#'
#' @param groups An optional vector of group identifers the same length as \code{scores}.
#' If provided, individual Rasch models will be run by group.
#'
#' @param return.models logical. Should all models for the groups be returned?
#'
#' @param ... Additional arguments to be passed to \code{\link[eRm]{RM}}
#'
#' @author Tara Valladares <tls8vx at virginia.edu>, Hudson F. Golino <hfg9s at virginia.edu>
#'
#' @examples
#'
#' \donttest{
#' #examples here
#' }
#' @seealso \code{\link{emoxicon}}, where the emotion scores are generated.
#'
#' @references
#'
#' @importFrom eRm RM
#' @importFrom stringr str_extract str_extract_all
#'
#' @export
#'
#'


rasch <- function(scores, groups = NULL, return.models = TRUE,
                  ...) {

  errors <- vector("list")

  if (!is.null("groups")) {
    if (length(groups) != nrow(scores)) {
      stop("The length of groups does not match the number of rows of data")
    }
  }

  scores <-
    scores[which(sapply(scores, class) == "numeric" |
                   sapply(scores, class) == "integer")]

  # check for/resolve dicotomization
  if (!all(apply(scores, 2, function(x) {
    length(unique(na.omit(x))) == 2}))) {
    scores_all <-
      apply(scores, 2, function(x) {
        ifelse(x <= mean(x, na.rm = TRUE), 0, 1)
      })
    message("Mean split performed to dichotomize data.")
  }

  # Run an overall Rasch model
  rasch_fit <- eRm::RM(scores_all, ...)

  # model summary, item measures, fit measures, ability measures, item fit,
  # plot functions for this class - the llr plot

  # separate function for the entropy/TI (can call it within this function)


  category_output <- category_output()

  output <- list(full_model = rasch_fit, category_order = category_output$dif.order)
  if (return.models == TRUE) {
    output$group_models <- category_output$rasch_groups
  }
  output
}
