#' Estimation of Rasch Models for Emoxicon
#'
#' @description Runs a Rasch model using the emotion scores from \code{emoxicon}.
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
#' @param return_models Logical. Should the model for each group be returned? Default is TRUE.
#' Setting this value to FALSE will reduce object size, but only the category orders will
#' be returned.
#'
#' @param ... Additional arguments to be passed to \code{\link[eRm]{RM}}
#'
#' @author Tara Valladares <tls8vx at virginia.edu>, Hudson F. Golino <hfg9s at virginia.edu>
#'
##' @examples
#' # Load the tinytrolls data
#' data(tinyTrolls)
#'
#' # Use the emoxicon function
#' \dontrun{
#' emotions.tinytrolls <- emoxicon(text = tinyTrolls$content, lexicon = emotions)
#' # Recode the variables to 0 (below mean) or 1 (equal to or above mean)
#' emotions.rasch <- as.data.frame(apply(emotions.tinytrolls[,-c(1:2)],2,
#' function(x) ifelse(x<mean(x),0,1)))
#' # Apply the Rasch Model
#' rm.tinytrolls <- rasch(scores = emotions.tinytrolls)
#' }
#' @seealso \code{\link{emoxicon}}, where the emotion scores are generated and \code{\link{rasch_fit}} to calculate Rasch fit statistics.
#'
#' @references
#'
#' Mair, P., & Hatzinger, R. (2007).
#' Extended Rasch modeling: The eRm package for the application of IRT models in R.
#' \emph{Journal of Statistical Software}, \emph{20(9)},1-20.
#' doi:\href{http://www.jstatsoft.org/v20/i09}{10.18637/jss.v020.i09}
#'
#' @importFrom stats na.omit
#'
#' @export
# Rasch Function:
# Updated 04.06.2020
rasch <- function(scores, groups, return_models = TRUE,...) {

  errors <- vector("list")

  if (!missing("groups")) {
    if (length(groups) != nrow(scores)) {
      stop("The length of groups does not match the number of rows of data")
    }
  }

  scores <-
    scores[which(sapply(scores, class) == "numeric" |
                   sapply(scores, class) == "integer")]

  # check for & resolve dicotomization
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

  if (!missing("groups")) {
    category_output <- category_order(scores=scores, groups=groups,
                                      return_models = return_models)
    output <- list(full_model = rasch_fit,
                   category_order = category_output$category_order)

    if (return_models == TRUE) {
      output$group_models <- category_output$group_models
  }} else{
    output <- list(full_model = rasch_fit)
  }
  class(output) <- append(class(output),"emoxrasch")
  output
}
