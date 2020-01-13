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


rasch <- function(scores,
                  groups = NULL,
                  return.models = TRUE,
                  ...) {
  #set up error list
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


  # Category Ordering by groups

  if (exists("groups")) {
    #count lengths; Rasch models should have a minimum of 30 observations
    group_lengths <-
      sapply(unique(groups), function(x) {
        NROW(scores[which(groups == x), ])
      })
    if (sum(group_lengths < 30) > 0) {
      message("Rasch models were not run for groups with less than 30 observations")
    }
    g30 <- unique(groups)[group_lengths >= 30]

    # function for individual dichtomoziation and RM
    indv_model <-
      function(x) {
        group <- apply(scores[which(groups == x), ], 2,
                       function(x) {
                         ifelse(x <= mean(x, na.rm = TRUE), 0, 1)
                       })
        eRm::RM(group)
      }

    # run individual models
    rasch_groups <-
      lapply(g30, function(x)
        c(group = x, tryCatch.W.E(indv_model(x))))

    # index issues
    err <-
      which(sapply(rasch_groups, function(x) {
        inherits(x[["model"]], "error")
      }))
    warn <-
      which(sapply(rasch_groups, function(x) {
        inherits(x[["warning"]], "warning")
      }))

    dif <- Reduce(function(x, y)
      merge(x, y, all = TRUE),
      lapply(1:length(rasch_groups), function (x) {
        t(as.data.frame(c(x, -1 * (
          rasch_groups[[x]][["model"]]$betapar
        ))))
      }))[-1]# negative b/c these are betas

    # Due to how the the mean spit is conducted, if there is an item
    # with complete 0/full responses, it will always be a complete 0 line.

    mes <-
      stringr::str_extract(
        sapply(warn, function(x)
          rasch_groups[[x]][["warning"]][["message"]]),
        "(\nThe following items were excluded due to complete 0/full responses:\n).*"
      )
    mes <-
      stringr::str_extract_all(mes, gsub(", ", "|", toString(colnames(scores))),
                               simplify = FALSE)

    for (i in 1:nrow(dif[warn, ])) {
      dif[warn, ][i, paste("beta", mes[[i]])] <- 999
    }

    dif.order <-
      as.data.frame(t(apply(
        dif, 1, rank, ties.method = "random", na.last = "keep"
      )))
    row.names(dif) <- g30
    colnames(dif.order) <-
      gsub(pattern = "beta ", x = colnames(dif.order), "")
  }

  output <- list(full_model = rasch_fit, category_order = dif.order)
  if (return.models == TRUE) {
    output$group_models <- rasch_groups
  }
  output
}
