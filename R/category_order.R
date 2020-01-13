#' run individual rasch models based on group
#'
#' @author Tara Valladares <tls8vx at virginia.edu>
#' @noRd
#'
#'

category_order <- function(){
  # Category Ordering by groups
  if (exists("groups")) {
    group_lengths <-
      sapply(unique(groups), function(x) {
        NROW(scores[which(groups == x), ])
      })

    if (sum(group_lengths < 30) > 0) {
      message("Rasch models were not run for groups with less than 30 observations")
    }
    g30 <- unique(groups)[group_lengths >= 30]

    # individual dichtomoziation and RM
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

    category_output <- list(dif.order, rasch_groups)
    category_output
  }
}
