#' Run individual rasch models based on group
#'
#' @author Tara Valladares <tls8vx at virginia.edu>
#' @noRd
#'
#' @importFrom eRm RM
#' @importFrom stringr str_extract str_extract_all
#'
#'

category_order <- function(scores, groups, return_models=TRUE){
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
    group_models <-
      lapply(g30, function(x){
        c(group = x, tryCatch.W.E(indv_model(x)))})

    # index issues
    err <-
      which(sapply(group_models, function(x) {
        inherits(x[["model"]], "error")
      }))
    warn <-
      which(sapply(group_models, function(x) {
        inherits(x[["warning"]], "warning")
      }))

    dif <- Reduce(function(x, y)
      merge(x, y, all = TRUE),
      lapply(1:length(group_models), function (x) {
        t(as.data.frame(c(x, -1 * (
          group_models[[x]][["model"]]$betapar
        ))))
      }))[-1]# negative b/c these are betas

    # Due to how the the mean spit is conducted, if there is an item
    # with complete 0/full responses, it will always be a complete 0 line.

    mes <-
      stringr::str_extract(
        sapply(warn, function(x)
          group_models[[x]][["warning"]][["message"]]),
        "(\nThe following items were excluded due to complete 0/full responses:\n).*"
      )
    if(length(mes>0)){
       mes <-
        stringr::str_extract_all(mes, gsub(", ", "|", toString(colnames(scores))),
                                 simplify = FALSE)
      for (i in 1:nrow(dif[warn, ])) {
        dif[warn, ][i, paste("beta", mes[[i]])] <- 999
      }
    }

    row.names(dif) <- g30

    category_order <-
      as.data.frame(t(apply(
        dif, 1, rank, ties.method = "random", na.last = "keep"
      )))
    colnames(category_order) <-
      gsub(pattern = "beta ", x = colnames(category_order), "")

    category_output <- list(category_order=category_order)

    if(return_models == TRUE){
      category_output$group_models <- group_models
    }
    category_output
}
