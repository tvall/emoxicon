#' Return Rasch fit statistics
#'
#' This function calculates Rasch model person and item fit statistics.
#'
#' @param model The object outputted by the \code{emoxicon} \code{rasch} function.
#'
#' @param groups Logical. Should summary fit statistics be calculated across
#' the individual group models, if present? The default is FALSE. Running fit statistics
#' on many models can be slow.
#'
#'
#' @author Tara Valladares <tls8vx at virginia.edu>
#'
#' @importFrom eRm RM person.parameter personfit itemfit personMisfit
#'
#'

rasch_fit <- function(model, groups = FALSE){

  if(!inherits(model, what = "emoxrasch")){
    stop("The rasch_fit function requires an emoxicon object.")
  }

  pparameters <- eRm::person.parameter(model$full_model)

  pfit <- eRm::personfit(pparameters)
  ifit <- eRm::itemfit(pparameters)
  pmis <- eRm::PersonMisfit(pparameters)


  if(groups){

    # Run fit functions on all models individually
    manymods_pparameters <- lapply(model$group_models, function(x){
      eRm::person.parameter(x$model)
    })

    manymods_pfit <- lapply(1:length(model$group_models), function(x){
      eRm::personfit(manymods_pparameters[[x]])
    })
    manymods_ifit <- lapply(1:length(model$group_models), function(x){
      eRm::itemfit(manymods_pparameters[[x]])
    })
    manymods_pmis <- lapply(1:length(model$group_models), function(x){
      eRm::PersonMisfit(manymods_pparameters[[x]])
    })
    manymods_pmis_total <- list("P-Misfit by Model in Percents" = sapply(manymods_pmis, function(x){x$PersonMisfit}))

    manymods_pmis_total

    # save item names
    z <- names(ifit$i.fit)

    # look at misfitting items within individual models
    manymods_ifit_total <- sapply(1:length(manymods_ifit), function(i){

      p <- round(pchisq(manymods_ifit[[i]]$i.fit,
                        df=manymods_ifit[[i]]$i.df-1, lower.tail=FALSE),3)

      items <- as.data.frame(t(unlist(c(model$group_models[[i]]$group, p))),
                             row.names = model$group_models[[i]]$group)
      colnames(items)[1] <- "model"

      if(ncol(items) < (length(z)+1)){
        items[,z[which(!z%in% colnames(items))]]<-NA
      }
      items

    }, simplify = F)

    manymod_ifit_total<-do.call(rbind, manymods_ifit_total)

    output <- list(
      full_model = list(personfit = list(pfit_statistics = pfit,
                           percent_pmisfit = pmis),
          itemfit = ifit
      ),
      group_models = list(group_personfit = list(group_pfit_statistics = 1,
                                                 percent_pmisfit = manymods_pmis_total),
                          group_itemfit = 1)
    )
  } else{
    output <- list(personfit = list(pfit_statistics = pfit,
                                    misfit = pmis),
                   itemfit = ifit
    )
  }

}
