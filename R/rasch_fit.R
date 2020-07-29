#' Calculate Rasch fit statistics
#'
#' This function calculates Rasch person and item fit statistics.
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

    nom <- sapply(model$group_models, function(x){
      x$group
    })

    # Run fit functions on all models individually
    manymods_pparameters <- lapply(model$group_models, function(x){
      eRm::person.parameter(x$model)
    })

    manymods_pfit <- lapply(1:length(model$group_models), function(x){
      eRm::personfit(manymods_pparameters[[x]])
    })
    names(manymods_pfit) <- nom
    manymods_ifit <- lapply(1:length(model$group_models), function(x){
      eRm::itemfit(manymods_pparameters[[x]])
    })
    names(manymods_ifit) <- nom
    manymods_pmis <- lapply(1:length(model$group_models), function(x){
      eRm::PersonMisfit(manymods_pparameters[[x]])
    })
    names(manymods_pmis) <- nom

    output <- list(
      full_model = list(personfit =  pfit,
                        percent_pmisfit = pmis,
                        itemfit = ifit
      ),
      group_models = list(personfit = manymods_ifit,
                          percent_pmisfit = manymods_pmis,
                          itemfit = manymods_ifit)
    )
  } else{
    output <- list(personfit = pfit,
                   percent_pmisfit = pmis,
                   itemfit = ifit
    )
  }

  output
}
