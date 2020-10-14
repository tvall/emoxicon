#' Calculate Rasch fit statistics
#'
#' @description This function serves as a wrapper to calculate Rasch person and item fit statistics based
#' on the \code{\link[eRm]{person.parameter}} fit functions.
#'
#' @param model The object outputted by the \code{\link{emoxicon}} \code{\link{rasch}} function.
#'
#' @param groups Logical. Should summary fit statistics be calculated across
#' the individual group models, if present? The default is FALSE. Running fit statistics
#' on many models can be slow.
#'
#' @examples
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
#' # Use the rasch_fit function
#' fit.tinytrolls <- rasch_fit(rm.tinytrolls)
#' }
#'
#' @references
#' Mair, P., & Hatzinger, R. (2007).
#' Extended Rasch modeling: The eRm package for the application of IRT models in R.
#' \emph{Journal of Statistical Software}, \emph{20(9)},1-20.
#' doi:\href{http://www.jstatsoft.org/v20/i09}{10.18637/jss.v020.i09}
#'
#' @author Tara Valladares <tls8vx at virginia.edu>
#'
#' @export
# Rasch Function:
# Updated 04.06.2020
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
