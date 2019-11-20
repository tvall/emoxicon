#' rasch
#'
#' Runs a Rasch model using the emotion scores from emoxicon().
#' If the data is not already dichotomous, a mean split will be performed.
#' When the data is generated using emoxicon() and the emotions lexicon,
#' the entire emoxicon object can be entered.
#'
#'
#' @param scores A dataframe containing the emotion scores
#'
#' @param groups To run separate rasch models by a grouping variable,
#' specify the name of the variable that contains the groups in the dataframe
#'
#' @param ... Additional arguments to be passed to \code{\link[RM]{eRm}}
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
#'
#' @export
#'
#'


rasch <- function(scores, groups = NULL, ...){

  if(exists("groups")){
    if(!(groups %in% colnames(scores))){
      warning("groups is not a column name in the scores dataframe")
    }
    groupV <- scores[[groups]]
    scores <- scores[,!(colnames(scores)%in%groups)]
  }


  if("emoxicon" %in% class(scores) & "emotions" %in% class(scores)){
    class_emoxicon <- TRUE
    scores.raw <- scores
    scores <- scores[,c("AFRAID", "AMUSED", "ANGRY", "ANNOYED", "DONT_CARE", "HAPPY",
                       "INSPIRED", "SAD")]
  }

  if(!all(apply(scores, 2, function(x) {length(unique(na.omit(x))) == 2} ))){
    scores_all <- apply(scores, 2,function(x){ifelse(x< mean(x, na.rm = TRUE), 0,1)})
    message("Data was not dichotomized. Mean split performed.")
  }

  # Run an overall Rasch model

  scores.RM <- eRm::RM(scores_all,...)

  if(exists("class_emoxicon")){
    scores.RM$raw.data <- scores.raw
  }

  scores.RM

  # model summary, item measures, fit measures, ability measures, item fit,
  # plot functions for this class - the llr plot
  # ordering function - by groups=TRUE
  # separate function for the entropy/TI (can call it within this function)


  # Category Ordering by groups

  if(exists("groupV")){
    group_lengths<-sapply(unique(groupV), function(x){NROW(scores[which(groupV==x),])}) #count lengths
    if(sum(group_lengths<30)>0){
      message("Rasch models were not run for groups with less than 30 observations")
    }

    rasch_groups <- lapply(unique(groupV)[group_lengths>=30],function(x){
      tryCatch({
        y<-apply(scores[which(groupV==x),], 2,
                 function(x){ifelse(x< mean(x, na.rm = TRUE), 0,1)})
        eRm::RM(y,...)
      },
      error = function(e)
        e)
    })

    # bad <- which(sapply(rasch_groups, function(x){inherits(x, "simpleError")}))
    zeros <- which(sapply((1:length(rasch_groups)), function(x){rasch_groups[[x]]$npar})!=(ncol(scores)-1))

    dif<-Reduce(function(x, y) merge(x, y, all=TRUE),
              lapply(1:length(rasch_groups), function (x){t(as.data.frame(
                c(x,-(rasch_groups[[x]]$betapar))))}))[-1]# negative b/c these are betas

    dif[is.na(dif)]<- 999

    dif.order <- as.data.frame(t(apply(dif, 1, rank, ties.method= "random")))
    colnames(dif.order) <- gsub(pattern="beta ", x=colnames(dif.order), "")
  }

}
