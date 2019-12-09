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
#' @param groups To run separate rasch models by group,
#' specify the name of the variable that contains the group identifiers
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
#'
#' @export
#'
#'


rasch <- function(scores, groups = NULL, ...){

  #set up error list
  errors <- vector("list")

  # if(!is.null(groups)){
  #   if(!(groups %in% colnames(scores))){
  #     warning("groups is not a column name in the scores dataframe")
  #   }
  #   groupV <- scores[[groups]]
  #   scores <- scores[,!(colnames(scores)%in%groups)]
  # }
  groupV <- groups


  # if("emoxicon" %in% class(scores) & "emotions" %in% class(scores)){
  #   class_emoxicon <- TRUE
  #   scores.raw <- scores
  #   scores <- scores[,c("AFRAID", "AMUSED", "ANGRY", "ANNOYED", "DONT_CARE", "HAPPY",
  #                      "INSPIRED", "SAD")]
  # }else{
    scores <- scores[which(sapply(scores, class)=="numeric"|sapply(scores, class)=="integer")]
  # }

  if(!all(apply(scores, 2, function(x) {length(unique(na.omit(x))) == 2} ))){
    scores_all <- apply(scores, 2,function(x){ifelse(x<= mean(x, na.rm = TRUE), 0,1)})
    message("Data was not dichotomized. Mean split performed.")
  }

  # Run an overall Rasch model

  rasch_fit <- eRm::RM(scores_all,...)
  # if(exists("class_emoxicon")){
  #   rasch_fit$raw.data <- scores.raw
  # }

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
                 function(x){ifelse(x<= mean(x, na.rm = TRUE), 0,1)})
        eRm::RM(y,...) # here we need to probably change it so that we store everyones group in y and subset by y here
      },
      error = function(e)
        e)
    })

    # index models that failed
    bad <- which(sapply(rasch_groups, function(x){inherits(x, "simpleError")}))

    # index models where one or more categories were not estimated
    # if more than one,
    zeros <- (1:length(rasch_groups))[-bad][sapply((1:length(rasch_groups))[-bad],
                    function(x){
                      rasch_groups[[x]]$npar != (ncol(scores)-1)
                      })]
    # xx <- lapply(zeros, function(z){
    #   y<-apply(scores[which(groupV==unique(groupV)[group_lengths>=30][-bad][z]),], 2,
    #            function(x){ifelse(x<= mean(x, na.rm = TRUE), 0,1)})
    #   apply(y, 2,mean)
    # })


    # y<-apply(scores[which(groupV==unique(groupV)[group_lengths>=30][22]),], 2,
    #          function(x){ifelse(x<= mean(x, na.rm = TRUE), 0,1)})
    # erm::RM(y)



    # zeros2 <-sapply((1:length(rasch_groups))[-bad],
    #                                                function(x){
    #                                                  rasch_groups[[x]]$npar != (ncol(scores)-1)
    #                                                })

    dif<-Reduce(function(x, y) merge(x, y, all=TRUE),
              lapply(1:length(rasch_groups), function (x){t(as.data.frame(
                c(x,-1*(rasch_groups[[x]]$betapar))))}))[-1]# negative b/c these are betas

    # remove lines with all NA's/model didnt converge

    # add identifiers
    row.names(dif) <- unique(groupV)[group_lengths>=30]

    dif.order <- as.data.frame(t(apply(dif, 1, rank, ties.method= "random", na.last = "keep")))
    colnames(dif.order) <- gsub(pattern="beta ", x=colnames(dif.order), "")
    # dif.order$group <- # need to store the group name here
  }

  list(rasch_fit == rasch_fit, rasch_groups == rasch_groups, dif.order == dif.order,
       errors = errors)

}
