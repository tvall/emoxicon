#' Summary method
#'
#' @param object Object of class emoxrasch
#'
#' @author Tara Valladares <tls8vx at virginia.edu>
#'
#' @importFrom eRm summary.RM
#'
#'

summary.emoxrasch <- function(object, ...) {
  if("category_order" %in% names(object)){
    catord <- list("Summary of Category Orders",
                   summary.data.frame(object$category_order))
  }

  sum.er <- catord
  print(sum.er)
}
