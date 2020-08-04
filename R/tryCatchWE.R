#' Catch errors and warnings
#'
#' @description Catch any errors and warnings and then rerun the function if necessary.
#' The function returns all messages and the function output.
#'
#' @param expr R expression to evaluate.
#'
#' @return The function returns all warning and error messages and the function output.
#'
#' @examples
#' # Load the tinytrolls data
#' data(tinytrolls)
#'
#' # Use the emoxicon function
#' \dontrun{
#' warns.errs.tinytrolls <- tryCatch.W.E(emoxicon(text = tinyTrolls$content, lexicon = emotions))
#' }
#' @author Tara Valladares <tls8vx at virginia.edu>
#' @export
# Function to catch errors and warnings
tryCatch.W.E <- function(expr)
{
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(model = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warnings = W)
}
