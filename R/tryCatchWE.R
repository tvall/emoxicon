#' Run a function, catch errors, keep output
#'
#' Allows you to run a function, catch any errors and warnings,
#' and then rerun the function. The end results is a list of
#' all messages and the function output, if applicable.
#'
#' @author Tara Valladares <tls8vx at virginia.edu>
#' @noRd

tryCatch.W.E <- function(expr)
{
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(model = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}
