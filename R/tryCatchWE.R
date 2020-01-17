#' Catch errors and warnings
#'
#' Catch any errors and warnings and then rerun the function if necessary.
#' The returns all messages and the function output.
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
       warnings = W)
}
