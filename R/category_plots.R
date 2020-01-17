#' Category plots
#'
#' @author Tara Valladares <tls8vx at virginia.edu>
#'
#' @export
#'
#' @importFrom reshape2 melt
#' @import ggplot2
#'
#'

catplot<- function(categorys, ...){
  meltcats<-reshape2::melt(categorys, variable.name="Category", value.name="Order")

  meltcats$Category <- factor(meltcats$Category)

  mt <- ggplot(meltcats,
               aes(Order, colour = Category, fill=Category)) +
    geom_histogram() + facet_grid(. ~ Category, scales = "fixed") +
    ggtitle("Category Order Plot")

  mt
}
# setMethod("plot", signature = )
