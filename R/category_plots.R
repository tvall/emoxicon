#' Category plots
#'
#' @param categories Category orders produced from \code{rasch}
#'
#' @author Tara Valladares <tls8vx at virginia.edu>
#'
#' @export
#'
#' @importFrom reshape2 melt
#' @import ggplot2
#'
#'

catplot<- function(categories, ...){
  meltcats<-reshape2::melt(categories, variable.name="Category", value.name="Order")

  meltcats$Category <- factor(meltcats$Category)

  mt <- ggplot(meltcats,
               aes(Order, colour = Category, fill=Category)) +
    geom_histogram() + facet_grid(. ~ Category, scales = "fixed") +
    ggtitle("Category Order Plot")

  mt
}
