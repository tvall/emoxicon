#' Category plots
#' @description Plot function for the categories produced from \code{rasch}
#' @param categories Category orders produced from \code{rasch}
#' @param cat_labels Optional category labels in the style of c(colname1 = "newlabel1", colname2 = "newlabel2").
#' If not specified, labels will be taken from the category column names
#' @param \dots Other arguments to pass onto \code{\link{ggplot}}
#'
#' @author Tara Valladares <tls8vx at virginia.edu>
#'
#' @export
#'
#' @importFrom reshape2 melt
#' @import ggplot2
#'
#'

catplot<- function(categories, cat_labels, ...){
  meltcats<-reshape2::melt(categories, variable.name="Category", value.name="Order")

  meltcats$Category <- factor(meltcats$Category)

  if(missing("cat_labels")){
    cat_labels <- unique(meltcats$Category)
  }

  mt <- ggplot(meltcats,
               aes(Order, colour = Category, fill=Category)) +
    geom_histogram() + facet_grid(. ~ Category,
                                  labeller = as_labeller(cat_labels),
                                  scales = "fixed") +
    ggtitle("Category Order Plot")

  mt
}
