#' Category plots
#'
#' @author Tara Valladares <tls8vx at virginia.edu>
#' @noRd
#'
#' @importFrom reshape2 melt
#'
#'

setMethod("plot", signature = )
dol<-melt(dif.order)
levels(dol$variable)<- category
str(dol)

describe(dif.order)

library(ggplot2)
mt <- ggplot(dol[which(dol$accountType=="Right"),],
             aes(value, colour = factor(variable), fill=factor(variable))) +
  geom_histogram()

mt + facet_grid(. ~ variable, scales = "fixed") +ggtitle("Right Trolls")

mt2 <- ggplot(dol[which(dol$accountType=="Left"),], aes(value, colour = factor(variable), fill=factor(variable))) +
  geom_histogram()

mt2 + facet_grid(. ~ variable, scales = "fixed")+ggtitle("Left Trolls")
