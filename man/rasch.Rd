% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rasch.R
\name{rasch}
\alias{rasch}
\title{Estimation of Rasch Models for Emoxicon}
\usage{
rasch(scores, groups, return_models = TRUE, ...)
}
\arguments{
\item{scores}{A dataframe containing the emotion scores}

\item{groups}{An optional vector of group identifers the same length as \code{scores}.
If provided, individual Rasch models will be run by group.}

\item{return_models}{Logical. Should the model for each group be returned? Default is TRUE.
Setting this value to FALSE will reduce object size, but only the category orders will
be returned.}

\item{...}{Additional arguments to be passed to \code{\link[eRm]{RM}}}
}
\description{
Runs a Rasch model using the emotion scores from \code{emoxicon}.
If the data is not already dichotomous, a mean split will be performed.
}
\details{
When the data is generated using \code{emoxicon} and the default emotions lexicon,
the entire emoxicon object can be entered.
}
\examples{
# Load the tinytrolls data
data(tinyTrolls)

# Use the emoxicon function
\dontrun{
emotions.tinytrolls <- emoxicon(text = tinyTrolls$content, lexicon = emotions)
# Recode the variables to 0 (below mean) or 1 (equal to or above mean)
emotions.rasch <- as.data.frame(apply(emotions.tinytrolls[,-c(1:2)],2,
function(x) ifelse(x<mean(x),0,1)))
# Apply the Rasch Model
rm.tinytrolls <- rasch(scores = emotions.tinytrolls)
}
}
\references{
Mair, P., & Hatzinger, R. (2007).
Extended Rasch modeling: The eRm package for the application of IRT models in R.
\emph{Journal of Statistical Software}, \emph{20(9)},1-20.
doi:\href{http://www.jstatsoft.org/v20/i09}{10.18637/jss.v020.i09}
}
\seealso{
\code{\link{emoxicon}}, where the emotion scores are generated and \code{\link{rasch_fit}} to calculate Rasch fit statistics.
}
\author{
Tara Valladares <tls8vx at virginia.edu>, Hudson F. Golino <hfg9s at virginia.edu>
}
