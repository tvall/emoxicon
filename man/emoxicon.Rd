% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emoxicon.R
\name{emoxicon}
\alias{emoxicon}
\title{Emoxicon}
\usage{
emoxicon(text, lexicon = emotions, exclude)
}
\arguments{
\item{text}{A dataframe containing texts to be scored (one text per row).}

\item{lexicon}{The lexicon used to score the words. The default is the \code{\link{emotions}} dataset,
a modification of the lexicon developed by Araque, Gatti, Staiano, and Guerini (2018).
To use the raw lexicon from Araque et. al (2018) containing the original probability weights, use the \code{\link{weights}} dataset.
If another custom lexicon is used, the first column of the lexicon should contain the terms
and the subsequent columns contain the scoring categories.}

\item{exclude}{A vector listing terms that should be excluded from the lexicon.
Words specified in \code{exclude} will not
influence document scoring. Users should consider excluding 'red herring' words
that are more closely related to the topics of the documents,
rather than the documents' emotional content.
For example, the words "clinton" and "trump" are present in the lexicon and are both associated with the emotion 'AMUSED'.
Excluding these words when analyzing political opinions may produce more accurate results.}
}
\description{
Calculates emotion scores in texts using a bag-of-words approach. The default lexicon was
compiled by Araque, Gatti, Staiano, and Guerini (2018).
}
\examples{
# Load the tinytrolls data
data(tinyTrolls)

# Use the emoxicon function
\dontrun{
emotions.tinytrolls <- emoxicon(text = tinyTrolls$content, lexicon = emotions)
}
}
\references{
Araque, O., Gatti, L., Staiano, J., and Guerini, M. (2018).
DepecheMood++: a Bilingual Emotion Lexicon Built Through Simple Yet Powerful Techniques.
\emph{ArXiv} preprint is available at https://arxiv.org/abs/1810.03660.
}
\seealso{
\code{\link{emotions}}, where we describe how we modified the original DepecheMood++ lexicon.
}
\author{
Tara Valladares <tls8vx at virginia.edu>, Hudson F. Golino <hfg9s at virginia.edu>
}
