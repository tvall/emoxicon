#' Russian Trolls Data
#'
#' A matrix containing tweets in English (n = 476,215)
#' from Left Wing and Right Wing Russian troll twitter accounts.
#' Posts were created between February 2012 and May 2018.
#' This dataset includes only authored tweets; retweets, reposts, and repetitions have been removed.
#' The original data was provided by FiveThirtyEight and Clemson University researchers Darren Linvill and Patrick Warren.
#' For more invormation visit https://github.com/fivethirtyeight/russian-troll-tweets
#'
#'
#'
#' @name trolls
#'
#' @docType data
#'
#' @usage data(trolls)
#'
#' @format A data frame with 476,215 rows and 6 columns.
#' \describe{
#'    \item{content}{A tweet.}
#'    \item{author}{The name of the handle that authored the tweet.}
#'    \item{publish_date}{The date the tweet was published on.}
#'    \item{followers}{How many followers the handle had at the time of posting.}
#'    \item{updates}{How many interactions (including likes, tweets, retweets) the post garnered.}
#'    \item{account_type}{Left or Right}
#
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(trolls)
#'
#'

NULL
#----
