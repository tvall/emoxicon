library(emoxicon)
library(eRm)
library(ggplot2)
library(boot)
library(infotheo)
library(psych)
library(mirt)

## code to prepare `trolls` dataset

### not run ###
# trolls_raw <- vector("list", ndoc)
# for(i in 1:ndoc){
#   trolls_raw[[i]]<- read.csv(paste0("../russian-troll-tweets-master/IRAhandle_tweets_", i,".csv"),
#               stringsAsFactors = FALSE)
# }
# trolls<-vector("list",ndoc)
# for(i in 1:ndoc){
#   trolls[[i]]<- trolls_raw[[i]][which(trolls_raw[[i]]$language == "English"),]
# }
#
# # Remove retweets
# trolls.clean <- lapply(trolls,function(x){
#   x<-x[which(x$retweet == 0),]
# } )
#
# # clean data
# trolls.clean <- lapply(trolls.clean, function(y){
#   content<- y$content
#   content<- stringi::stri_trans_general(content, "latin-ascii")
#   content<- gsub("&amp", "and", content)
#   content<- gsub("http[^[:space:]]*", "", content) # remove links
#   content<- tolower(content)
#   content<- gsub("^rt.*", "", content) # remove reposts
#
#   results<- cbind(content, y[,c("author", "publish_date", "followers", "updates","account_type")])
#   results<- results[which(results$account_type=="Left"|
#                             results$account_type=="Right"),]
#   results<- subset(results, content !="") #remove empty tweets
# })
#
# trolls<-do.call(rbind, trolls.clean)
# trolls$content<- as.character(trolls$content)

# THIS HAS CHANGED!!!!
exclude <-c("hilary", "hillary", "russia", "russian",
            "trump","donald", "bernie", "sanders","clinton", "rt",
            "obama", "barack", "america", "president",
            "black", "white", "racist")

(exclude %in% emotions$word)

# Load data from Emoxicon package
data(trolls)


###THIS HAS CHANGED!!!!
# Create vector of unique author names w/ more than 30 tweets, for later
author.small <- unique(trolls[c("author", "account_type")])
author.small <-  author.small[order(author.small$author),]
author.small<- author.small[as.vector(table(trolls$author) > 29),]


# Run Emoxicon
trolls_scored <- emoxicon(text=trolls$content, lexicon = "emotions",
                          exclude = exclude)


# emotions[1996:2001,]
trolls_scored[204:209,]
trolls_models$full_model$X[204:209,]

# Run Rasch models

trolls_models <- rasch(scores= trolls_scored, groups = trolls$author, return_models = TRUE)


# Fit
summary(trolls_models$full_model)

pparameters <- eRm::person.parameter(trolls_models$full_model)

pfit <- eRm::personfit(pparameters)
ifit <- eRm::itemfit(pparameters)
pmis <- eRm::PersonMisfit(pparameters)

ifit
pmis


# infit/outfit plots by theta-----
plotdat <- data.frame(theta = as.factor(pparameters$thetapar[[1]]),
                      infitZ =pfit$p.infitZ,
                      outfitZ =pfit$p.outfitZ,
                      se = pparameters$se.theta$NAgroup1)
levels(plotdat$theta) <- round(as.numeric(levels(plotdat$theta)),2)

ggplot(data = plotdat, aes(x = theta, y=infitZ)) +
  # geom_jitter(aes(x = theta, y=infitZ),height = .1, width = .1)
  geom_violin() + geom_hline(yintercept = c(2,-2), col="#D55E00") +
  ylim(-3,3) + ggtitle("Standardized Infit")

ggplot(data = plotdat, aes(x = theta, y=outfitZ)) +
  # geom_jitter(aes(x = theta, y=infitZ),height = .1, width = .1)
  geom_violin() + geom_hline(yintercept = c(2,-2), col="#D55E00") +
  ylim(-3,3) + ggtitle("Standardized Outfit")

ggplot(data = unique(plotdat), aes(x = theta, y=se)) +
  # geom_jitter(aes(x = theta, y=infitZ),height = .1, width = .1)
  geom_point() + ggtitle("Standard Error of the Estimate")

# fit plots -----
# person item map
plotPImap(trolls_models$full_model, sorted = TRUE)

# check if high and low scores are the same
lrres.rasch <- LRtest(trolls_models$full_model, splitcr = "median")
plotGOF(lrres.rasch, tlab = "item",
        conf = list(ia = FALSE, col = "blue", lty = "dotted"),
        smooline= list(gamma = .95, col = "black", lty = "dashed"))

# check if left and right trolls are the same
lrres <- LRtest(trolls_models$full_model, splitcr = trolls$account_type)
plotGOF(lrres, tlab = "item",
        conf = list(ia = FALSE, col = "red", lty = "dotted"),
        smooline = list(gamma = .95, col = "black", lty = "dashed"))

# Look at empiracal plots, ICCs

plotICC(trolls_models$full_model,1, empICC = list("raw", type="b",col = "blue", lty = 2))
plotICC(trolls_models$full_model,2, empICC = list("raw", type="b",col = "blue", lty = 2))
plotICC(trolls_models$full_model,3, empICC = list("raw", type="b",col = "blue", lty = 2))
plotICC(trolls_models$full_model,4, empICC = list("raw", type="b",col = "blue", lty = 2))
plotICC(trolls_models$full_model,5, empICC = list("raw", type="b",col = "blue", lty = 2))
plotICC(trolls_models$full_model,6, empICC = list("raw", type="b",col = "blue", lty = 2))
plotICC(trolls_models$full_model,7, empICC = list("raw", type="b",col = "blue", lty = 2))
plotICC(trolls_models$full_model,8, empICC = list("raw", type="b",col = "blue", lty = 2))


# Look at empirical plots - total scores
library(mirt)
empirical_plot(trolls_models$full_model$X
               ,1)
empirical_plot(trolls_models$full_model$X
               ,2)
empirical_plot(trolls_models$full_model$X
               ,3)
empirical_plot(trolls_models$full_model$X
               ,4)
empirical_plot(trolls_models$full_model$X
               ,5)
empirical_plot(trolls_models$full_model$X
               ,6)
empirical_plot(trolls_models$full_model$X
               ,7)
empirical_plot(trolls_models$full_model$X
               ,8)

# Individual model fit -----
# CHANGED!!!


pparameters <- eRm::person.parameter(trolls_models$full_model)

pfit <- eRm::personfit(pparameters)
ifit <- eRm::itemfit(pparameters)
pmis <- eRm::PersonMisfit(pparameters)

manymods_pparameters <- lapply(trolls_models$group_models, function(x){
  eRm::person.parameter(x$model)
})
manymods_pfit <- lapply(1:length(trolls_models$group_models), function(x){
  eRm::personfit(manymods_pparameters[[x]])
})
manymods_ifit <- lapply(1:length(trolls_models$group_models), function(x){
  eRm::itemfit(manymods_pparameters[[x]])
})
manymods_pmis <- lapply(1:length(trolls_models$group_models), function(x){
  eRm::PersonMisfit(manymods_pparameters[[x]])
})

modsL <- sapply(trolls_models$group_models, function(x) x$group)
modsL <- modsL %in% author.small$author[which(author.small$account_type == "Left")]

manymods_pmis_total <- sapply(manymods_pmis, function(x){x$PersonMisfit})
summary(manymods_pmis_total)
summary(manymods_pmis_total[modsL])
summary(manymods_pmis_total[!modsL])

manymods_ifit[[1]]

manymods_ifit_total <- sapply(1:length(manymods_ifit), function(i){
  p <- pchisq(manymods_ifit[[i]]$i.fit, df=manymods_ifit[[i]]$i.df-1, lower.tail=FALSE)
  x <- t(as.data.frame(ifelse(p<=.05,TRUE, FALSE)))
  items <- data.frame(model=trolls_models$group_models[[i]]$group,
                      x,
                      stringsAsFactors = FALSE,
                      row.names = trolls_models$group_models[[i]]$group)
  z<-c("model","AFRAID", "AMUSED", "ANGRY", "ANNOYED", "DONT_CARE", "HAPPY",
       "INSPIRED", "SAD")
  if(ncol(items) < 9){
    items[,z[which(!z%in% colnames(items))]]<-NA
  }
  items
}, simplify = F)
x<-do.call(rbind, manymods_ifit_total)
head(x)
summary(x)

round(colSums(x[,-1], na.rm = T)/nrow(x),2)

round(colSums(x[modsL,-1], na.rm = T)/nrow(x[modsL,]),2)
round(colSums(x[!modsL,-1], na.rm = T)/nrow(x[!modsL,]),2)


# Item category ordering -----
## CHANGED!!!!



catleft <- row.names(trolls_models$category_order) %in%
  author.small[which(author.small$account_type == "Left"), "author"]
catright <- row.names(trolls_models$category_order) %in%
  author.small[which(author.small$account_type == "Right"), "author"]

# check
x<-row.names(trolls_models$category_order)[catleft]
sum(!x %in% trolls$author[which(trolls$account_type == "Left")])

x<-row.names(trolls_models$category_order)[catright]
sum(!x %in% trolls$author[which(trolls$account_type == "Right")])


catplots <- catplot(trolls_models$category_order)

catplotsL <- catplot(trolls_models$category_order[catleft,])
catplotsR <- catplot(trolls_models$category_order[catright,])

catplots + ggtitle("Category Order Plot - All Trolls")
catplotsL + ggtitle("Category Order Plot - Left Trolls")
catplotsR  + ggtitle("Category Order Plot - Right Trolls")

describe(trolls_models$category_order)
describe(trolls_models$category_order[catright,])
describe(trolls_models$category_order[catleft,])

dat<- data.frame(
  cbind(trolls_models$category_order,
        group=catleft))
aggregate(. ~ group,dat, FUN=mean) # false = right

# t tests
ttests<-lapply(colnames(trolls_models$category_order), function(i){
  t.test(trolls_models$category_order[catright,i],
         trolls_models$category_order[catleft,i])
})
# pvalues
cat("\n pvalues for t test \n")
sapply(1:length(colnames(trolls_models$category_order)), function(num){
  rbind(colnames(trolls_models$category_order)[num],
        round(ttests[[num]]$p.value,2))
})
# distribution tests
kstests<-suppressWarnings(lapply(colnames(trolls_models$category_order), function(i){
  ks.test(trolls_models$category_order[catright,i],
          trolls_models$category_order[catleft,i])
}))
# pvalues
cat("\n pvalues for KS test \n")
sapply(1:length(colnames(trolls_models$category_order)), function(num){
  rbind(colnames(trolls_models$category_order)[num],
        round(kstests[[num]]$p.value,2))
})


# Weighted category ordering-----

weighted <- trolls_models$category_order
weighted$weight <- NA

for(i in 1:length(trolls_models$group_models)){
  weighted$weight[i] <-nrow(trolls_models$group_models[[i]]$model$X)
}

# weighted$weight <-  weighted$weight/sum(weighted$weight)
#
# weighted[,c("AMUSED", "ANGRY", "ANNOYED", "DONT_CARE", "INSPIRED", "SAD",
#             "AFRAID", "HAPPY")] <- sapply(weighted[,c("AMUSED", "ANGRY", "ANNOYED", "DONT_CARE", "INSPIRED", "SAD",
#                                                       "AFRAID", "HAPPY")],
#                                           function(x){x*weighted$weight})
#
# ttestsweighted<-lapply(colnames(trolls_models$category_order), function(i){
#   t.test(weighted[right,i],
#          weighted[catleft,i])
# })


# Mutual Information -----

# Determine the minimum amount of mutual information possible
min_info<-vector(mode = "numeric")
j<-500
for(j in 1:j){
  n<-400
  ranMi<-matrix(nrow=n, ncol=8)
  for(i in 1:n){
    ranMi[i,] <- sample(1:8, 8,replace = FALSE)}
  min_info[j]<-multiinformation(ranMi)
}
summary(min_info)

# Calculate mutual information per group

multiinformation(trolls_models$category_order)
multiinformation(trolls_models$category_order[catright,])
multiinformation(trolls_models$category_order[catleft,])

# bootstrap CI for mutual information
mi <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  mi<- multiinformation(data[indices,])
  return((mi))
}
boot.left <-boot(trolls_models$category_order
                 [which(catleft),],
                 statistic = mi, R=1000)
boot.left
bci.left<-boot.ci(boot.left, conf = .95, type = "norm")


boot.right <-boot(trolls_models$category_order
                  [which(catright),],
                  statistic = mi, R=1000)
boot.right
bci.right<-boot.ci(boot.right, conf = .95, type = "norm")



# plot

plotdat2 <- data.frame(
  MI = c(multiinformation(trolls_models$category_order[which(catright),]),
             multiinformation(trolls_models$category_order[which(catleft),])),
  ci.low =  c(bci.right$normal[2],
             bci.left$normal[2]),
  ci.high =  c(bci.right$normal[3],
             bci.left$normal[3]),
  Group = c("Right", "Left")
)
plotdat2
ggplot(data = plotdat2) + geom_point(aes(x=Group, y=MI), color = c("Red", "Blue")) +
  geom_hline(yintercept = min_info, linetype="dashed") +
  geom_errorbar(aes(ymin=ci.low, ymax=ci.high, x = Group), colour="black", width=.1) +
  ggtitle("Mutual Information by Group w/ 95% CI") + ylim(0,12) +
  # annotate("text", label="Maximum value (minimum info)",
  #          x=1, y=min_info+.3,
  #          size=4, fontface="italic") +
  ylab("Mutual Information")

# Diversity -----
# Gini Coefficient
# FYI, it fails when all responses are zero, row gets NA

# Gini by tweet
gini_trolls_tweets <- data.frame(full = apply(trolls_scored[,c(-1,-2)],1,reldist::gini),
                                 dich = apply(trolls_models$full_model$X,1,reldist::gini),
                                 group = trolls$account_type)
# Gini by author
gini_trolls_author <-
  data.frame(
    full = sapply(unique(trolls$author), function(group) {
      x <- trolls_scored[trolls$author == group, ]
      if(ncol(x) != 10){NA}else{
        reldist::gini(colSums(x[, c("AFRAID",
                                    "AMUSED",
                                    "ANGRY",
                                    "ANNOYED",
                                    "DONT_CARE",
                                    "HAPPY",
                                    "INSPIRED",
                                    "SAD")]))}
    }),

    dich = sapply(unique(trolls$author), function(group) {
      x <- trolls_models$full_model$X[trolls$author == group, ]
      if(length(x) <= 8){z<-NA}else{
        reldist::gini(colSums(x[, c("AFRAID",
                                    "AMUSED",
                                    "ANGRY",
                                    "ANNOYED",
                                    "DONT_CARE",
                                    "HAPPY",
                                    "INSPIRED",
                                    "SAD")]))}
    }),
    group = unique(trolls[trolls$author %in% unique(trolls$author),c("author","account_type")])$account_type,
    author = unique(trolls$author)
  )

# Average gini by tweet within authors
gini_trolls_tweets_average <- do.call(rbind, lapply(unique(trolls$author), function(auth){
  x <- gini_trolls_tweets[trolls$author == auth, ]
  averages <- data.frame(
    full = mean(na.omit(x$full)),
    dich = mean(na.omit(x$dich)),
    author = as.character(auth),
    group = x$group[1],
    stringsAsFactors = FALSE
  )
  averages
}))


# .7 correlation between the two per tweet, less NA's with full (not dich)
cor(na.omit(gini_trolls_tweets[c("full","dich")]))
cor(na.omit(gini_trolls_author[c("full","dich")]))
cor(na.omit(gini_trolls_tweets_average[c("full","dich")]))

# aggregate(full ~ group, gini_trolls_tweets, FUN=median)
# aggregate(full ~ group, gini_trolls_author, FUN=median)
# aggregate(full ~ group, gini_trolls_tweets_average, FUN=median)

# t.test(dich ~ group, gini_trolls_tweets)
t.test(dich ~ group, gini_trolls_author)
t.test(full ~ group, gini_trolls_author)

# t.test(dich ~ group, gini_trolls_tweets_average)




# number of tweets -----
quantile(weighted$weight, seq(0,1,.10))

plot1 <- data.frame(weights = sort(weighted$weight),
                    x = 1:nrow(weighted))

ggplot(data = plot1, aes(y=weights, x = x)) + geom_point() +
  ylab("Number of Tweets") + xlab(" ") + ggtitle("Number of Tweets in Accounts with >30 Tweets")


# how many left and right troll tweets?
table(trolls$account_type)

# how many left and right accounts?
table(unique(trolls[,c("author", "account_type")])$account_type)

# distribution of tweets per type?
x<-table(trolls$author, trolls$account_type)
x<-x[,"Right"]
x <- x[which(x>0)]
summary(x)

x<-table(trolls$author, trolls$account_type)
x<-x[,"Left"]
x <- x[which(x>0)]
summary(x)


# how many left and right troll accounts, 30+ tweets?
length(table(trolls$author)[table(trolls$author) > 29])
nrow(author.small)

x<-sapply(author.small$author,function(z){
  nrow(trolls[which(trolls$author==z),])
})
summary(x[which(author.small$account_type=="Right")])
sum(x[which(author.small$account_type=="Right")])
summary(x[which(author.small$account_type=="Left")])
sum(x[which(author.small$account_type=="Left")])


# how many troll accounts have 30+ tweets?
nrow(author.small)
nrow(author.small)/length(unique(trolls$author))

# how many tweets does this account for?
sum(weighted$weight)/nrow(trolls)


# check
table(trolls$author[which(!trolls$author %in% author.small$author)])

sum(table(trolls$author[which(!trolls$author %in% author.small$author)]))

sum(table(trolls$author[which(trolls$author %in% author.small$author)]))

describe(weighted$weight)

quantile(weighted$weight, seq(0,1,.10))

nrow(trolls_models$group_models[[i]]$model$X)
trolls_models$group_models[[i]]$group

nrow(subset(trolls, author == "_SHERYLGILBERT"))

x<-rep(NA, length(trolls_models$group_model))
for(i in 1:length(trolls_models$group_model)){
 x[i] <- nrow(trolls_models$group_models[[i]]$model$X) ==
   nrow(subset(trolls, author == trolls_models$group_models[[i]]$group))
}
sum(!x)

x<-rep(NA, length(trolls_models$group_model))
for(i in 1:length(trolls_models$group_model)){
  x[i] <- trolls_models$group_models[[i]]$group == author.small$author[i]
}
sum(x)


# x %in% author.small$author
# length(x) == nrow(author.small)
# author.small$author %in% x
#
# sum(!author.small$author %in% x)
#
# nrow(subset(trolls, author == "4EVER_SUSAN"))
# "4MYSQUAD" %in% author.small
#
#
# # Create vector of unique author names w/ more than 30 tweets, for later
# author.small <- unique(trolls[c("author", "account_type")])
# author.small <-  author.small[order(author.small$author),]
# author.small<- author.small[as.vector(table(trolls$author) > 29),]
#
#
# nrow(subset(trolls, author == "_SOLOMONALBERT_"))


434 + 158

length( author.small[which(author.small$account_type == "Left"), "author"])


# --- EGA -----
# install.packages("EGAnet")
library(EGAnet)
library(tm)

trolls.corpus.left <- Corpus(VectorSource(
  trolls$content[which(trolls$account_type == "Left")]
))

# Functions to remove URL and other characters:
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)


# Applying transformations to the corpora:
trolls.corpus.left <- tm_map(trolls.corpus.left, removeURL)
trolls.corpus.left <- tm_map(trolls.corpus.left, removePunctuation)
trolls.corpus.left <- tm_map(trolls.corpus.left, removeNumbers)
trolls.corpus.left <- tm_map(trolls.corpus.left, removeNumPunct)
trolls.corpus.left <- tm_map(trolls.corpus.left, tolower)
trolls.corpus.left2 <- tm_map(trolls.corpus.left, removeWords, stopwords("English"))
trolls.corpus.left2 <- tm_map(trolls.corpus.left, removeWords, stopwords("Russian"))
dtm.left <- DocumentTermMatrix(trolls.corpus.left2)

# Reducing sparsity:
dtm2.left <- removeSparseTerms(dtm.left, 0.995)
dtm2.left

# Transforming the document term matrix into a dataframe:
dtm.data.left <- as.data.frame(as.matrix(dtm2.left))
head(dtm.data.left)

nrow(dtm.data.left) == nrow(trolls[which(trolls$account_type == "Left"),])

# right

trolls.corpus.right <- Corpus(VectorSource(
  trolls$content[which(trolls$account_type == "Right")]
))

# Functions to remove URL and other characters:
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)


# Applying transformations to the corpora:
trolls.corpus.right <- tm_map(trolls.corpus.right, removeURL)
trolls.corpus.right <- tm_map(trolls.corpus.right, removePunctuation)
trolls.corpus.right <- tm_map(trolls.corpus.right, removeNumbers)
trolls.corpus.right <- tm_map(trolls.corpus.right, removeNumPunct)
trolls.corpus.right <- tm_map(trolls.corpus.right, tolower)
trolls.corpus.right2 <- tm_map(trolls.corpus.right, removeWords, stopwords("English"))
trolls.corpus.right2 <- tm_map(trolls.corpus.right, removeWords, stopwords("Russian"))
dtm.right <- DocumentTermMatrix(trolls.corpus.right2)

# Reducing sparsity:
dtm2.right <- removeSparseTerms(dtm.right, 0.995)
dtm2.right

# Transforming the document term matrix into a dataframe:
dtm.data.right <- as.data.frame(as.matrix(dtm2.right))
head(dtm.data.right)

nrow(dtm.data.right) == nrow(trolls[which(trolls$account_type == "Right"),])



# EGA:
ega.trolls.99.left <- EGA(dtm.data.left,
                          model = "TMFG")
save(ega.trolls.99.left, file ="egatrolls-left-April8-2020")

ega.trolls.99.right <- EGA(dtm.data.right,
                          model = "TMFG")
save(ega.trolls.99.right, file ="egatrolls-right-April8-2020")

library(beepr)
beep()

