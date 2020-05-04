## Analyses for the Emoxicon paper
library(emoxicon)
library(eRm)
library(ggplot2)
library(boot)
library(infotheo)
library(psych)

# Load data

data(trolls)

# FOR CODE BUILDING
# set.seed(2)
# trolls <- trolls[which(trolls$author%in%sample(unique(trolls$author), size=50)),]

author.small <- trolls[c("author", "account_type")]
author.small<- author.small[author.small$author %in% unique(trolls$author)[table(trolls$author) > 29],]
author.small <- unique(author.small)

# Run Emoxicon
exclude <-c("hilary", "hillary", "russia", "russian",
            "trump", "bernie", "clinton", "rt")
trolls_scored <- emoxicon(text=trolls$content, lexicon = "emotions",
                          exclude = exclude)

# Run Rasch models

trolls_models <- rasch(scores= trolls_scored, groups = trolls$author, return_models = TRUE)

# Fit
summary(trolls_models$full_model)

pparameters <- eRm::person.parameter(trolls_models$full_model)

pfit <- eRm::personfit(pparameters)
ifit <- eRm::itemfit(pparameters)
pmis <- eRm::PersonMisfit(pparameters)

summary(pfit)
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

# Item category ordering -----

catplots <- catplot(trolls_models$category_order)

catplotsL <- catplot(trolls_models$category_order[author.small$account_type == "Left",])
catplotsR <- catplot(trolls_models$category_order[author.small$account_type == "Right",])

catplots + ggtitle("Category Order Plot - All Trolls")
catplotsL + ggtitle("Category Order Plot - Left Trolls")
catplotsR  + ggtitle("Category Order Plot - Right Trolls")

describe(trolls_models$category_order)
describe(trolls_models$category_order[author.small$account_type == "Right",])
describe(trolls_models$category_order[author.small$account_type == "Left",])

# t tests
ttests<-lapply(colnames(trolls_models$category_order), function(i){
  t.test(trolls_models$category_order[author.small$account_type == "Right",i],
         trolls_models$category_order[author.small$account_type == "Left",i])
})
sapply(1:length(colnames(trolls_models$category_order)), function(num){
  rbind(colnames(trolls_models$category_order)[num],
        round(ttests[[num]]$p.value,2))
})
# distribution tests
kstests<-lapply(colnames(trolls_models$category_order), function(i){
  ks.test(trolls_models$category_order[author.small$account_type == "Right",i],
          trolls_models$category_order[author.small$account_type == "Left",i])
})
sapply(1:length(colnames(trolls_models$category_order)), function(num){
  rbind(colnames(trolls_models$category_order)[num],
        round(kstests[[num]]$p.value,2))
})

# Mutual Information -----

# Determine the minimum amount of mutual information possible
min_info<-vector(mode = "numeric")
j<-1000
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
multiinformation(trolls_models$category_order[which(author.small$account_type=="Right"),])
multiinformation(trolls_models$category_order[which(author.small$account_type=="Left"),])

# bootstrap CI for mutual information
mi <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  mi<- multiinformation(data[indices,])
  return((mi))
}
boot.left <-boot(trolls_models$category_order[which(author.small$account_type=="Left"),],
                 statistic = mi, R=1000)
quantile(boot.left$t, c(.025,.975))

boot.right <-boot(trolls_models$category_order[which(author.small$account_type=="Right"),],
                  statistic = mi, R=1000)
quantile(boot.right$t, c(.025,.975))


# plot

plotdat2 <- data.frame(
  minfo = c(multiinformation(trolls_models$category_order[which(author.small$account_type=="Right"),]),
             multiinformation(trolls_models$category_order[which(author.small$account_type=="Left"),])),
  ci.l =  c(quantile(boot.right$t, c(.025,.975))[[1]],
             quantile(boot.left$t, c(.025,.975))[[1]]),
  ci.h =  c(quantile(boot.right$t, c(.025,.975))[[2]],
             quantile(boot.left$t, c(.025,.975))[[2]]),
  Group = c("Right", "Left")
)

ggplot(data = plotdat2) + geom_point(aes(x=Group, y=minfo), color = c("Red", "Blue")) +
  geom_hline(yintercept = min_info, linetype="dashed") +
  geom_errorbar(aes(ymin=ci.l, ymax=ci.h, x = Group), colour="black", width=.1) +
  ggtitle("Mutual Information by Group w/ 95% CI") + ylim(0,12) +
  annotate("text", label="Maximum value (minimum info)",
           x=1, y=min_info+.3,
           size=4, fontface="italic") +
  ylab("Mutual Information")


# Diversity -----
# Gini Coefficient
# fails when it is zero, row gets NA
gini_trolls_tweets <- data.frame(full = apply(trolls_scored[,c(-1,-2)],1,reldist::gini),
                          dich = apply(trolls_models$full_model$X,1,reldist::gini),
                          group = trolls$account_type)
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

# .7 correlation between the two per tweet, less NA's with full (not dich)
cor(na.omit(gini_trolls_tweets[c("full","dich")]))

# .3 correlation between the two per author
cor(na.omit(gini_trolls_author[c("full","dich")]))

aggregate(full ~ group, gini_trolls_author, FUN=median)


# Prediction -----

