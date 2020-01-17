# --- DADA Fall 2019 --- #
# Tara L. Valladares
# November 7th, 2019
# - Emoxicon Walkthrough - #

## Install the Emoxicon package

#install.packages("mirt")
library(mirt)

## Load the data
data(trolls)

str(trolls)

table(trolls$account_type)

groups <- trolls$author

## Score the data
t<-Sys.time()
trolls_scored_w <- emoxicon(text=trolls$content, lexicon = "weights",
                                 exclude = c("hilary", "hillary", "russia", "russian",
                                             "trump", "bernie", "clinton"))
Sys.time()-t

t<-Sys.time()
trolls_scored <- emoxicon(text=trolls$content, lexicon = "emotions",
                            exclude = c("hilary", "hillary", "russia", "russian",
                                        "trump", "bernie", "clinton", "rt"))
Sys.time()-t

t<-Sys.time()
trolls_scored_f <- emoxicon(text=trolls$content, lexicon = emotionsfreq,
                          exclude = c("hilary", "hillary", "russia", "russian",
                                      "trump", "bernie", "clinton", "rt"))
Sys.time()-t


## Run the Rasch  model

# trolls_RM <- rasch(trolls_scored, groups=trolls$author)
trolls_RM_f <- rasch(trolls_scored_f, groups=trolls$author)
trolls_RM_w <- rasch(trolls_scored_w, groups=trolls$author)
trolls_RM_wf <- rasch(trolls_scored_wf, groups=trolls$author)

exists("trolls_RM")
exists("trolls_RM_f")
exists("trolls_RM_w")
exists("trolls_RM_wf")
## Inspect Fit

mirt::empirical_plot(trolls_RM[[1]][["X"]],1:8)
mirt::empirical_plot(trolls_RM_f[[1]][["X"]],1:8)
mirt::empirical_plot(trolls_RM_w[[1]][["X"]],1:8)
mirt::empirical_plot(trolls_RM_wf[[1]][["X"]],1:8)

pp<-eRm::person.parameter(rasch_fit)
pfit<- eRm::personfit(pp)
pmis <- eRm::PersonMisfit(pp)
ifit<-eRm::itemfit(pp)

# look at the lexicon

dplyr::sample_n(subset(emotions, HAPPY == 1), 20)


library(ggplot2)
x<-trolls_RM[[3]]
x<-reshape2::melt(trolls_RM[[3]])
mt <- ggplot(x,
             aes(value, colour = factor(variable), fill=factor(variable))) +
  geom_histogram()

mt + facet_grid(. ~ variable, scales = "fixed") +ggtitle("All Trolls")

mt2 <- ggplot(x[which(dol$accountType=="Left"),], aes(value, colour = factor(variable), fill=factor(variable))) +
  geom_histogram()

mt2 + facet_grid(. ~ variable, scales = "fixed")+ggtitle("Left Trolls")



#### testing ####

t<-Sys.time()
trolls_scored <- emoxicon(text=trolls$content, lexicon = "emotions",
                          exclude = c("hilary", "hillary", "russia", "russian",
                                      "trump", "bernie", "clinton", "rt"))
Sys.time()-t

group2 <- trolls$author

t<-Sys.time()
x<- rasch(scores= trolls_scored, groups = group2, return_models = TRUE)
Sys.time()-t

## Small trolls
data(trolls)
set.seed(1)
tinyTrolls <- trolls[which(trolls$author%in%sample(unique(trolls$author), size=50)),]

t<-Sys.time()
tinyTrolls_scored <- emoxicon(text=tinyTrolls$content, lexicon = "emotions",
                          exclude = c("hilary", "hillary", "russia", "russian",
                                      "trump", "bernie", "clinton", "rt"))
Sys.time()-t
group_lengths <-
  sapply(unique(tinyTrolls$author), function(x) {
    NROW(tinyTrolls_scored[which(tinyTrolls$author == x), ])
  })



t<-Sys.time()
st_rasch<- emoxicon::rasch(scores= tinyTrolls_scored, groups = tinyTrolls$author, return_models = TRUE)
Sys.time()-t


t<-Sys.time()
xx<-category_order(scores = tinyTrolls_scored[c("AFRAID", "AMUSED", "ANGRY", "ANNOYED", "DONT_CARE",
                                                 "HAPPY", "INSPIRED", "SAD")], groups= tinyTrolls$author)
Sys.time()-t

categorys<-st_rasch$category_order
catplot(categorys = categorys)
