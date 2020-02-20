## Using the Emoxicon package

library(emoxicon)

# load data, make a smaller set for speed
data(trolls)
set.seed(1)
tinyTrolls <- trolls[which(trolls$author%in%sample(unique(trolls$author), size=50)),]

# score the data
t<-Sys.time()
tinyTrolls_scored <- emoxicon(text=tinyTrolls$content, lexicon = "emotions",
                              exclude = c("hilary", "hillary", "russia", "russian",
                                          "trump", "bernie", "clinton", "rt"))
Sys.time()-t

# run the rasch model
t<-Sys.time()
tt_rasch<- rasch(scores= tinyTrolls_scored, groups = tinyTrolls$author, return_models = TRUE)
Sys.time()-t

# look at a category plot
catplot(tt_rasch$category_order)
