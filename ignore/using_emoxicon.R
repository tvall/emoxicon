## Using the Emoxicon package

library(emoxicon)

# load data
data(tinytrolls)

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


# Still to do:
# model summary
# auto uses the eRm functions
coef(tt_rasch$full_model)
summary(tt_rasch$full_model)

# item measures, ability measures

# fit measures
tt_fit<-rasch_fit(tt_rasch, groups = TRUE)

tt_fit2 <- rasch_fit(tt_rasch, groups = FALSE)

# the llr plot

# force eRm to load alongside emoxicon

# separate function for the entropy/TI (can call it within this function)

# document everything
