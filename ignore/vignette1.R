seed(1)
tinyTrolls <- trolls[which(trolls$author%in%sample(unique(trolls$author), size=50)),]

tinyTrolls_scored <- emoxicon(text=tinyTrolls$content, lexicon = "emotions",
                              exclude = c("hilary", "hillary", "russia", "russian",
                                          "trump", "bernie", "clinton", "rt"))

tt_rasch<- emoxicon::rasch(scores= tinyTrolls_scored, groups = tinyTrolls$author, return_models = TRUE)

catplot(categorys = tt_rasch$category_order)
