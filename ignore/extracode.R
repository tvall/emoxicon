
# save item names
z <- names(ifit$i.fit)

# look at misfitting items within individual models
manymods_ifit_total <- sapply(1:length(manymods_ifit), function(i){

  p <- round(pchisq(manymods_ifit[[i]]$i.fit,
                    df=manymods_ifit[[i]]$i.df-1, lower.tail=FALSE),3)

  items <- as.data.frame(t(unlist(c(model$group_models[[i]]$group, p))),
                         row.names = model$group_models[[i]]$group)
  colnames(items)[1] <- "model"

  if(ncol(items) < (length(z)+1)){
    items[,z[which(!z%in% colnames(items))]]<-NA
  }
  items

}, simplify = F)

manymod_ifit_total<-list("Chi-Square Item Fit" = do.call(rbind, manymods_ifit_total))
