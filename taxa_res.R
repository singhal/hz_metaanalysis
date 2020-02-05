



coefbest <- ms@objects[[1]]$coefficients
coefres[2 * i - 1, 'critter'] = names(t)[i]
coefres[2 * i, 'critter'] = names(t)[i]
coefres[2 * i - 1, 'intercept'] = coefbest[1]
coefres[2 * i, 'intercept'] = coefbest[1]
coefres[2 * i - 1, 'variable'] = 'log_dist'
coefres[2 * i, 'variable'] = 'log_dispersal_ind'
if (length(which(names(coefbest) == "log_dist")) > 0) {
  coefres[2 * i - 1, 'coef'] = coefbest[which(names(coefbest) == "log_dist")]
}
if (length(which(names(coefbest) == "log_dispersal_ind")) > 0) {
  coefres[2 * i, 'coef'] = coefbest[which(names(coefbest) == "log_dispersal_ind")]
}
se_combine <- function(x) {
  return(paste(x[1], " (SE=", x[2], ")", sep=""))
}
taxares[i , "best_model"] = summary(ms)$bestmodel
taxares[i , "rsq"] = summary(ms@objects[[1]])$adj.r.squared
if (length(which(names(coefbest) == "log_dispersal_ind")) > 0) {
  taxares[i , "dispersal"] = se_combine(round(coef(summary(ms@objects[[1]]))["log_dispersal_ind", 1:2], 2))
}
if (length(which(names(coefbest) == "log_dist")) > 0) {
  taxares[i , "mtDNA_dist"] = se_combine(round(coef(summary(ms@objects[[1]]))["log_dist", 1:2], 2))
}
if (length(which(names(coefbest) == "log_dispersal_ind:log_dist")) > 0) {
  taxares[i , "interaction"] = se_combine(round(coef(summary(ms@objects[[1]]))["log_dispersal_ind:log_dist", 1:2], 2))
}
taxares[i , "dispersalRI"] = round(coef_ests["log_dispersal_ind","Importance"], 2)
taxares[i , "mtDNA_distRI"] = round(coef_ests["log_dist","Importance"], 2)
taxares[i , "interactionRI"] = round(coef_ests["log_dispersal_ind:log_dist", "Importance"], 2)


}

knitr::kable(taxares)
write.csv(taxares, paste(outdir, "taxon_results.csv", sep=""), row.names = T)

##Subset data to those with nuclear distance estimates
t_nDNA <- list()
for(i in 1:length(t)){
  t_nDNA[[i]] <- t[[i]][complete.cases(t[[i]]$log_mean_nDNA),]
}

for (i in 1:length(t_nDNA)) {
  t_nDNA[[i]]$log_dispersal = scale(log(t_nDNA[[i]]$dispersal))
  t_nDNA[[i]]$log_dispersal_ind = scale(log(t_nDNA[[i]]$dispersal_only_ind))
  t_nDNA[[i]]$log_dist = scale(log(t_nDNA[[i]]$mean_dist))
  t_nDNA[[i]]$log_max_dist = scale(log(t_nDNA[[i]]$max_dist))
  t_nDNA[[i]]$log_mean_nDNA = scale(log(t_nDNA[[i]]$mean_nDNA))
  t_nDNA[[i]]$log_max_nDNA = scale(log(t_nDNA[[i]]$max_nDNA))
}

##nDNA amphibians
amphib_nDNA <- glmulti(y = "log_width", xr = c("log_dispersal_ind", "log_dist",
                                               "log_mean_nDNA"), data = t_nDNA[[1]], level = 2, fitfunction = lm, marginality
                       = TRUE, crit = "aicc")
amphib_nDNA_MS <- glmulti_mod_sel_table(amphib_nDNA)
amphib_nDNA_coefs <- coef(amphib_nDNA)
write.csv(amphib_nDNA_MS, paste(outdir, "amphib_nDNA_MS.csv", sep=""), row.names = T)
write.csv(amphib_nDNA_coefs, paste(outdir, "amphib_nDNA_coefs.csv", sep = ""), row.names = T)

##nDNA birds
birds_nDNA <- glmulti(y = "log_width", xr = c("log_dispersal_ind", "log_dist",
                                              "log_mean_nDNA"), data = t_nDNA[[2]], level = 2, fitfunction = lm, marginality
                      = TRUE, crit = "aicc")
birds_nDNA_MS <- glmulti_mod_sel_table(birds_nDNA)
birds_nDNA_coefs <- coef(birds_nDNA)
write.csv(birds_nDNA_MS, paste(outdir, "birds_nDNA_MS.csv", sep = ""), row.names = T)
write.csv(birds_nDNA_coefs, paste(outdir, "birds_nDNA_coefs.csv", sep = ""), row.names = T)

##nDNA insects
insects_nDNA <- glmulti(y = "log_width", xr = c("log_dispersal_ind", "log_dist",
                                                "log_mean_nDNA"), data = t_nDNA[[3]], level = 2, fitfunction = lm, marginality
                        = TRUE, crit = "aicc")
insects_nDNA_MS <- glmulti_mod_sel_table(insects_nDNA)
insects_nDNA_coefs <- coef(insects_nDNA)
write.csv(insects_nDNA_MS, paste(outdir, "insects_nDNA_MS.csv", sep = ""), row.names = T)
write.csv(insects_nDNA_coefs, paste(outdir, "insects_nDNA_coefs.csv", sep = ""), row.names = T)

mammals_nDNA <- glmulti(y = "log_width", xr = c("log_dispersal_ind", "log_dist",
                                                "log_mean_nDNA"), data = t_nDNA[[4]], level = 2, fitfunction = lm, marginality
                        = TRUE, crit = "aicc")
mammals_nDNA_MS <- glmulti_mod_sel_table(mammals_nDNA)
mammals_nDNA_coefs <- coef(mammals_nDNA)
write.csv(mammals_nDNA_MS, paste(outdir, "mammals_nDNA_MS.csv", sep = ""), row.names = T)
write.csv(mammals_nDNA_coefs, paste(outdir, "mammals_nDNA_coefs.csv", sep = ""), row.names = T)

NAR_nDNA <- glmulti(y = "log_width", xr = c("log_dispersal_ind", "log_dist",
                                            "log_mean_nDNA"), data = t_nDNA[[5]], level = 2, fitfunction = lm, marginality
                    = TRUE, crit = "aicc")
NAR_nDNA_MS <- glmulti_mod_sel_table(NAR_nDNA)
NAR_nDNA_coefs <- coef(NAR_nDNA)
write.csv(NAR_nDNA_MS, paste(outdir, "NAR_nDNA_MS.csv", sep = ""), row.names = T)
write.csv(NAR_nDNA_coefs, paste(outdir, "NAR_nDNA_coefs.csv", sep = ""), row.names = T)

taxares = data.frame(critter=names(t), 
                     num_studies=unlist(lapply(t, nrow)), 
                     best_model=rep(NA, length(t)), 
                     rsq=rep(NA, length(t)), 
                     dispersal=rep(NA, length(t)), 
                     mtDNA_dist=rep(NA, length(t)), 
                     interaction = rep(NA, length(t)),
                     dispersalRI =rep(NA, length(t)), 
                     mtDNA_distRI =rep(NA, length(t)), 
                     interactionRI = rep(NA, length(t)))

coefres = data.frame(critter = rep(NA, 2 * length(t)),
                     variable = rep(NA, 2 * length(t)),
                     coef = rep(NA, 2 * length(t)),
                     intercept = rep(NA, 2 * length(t)))