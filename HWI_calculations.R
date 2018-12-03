setwd("/Users/jaymcentee/Dropbox/hz_metaanalysis/bird_morph/")

data <- read.csv("all_specimens_for_hz_analyses.csv")
##Additional hz systems not in spreadsheet: 31, 124, 113, 57
males <- data[which(data$Sex == "m"),]
females <- data[which(data$Sex == "f"),]
unknowns <- data[which(data$Sex == "u"),]

sp.sampling <- tapply(data$HWI, data$Scientific_Name, length)
hz.sampling <- tapply(data$HWI, data$System_ID, length)
##mean(sp.sampling)
## 4.38 specimens per taxon
## 1.88 SD
##mean(hz.sampling)
##6.514 specimens per hybrid zone
##sd(hz.samlping)
##3.760 SD

male_means <- tapply(males$HWI, males$Scientific_Name, mean)
female_means <- tapply(females$HWI, females$Scientific_Name, mean)
unknown_means <- tapply(unknowns$HWI, unknowns$Scientific_Name, mean)


means <- as.data.frame(cbind(male_means, female_means, unknown_means))
means$mean_of_sexes <- rowMeans(means, na.rm = TRUE)

setwd("/Users/jaymcentee/Dropbox/hz_metaanalysis/data_analysis/")

all_hzs <- read.csv("cleaned_combined_data-Jul-11-18.csv", stringsAsFactors = F, na.strings = c("", "NA"))
bird_hzs <- all_hzs[which(all_hzs$critter == "bird"),]
bird_sys_taxa <- unique(bird_hzs[,1:3])

bird_sys_taxa$binomial_1<-lapply(bird_sys_taxa$Taxon1, function(x) strsplit(x, split=" "))
bird_sys_taxa$binomial_1<-unlist(lapply(bird_sys_taxa$binomial_1, function(x) paste(x[[1]][1],x[[1]][2], sep=" ")))
bird_sys_taxa$binomial_2<-lapply(bird_sys_taxa$Taxon2, function(x) strsplit(x, split=" "))
bird_sys_taxa$binomial_2<-unlist(lapply(bird_sys_taxa$binomial_2, function(x) paste(x[[1]][1],x[[1]][2], sep=" ")))

bird_sys_taxa$HWI_1 <- rep(NA, length(bird_sys_taxa[,1]))
bird_sys_taxa$HWI_2 <- rep(NA, length(bird_sys_taxa[,1]))
bird_sys_taxa$HWI_mean <- rep(NA, length(bird_sys_taxa[,1]))

taxa <- cbind(bird_sys_taxa$binomial_1, bird_sys_taxa$binomial_2)
rownames(means)[which(!rownames(means) %in% taxa)]

for (i in 1:length(means[,1])){
	if(rownames(means)[i] %in% bird_sys_taxa$binomial_1){
		bird_sys_taxa$HWI_1[which(bird_sys_taxa$binomial_1 == rownames(means)[i])] <- means$mean_of_sexes[i]
		}
}

for (i in 1:length(means[,1])){
	if(rownames(means)[i] %in% bird_sys_taxa$binomial_2){
		bird_sys_taxa$HWI_2[which(bird_sys_taxa$binomial_2 == rownames(means)[i])] <- means$mean_of_sexes[i]	
	}
}

bird_sys_taxa$HWI_mean <- rowMeans(bird_sys_taxa[,6:7], na.rm = TRUE)

hist(bird_sys_taxa$HWI_1 - bird_sys_taxa$HWI_2, breaks = 10)


setwd("/Users/jaymcentee/Dropbox/hz_metaanalysis/bird_morph/")
write.csv(bird_sys_taxa, "bird_HWIs.csv")