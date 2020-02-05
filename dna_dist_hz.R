library(ape)

calc_dist_dna <- function(file, suffix1, suffix2) {
	# file should be aligned seq in fasta format
	# calculates the distance matrix
	# plots the tree
	# finds min, mean, median, max distance between two described taxa
	# taxa are defined by suffices, indicated by user as suffix 1 and suffix 2
	#	suffices are assumed to be preceded by _
	
	# dna matrix
	m = read.dna(file, format="fasta")
	
	taxa = dimnames(m)[[1]]
	# define taxa 1
	taxa1 = taxa[grep(paste('_', suffix1, '$', sep=""), taxa)]
	# define taxa 2
	taxa2 = taxa[grep(paste('_', suffix2, '$', sep=""), taxa)]
	
	m = m[dimnames(m)[[1]] %in% c(taxa1, taxa2), ]
		
	# plot tree for user
	dTN93 = dist.dna(m, model="TN93", pairwise.deletion=T)
	dK80 = dist.dna(m, pairwise.deletion=T)
	# neighbor joining tree
	tree = bionjs(dTN93)
	colors = rep('navyblue', length(tree$tip.label))
	colors[tree$tip.label %in% taxa1] = 'maroon'
	 # make tip labels smaller
	if (length(tree$tip.label) > 150) {
		tipcex = 0.5
	} else if (length(tree$top.label > 75)) {
		tipcex = 0.75
	} else {
		tipcex = 1
	}
	par(mfrow=c(1,2))
	plot(tree, cex=tipcex, tip.color=colors, type="unrooted")
	
	# calculate distances
	dTN93_m = as.matrix(dTN93)
	dTN93_m_sub = dTN93_m[taxa1, taxa2]
	
	dK80_m = as.matrix(dK80)
	dK80_m_sub = dK80_m[taxa1, taxa2]

	vals_TN93 = quantile(dTN93_m_sub, c(0, 0.025, 0.5, 0.975, 1), na.rm=T)
	mean_TN93 = mean(dTN93_m_sub, na.rm=T)
	
	vals_K80 = quantile(dK80_m_sub, c(0, 0.025, 0.5, 0.975, 1), na.rm=T)
	mean_K80 = mean(dK80_m_sub, na.rm=T)
	
	hist(dTN93_m_sub, col="gray", border='gray', main=NULL, xlab="genetic p-distance", ylab="count")
	for (i in 1:length(vals_TN93)) {
		abline(v=vals_TN93[i], col="maroon")
	}
	abline(v=mean_TN93, col="navyblue")

	cat("Length of sequences is: ", dim(m)[2], "\n", sep="")
	cat("Number of individuals in Group 1 is: ", length(taxa1), "\n", sep="")
	cat("Number of individuals in Group 2 is: ", length(taxa2), "\n", sep="")

	# return results as vector
	res<-rbind(vals_TN93, vals_K80)
	mean<-c(mean_TN93, mean_K80)
	res<-cbind(res,mean)
	rownames(res)<-c("TN93", "K80")
	print(res)
	return(tree)
}

t = calc_dist_dna("~/Dropbox/hz_metaanalysis/fastas/jay_used_for_dist_calc/Mytilus_edulis_trossulus_COIII_aligned.fasta", '1', '2')

dev.off()
tips = t$tip.label
colors = rep("black", length(tips))
colors[ grep("_2$", tips)] = "red"
plot(t, tip.color = colors, cex = 0.6)
