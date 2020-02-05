library(ape)

d = read.csv("~/Dropbox/hz_metaanalysis/data_analysis/cleaned_combined_data-Jul-18-19.csv", stringsAsFactors = F)

# get unique taxa, just take one side
taxa = unique(d$Taxon1)
# cleanup names
taxa1 = lapply(strsplit(taxa, " "), function(x) {paste(x[1], x[2], sep="_")})
taxa1 = unlist(taxa1)
taxa1 = gsub(",", "", taxa1)
names(taxa1) = taxa

t = read.tree("~/Dropbox/hz_metaanalysis/TimeTree/Metazoa_species.nwk")
cat(taxa[! (taxa1 %in% t$tip.label) ], sep = "\n")

# Pseudaris crucifer Interior
taxa1[which(taxa == 'Pseudaris crucifer Interior')] = 'Pseudacris_crucifer'
# Rana lessonae N
taxa1[which(taxa == 'Rana lessonae N')] = 'Pelophylax_lessonae'
# Arion vulgaris; replace with close relative
taxa1[which(taxa == 'Arion vulgaris')] = 'Arion_silvaticus'
# Setophaga auduboni; replace with other half of pair
taxa1[which(taxa == 'Setophaga auduboni')] = 'Setophaga_coronata'
# Setophaga auduboni N; replace with close relative
taxa1[which(taxa == 'Setophaga auduboni N')] = 'Setophaga_dominica'
# Artemisiospiza nevadensis; replace with other half of pair
taxa1[which(taxa == 'Artemisiospiza nevadensis')] = 'Amphispiza_belli'
# Spalax ehrenbergi (Spalax carmeli) 58
taxa1[which(taxa == 'Spalax ehrenbergi (Spalax carmeli) 58')] = 'Spalax_carmeli'
# Spalax ehrenbergi (Spalax galili) 52; congeneric (cannot find phylogeny)
taxa1[which(taxa == 'Spalax ehrenbergi (Spalax galili) 52')] = 'Spalax_arenarius'
# Spalax ehrenbergi (Spalax golani) 54; congeneric (cannot find phylogeny)
taxa1[which(taxa == 'Spalax ehrenbergi (Spalax golani) 54')] = 'Spalax_graecus'
# Triturus anatolicus; replace with close relative
taxa1[which(taxa == 'Triturus anatolicus')] = 'Triturus_karelinii'
# Bufo viridis
taxa1[which(taxa == 'Bufo viridis')] = 'Bufotes_viridis'
# Chorthippus brunneus; close relative
taxa1[which(taxa == 'Chorthippus brunneus')] = 'Chorthippus_curtipennis'
# Chorthippus parallelus parallelus; close relative
taxa1[which(taxa == 'Chorthippus parallelus parallelus')] = 'Chorthippus_curtipennis'
# Cnemidophorus punctilinealis; replace with other half of pair
taxa1[which(taxa == 'Cnemidophorus punctilinealis')] = 'Aspidoscelis_marmorata'
# Cottus rhenanus; close relative
taxa1[which(taxa == 'Cottus rhenanus')] = 'Cottus_carolinae'
# Fundulus olivaceus; replace with close relative
taxa1[which(taxa == 'Fundulus olivaceus')] = 'Fundulus_chrysotus'
# Geocrinia laevis; replace with other half
taxa1[which(taxa == 'Geocrinia laevis')] = 'Geocrinia_victoriana'
# Gryllus pennsylvanicus; replace with relative
taxa1[which(taxa == 'Gryllus pennsylvanicus')] = 'Gryllus_bimaculatus'
# Hoplodactylus maculatus; close relative
taxa1[which(taxa == 'Hoplodactylus maculatus')] = 'Hoplodactylus_duvaucelii'
# Lacerta lepida nevadensis; not super close congeneric
taxa1[which(taxa == 'Lacerta lepida nevadensis')] = 'Lacerta_viridis'
# Lycaeides idas
taxa1[which(taxa == 'Lycaeides idas')] = 'Plebejus_idas'
# Cinnyris moreaui
taxa1[which(taxa == 'Cinnyris moreaui')] = 'Nectarinia_moreaui'
# Oporornis tolmiei
taxa1[which(taxa == 'Oporornis tolmiei')] = 'Geothlypis_tolmiei'
# Orchelimum nigripes; in the same subfamily (!!!)
taxa1[which(taxa == 'Orchelimum nigripes')] = 'Conocephalus_maculatus'
# Passer italiae; other half
taxa1[which(taxa == 'Passer italiae')] = 'Passer_domesticus'
# Patella rustica C; not that closely related but same genus
taxa1[which(taxa == 'Patella rustica C')] = 'Patella_lugubris'
# Poecile atricapillus
taxa1[which(taxa == 'Poecile atricapillus')] = 'Poecile_atricapilla'
# Pontia daplidice; same genus
taxa1[which(taxa == 'Pontia daplidice')] = 'Pontia_callidice'

# Eunicea flexuosa lineage 1
taxa1[which(taxa == '')] = ''
# Vandiemenella viatica P24(XY)
taxa1[which(taxa == '')] = ''
# Hemideina thoracica 15
taxa1[which(taxa == '')] = ''
# Hemideina thoracica 17
taxa1[which(taxa == '')] = ''
# Hemideina thoracica 17'
taxa1[which(taxa == '')] = ''
# Hemideina thoracica 19
taxa1[which(taxa == '')] = ''
# Hemideina thoracica 23
taxa1[which(taxa == '')] = ''

write.csv(data.frame(tip_taxa = taxa1), "~/Dropbox/hz_metaanalysis/TimeTree/tips.csv", , row.names = T)
