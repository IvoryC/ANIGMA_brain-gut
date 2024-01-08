#' ---
#' title: "Diversity"
#' output: html_document
#' date: "2023-09-15"
#' ---
#' 
## ----setup, include=FALSE-------------------------------------------------------------------------------------------------
# I prefer to edit the Rmd file and then use purl to convert into a R script.
# knitr::purl(input=dir(pattern="Rmd"), documentation = 2L)
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # ANIGMA cohort Taxa
#' 
#' ### libraries
#' 
## -------------------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(vegan)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

#' 
#' A named vector of color vectors.  Most of these match the previous paper. 
## ----fig.height=4, fig.width=2, echo=FALSE, include=FALSE-----------------------------------------------------------------
themeFile = "../../input/themes.R"
if (file.exists(themeFile)){
    source(themeFile)
    myColorPalette = getColorPalette()
    displayColorPalette(myColorPalette)
}else{
    myColorPalette = c(
        # timepoint
        nonED = "#1B9E77",
        HC = "#1B9E77",
        T1 = "#D95F02",
        AN.AD = "#D95F02",
        T2 = "#7570B3",
        AN.DS = "#7570B3",
        # site
        Denver = "#1F78B4",
        ACUTE = "#1F78B4",
        UNC = "#F0027F",
        CEED = "#F0027F",
        FARGO = "coral",
        # subgroups indicating site AND group/timepoint
        UNC_HC = "#B2DF8A",
        Denver_HC = "#33A02C",
        UNC_T1 = "#FDBF6F",
        UNC_T2 = "#FF7F00",
        Denver_T1 = "#CAB2D6",
        Denver_T2 = "#6A3D9A"
    )
    # plot.new(); legend(legend=names(myColorPalette), 
    #                    col=myColorPalette, bty="n", x="center", pch=15, cex=2)
    
    colorKey = data.frame(color=myColorPalette, key=names(myColorPalette))
    # ggplot(data=colorKey, aes(x=1, y = 1:nrow(colorKey), fill=key, label=key)) +
    #     geom_tile() +
    #     scale_fill_manual(values = myColorPalette) +
    #     theme_void()+
    #     theme(legend.position="none") + 
    #     geom_text()
}

# Use the theme_classic() ggplot theme unless otherwise indicated.
theme_set(theme_classic())

#' 
#' Source the plot functions that go with this module.
## -------------------------------------------------------------------------------------------------------------------------
source("../resources/PlotDiversityRichness.R")

#' 
#' 
#' Pick a taxonomic level. Take an argument if one is given.
## -------------------------------------------------------------------------------------------------------------------------
args = commandArgs(trailingOnly=TRUE)
if (length(args) > 0){
    taxaLevel = args[1]
}else{
    taxaLevel = "species"
}

#' 
#' Direct output
## -------------------------------------------------------------------------------------------------------------------------
outDir = file.path("..", "output", taxaLevel)
suppressWarnings( dir.create(outDir, recursive = T) )

#' 
#' ### Read counts data
#' 
## -------------------------------------------------------------------------------------------------------------------------
input=c(
    domain = "../../input/counts-tables/filtered-counts_level-1_domain.csv",
    phylum = "../../input/counts-tables/filtered-counts_level-2_phylum.csv",
    class = "../../input/counts-tables/filtered-counts_level-3_class.csv",
    order = "../../input/counts-tables/filtered-counts_level-4_order.csv",
    family = "../../input/counts-tables/filtered-counts_level-5_family.csv",
    genus = "../../input/counts-tables/filtered-counts_level-6_genus.csv",
    species = "../../input/counts-tables/filtered-counts_level-7_species.csv")

# taxaLevel = "class"
countsFile = input[taxaLevel]

#' 
#' We are looking at the ``r taxaLevel`` level, so we will read file ``r countsFile``.
#' 
## -------------------------------------------------------------------------------------------------------------------------
countsAndMetaALL = read.csv(countsFile)
dim(countsAndMetaALL)

#' 
#' Make a table with all the metadata that came with the artifact.
## -------------------------------------------------------------------------------------------------------------------------
qzaMeta = countsAndMetaALL %>% select(id.orig, StudyID, Timepoint)

#' 
#' Split the meta data and counts
## -------------------------------------------------------------------------------------------------------------------------
metaCols = c("id.orig", "BarcodeNumber", "BarcodeSequenceFull", "BarcodeSequence", "LinkerPrimerSequence", "StudyID", "Timepoint", "Person")
counts = countsAndMetaALL %>% select(-all_of(metaCols))
row.names(counts) = countsAndMetaALL$id.orig
key = countsAndMetaALL %>% select(id.orig, StudyID, Timepoint)

#' 
#' ### Read meta data.
#' 
#' Read the original metadata file.
## -------------------------------------------------------------------------------------------------------------------------
meta = read.delim("../../input/meta/ANIGMA-metadata.txt") %>%
  select(PARTICIPANT.ID, LOCATION, TIMEPOINT, AGE, SUBTYPE, BMI, 
         STAI_Y1, STAI_Y2, STAI_TOTAL, PSS, DAYS_TREAT, Weight_kg, DNA.ID, DUR_ILLNESS_YRS) %>%
  filter(DNA.ID %in% countsAndMetaALL$id.orig)
row.names(meta) = meta$DNA.ID
dim(meta)

#' 
#' Check that the meta data that came with the qiime artifact matches up to the main project meta data.
## -------------------------------------------------------------------------------------------------------------------------
m = merge(meta, key, by.x="DNA.ID", by.y="id.orig")
table(m$PARTICIPANT.ID == m$StudyID)
table(m$TIMEPOINT == m$Timepoint)

#' 
#' The metadata from the qiime artifact and the metadata for the project match up. There is one that has an error in the qiime artifact metadata. That's fine. We'll use the project metadata moving forward.
## -------------------------------------------------------------------------------------------------------------------------
m[which(m$TIMEPOINT != m$Timepoint), c("PARTICIPANT.ID", "TIMEPOINT", "BMI", "Timepoint")]

#' ## Merge
#' 
#' Merge the meta data with the counts data.
## -------------------------------------------------------------------------------------------------------------------------
m = merge(meta, key, by.x="DNA.ID", by.y="id.orig")
table(m$PARTICIPANT.ID == m$StudyID)
table(m$TIMEPOINT == m$Timepoint)

#' 
#' # MAIN
#' 
#' ## Diversity 
#' 
#' Diversity on raw counts.
## -------------------------------------------------------------------------------------------------------------------------
div.alpha = vegan::diversity(counts, index="shannon")
meta$alpha.div.raw = div.alpha[row.names(meta)]

#' 
## ----plotDiv--------------------------------------------------------------------------------------------------------------
# source("../resources/PlotDiversityRichness.R")
# set defaults for this document
plotDiv = function(df, column, sig=0.05, saveToDir=outDir, tLevel=taxaLevel){
  plotDiversity(df, column=column, sig=sig, saveToDir=saveToDir, tLevel=tLevel)
}

plotDiv(meta, column="alpha.div.raw", sig=0.05)

#' 
#' Looking at the family level, HC is not very different from T1.  But T2 is less diverse.
#' 
#' ### Split Diversity by site
#' 
#' Diversity by site
#' 
## -------------------------------------------------------------------------------------------------------------------------
plotDiv(meta %>% filter(LOCATION=="ACUTE"), column="alpha.div.raw", saveToDir=NA) + ggtitle("ACUTE")
plotDiv(meta %>% filter(LOCATION=="CEED"), column="alpha.div.raw", saveToDir=NA) + ggtitle("CEED")
plotDiv(meta %>% filter(LOCATION=="FARGO"), column="alpha.div.raw", saveToDir=NA) + ggtitle("FARGO")

#' 
#' The change in diversity is site specific.
#' 
#' Is that driven by the controls from each site or the AN from each site?
#' 
#' In each comparison, use all controls.
## -------------------------------------------------------------------------------------------------------------------------
plotDiv(meta %>% filter(LOCATION=="ACUTE" | TIMEPOINT=="HC"), column="alpha.div.raw", saveToDir=NA) + ggtitle("ACUTE")
plotDiv(meta %>% filter(LOCATION=="CEED" | TIMEPOINT=="HC"), column="alpha.div.raw", saveToDir=NA) + ggtitle("CEED")
plotDiv(meta %>% filter(LOCATION=="FARGO" | TIMEPOINT=="HC"), column="alpha.div.raw", saveToDir=NA) + ggtitle("FARGO")

#' 
#' Using the site-specific controls, or all controls as a group, these are still true:
#' 
#'  * At ACUTE, HC and T1 are not significantly different in terms of diversity, but T2 is less (and the diff significant against either T1 or HC).
#'  * At CEED, there is not significant difference between any of the groups.
#'  * At FARGO, T1 and T2 are not significantly different from each other, but both are greater than HC.
#' 
#' This is based on looking at the family level data.
#' 
#' Save a pdf with these figures with matched next to each other:
## -------------------------------------------------------------------------------------------------------------------------
pdf(file=file.path(outDir, "diversity-by-site_joined-controls.pdf"))
plotDiv(meta %>% filter(LOCATION=="ACUTE"), column="alpha.div.raw", saveToDir=NA) + 
    ggtitle("ACUTE")
plotDiv(meta %>% filter(LOCATION=="ACUTE" | TIMEPOINT=="HC"), column="alpha.div.raw", saveToDir=NA) + 
    ggtitle("ACUTE - with all controls")

plotDiv(meta %>% filter(LOCATION=="CEED"), column="alpha.div.raw", saveToDir=NA) + 
    ggtitle("CEED")
plotDiv(meta %>% filter(LOCATION=="CEED" | TIMEPOINT=="HC"), column="alpha.div.raw", saveToDir=NA) + 
    ggtitle("CEED - with all controls")

plotDiv(meta %>% filter(LOCATION=="FARGO"), column="alpha.div.raw", saveToDir=NA) + 
    ggtitle("FARGO")
plotDiv(meta %>% filter(LOCATION=="FARGO" | TIMEPOINT=="HC"), column="alpha.div.raw", saveToDir=NA) + 
    ggtitle("FARGO - with all controls")
dev.off()

#' 
#' 
#' ### Diversity on Rarefied counts
#' 
#' Diversity on rarified data.
## -------------------------------------------------------------------------------------------------------------------------
counts_rarified = vegan::rarefy(counts, sample=rep(min(rowSums(counts)), nrow(counts)), MARGIN = 1)

counts_rarified = vegan::rrarefy(counts, sample=rep(min(rowSums(counts)), nrow(counts)))


div.alpha.rare = vegan::diversity(counts_rarified, index="shannon")
meta$alpha.div.rarified = div.alpha.rare[row.names(meta)]
plotDiv(meta, column="alpha.div.rarified")

#' 
#' 
#' ## Richness
#' 
#' richness on raw counts:
## -------------------------------------------------------------------------------------------------------------------------
countRichnessSimple <- function(sampleCounts){
  return(suppressWarnings(sum(sampleCounts > 0)))
}
rich.raw = apply(counts, countRichnessSimple, MARGIN=1)
meta$richness.raw = rich.raw[row.names(meta)]

#' 
## ----plotRich-------------------------------------------------------------------------------------------------------------
# source("../resources/PlotDiversityRichness.R")
# set defaults for this document
plotRich = function(df, column, sig=0.05, saveToDir=outDir, tLevel=taxaLevel){
    plotRichness(df, column, sig=sig, saveToDir=saveToDir, tLevel=tLevel)
}

plotRich(meta, column="richness.raw")

#' 
#' ### Richness with greater than 1 count
#' 
## -------------------------------------------------------------------------------------------------------------------------
countRichnessThreshold <- function(sampleCounts, threshold=0){
  return(suppressWarnings(sum(sampleCounts > threshold)))
}

meta$richness.over0 = apply(counts, countRichnessThreshold, MARGIN=1, threshold=0)[row.names(meta)]
plotRich(meta, column="richness.over0", saveToDir=NA)

meta$richness.over1 = apply(counts, countRichnessThreshold, MARGIN=1, threshold=1)[row.names(meta)]
plotRich(meta, column="richness.over1", saveToDir=NA)

meta$richness.over2 = apply(counts, countRichnessThreshold, MARGIN=1, threshold=2)[row.names(meta)]
plotRich(meta, column="richness.over2", saveToDir=NA)

meta$richness.over4 = apply(counts, countRichnessThreshold, MARGIN=1, threshold=4)[row.names(meta)]
plotRich(meta, column="richness.over4", saveToDir=NA)

meta$richness.over8 = apply(counts, countRichnessThreshold, MARGIN=1, threshold=8)[row.names(meta)]
plotRich(meta, column="richness.over8", saveToDir=NA)

meta$richness.over20 = apply(counts, countRichnessThreshold, MARGIN=1, threshold=20)[row.names(meta)]
plotRich(meta, column="richness.over20", saveToDir=NA)

#' 
#' With this modification to the approach, the significance between the T1 and T2 group get a little iffy. That test was sitting on the borderline to begin with.  But the idea that richness is less in T2 than HC is pretty solid.
#' 
#' Counting everything that has even a single count is sensitive to noise. Lets raise the bar a bit. Using a cuttoff 2 or 10 is more restrictive on a low-seq-depth sample than a deeper one.  So raise the bar based on the distribution of counts within each sample.
#' 
## -------------------------------------------------------------------------------------------------------------------------
countRichness <- function(sampleCounts, probs=0){
  if (probs == 0 ) threshold = 0
  else{
    threshold = quantile(sampleCounts[sampleCounts > 0], probs=probs)
  }
  # message("threshold: ", threshold)
  return(suppressWarnings(sum(sampleCounts > threshold)))
}

meta$richness.raw0 = apply(counts, countRichness, MARGIN=1, probs=0)[row.names(meta)]
plotRich(meta, column="richness.raw0", saveToDir=NA)

meta$richness.raw01 = apply(counts, countRichness, MARGIN=1, probs=0.01)[row.names(meta)]
plotRich(meta, column="richness.raw01", saveToDir=NA)

meta$richness.raw05 = apply(counts, countRichness, MARGIN=1, probs=0.05)[row.names(meta)]
plotRich(meta, column="richness.raw05", saveToDir=NA)

meta$richness.raw10 = apply(counts, countRichness, MARGIN=1, probs=0.10)[row.names(meta)]
plotRich(meta, column="richness.raw10", saveToDir=NA)

meta$richness.raw50 = apply(counts, countRichness, MARGIN=1, probs=0.50)[row.names(meta)]
plotRich(meta, column="richness.raw50", saveToDir=NA)

#' 
#' This alteration to the approach does not change our story.
#' 
#' ### Split Richness by site
#' 
#' Richness by site
#' 
## -------------------------------------------------------------------------------------------------------------------------
plotRich(meta %>% filter(LOCATION=="ACUTE"), column="richness.raw", saveToDir=NA) + ggtitle("ACUTE")
plotRich(meta %>% filter(LOCATION=="CEED"), column="richness.raw", saveToDir=NA) + ggtitle("CEED")
plotRich(meta %>% filter(LOCATION=="FARGO"), column="richness.raw", saveToDir=NA) + ggtitle("FARGO")

#' 
## -------------------------------------------------------------------------------------------------------------------------
plotRich(meta %>% filter(LOCATION=="ACUTE" | TIMEPOINT=="HC"), column="richness.raw", saveToDir=NA) + ggtitle("ACUTE")
plotRich(meta %>% filter(LOCATION=="CEED" | TIMEPOINT=="HC"), column="richness.raw", saveToDir=NA) + ggtitle("CEED")
plotRich(meta %>% filter(LOCATION=="FARGO" | TIMEPOINT=="HC"), column="richness.raw", saveToDir=NA) + ggtitle("FARGO")

#' 
#' Using all controls together, or using site-specific controls, the following is true:
#' 
#'  * At ACUTE, HC and T1 are not significantly different from each other, but T2 is less than either HC or T1.
#'  * At CEED, there is no significant difference between HC, T1 and T2.
#'  * At Fargo, there is no significant difference between HC, T1 and T2.
#' 
#' 
#' Save a pdf of these figures with matched site next to each other.
## -------------------------------------------------------------------------------------------------------------------------
pdf(file=file.path(outDir, "richness-by-site_joined-controls.pdf"))

plotRich(meta %>% filter(LOCATION=="ACUTE"), column="richness.raw", saveToDir=NA) + 
    ggtitle("ACUTE")
plotRich(meta %>% filter(LOCATION=="ACUTE" | TIMEPOINT=="HC"), column="richness.raw", saveToDir=NA) + 
    ggtitle("ACUTE - with all controls")

plotRich(meta %>% filter(LOCATION=="CEED"), column="richness.raw", saveToDir=NA) + 
    ggtitle("CEED")
plotRich(meta %>% filter(LOCATION=="CEED" | TIMEPOINT=="HC"), column="richness.raw", saveToDir=NA) + 
    ggtitle("CEED - with all controls")

plotRich(meta %>% filter(LOCATION=="FARGO"), column="richness.raw", saveToDir=NA) + 
    ggtitle("FARGO")
plotRich(meta %>% filter(LOCATION=="FARGO" | TIMEPOINT=="HC"), column="richness.raw", saveToDir=NA) + 
    ggtitle("FARGO - with all controls")

dev.off()

#' 
#' 
#' 
#' 
#' 
#' 
#' ## Sequencing Depth
#' 
#' What is the impact of sequencing depth on these metrics?
#' 
#' Calculate the seq depth in thousand-read counts.
## -------------------------------------------------------------------------------------------------------------------------
depths = rowSums(counts) 
meta$seqDepthK = depths[meta$DNA.ID] / 1000

#' 
#' How does that impact the richness.raw?
## -------------------------------------------------------------------------------------------------------------------------
depthPlot = ggplot(meta, aes(x=seqDepthK, y=richness.raw, col=LOCATION)) +
  geom_point() +
  scale_colour_manual(values = myColorPalette)
depthPlot
depthPlot + facet_wrap("LOCATION")
depthPlot + facet_grid(LOCATION ~ TIMEPOINT)

#' 
#' How does that affect diversity?
## -------------------------------------------------------------------------------------------------------------------------
depthPlot = ggplot(meta, aes(x=seqDepthK, y=alpha.div.raw, col=LOCATION)) +
  geom_point() +
  scale_colour_manual(values = myColorPalette)
depthPlot
depthPlot + facet_wrap("LOCATION")
depthPlot + facet_grid(LOCATION ~ TIMEPOINT)

#' 
#' Does a linear model show that sequencing depth impacts alpha diversity?
## -------------------------------------------------------------------------------------------------------------------------
# diversity.lm = lm(data=meta, formula = alpha.div.raw ~ seqDepthK + TIMEPOINT + LOCATION )
# summary(diversity.lm)
# diversity.lm = lm(data=meta, formula = alpha.div.raw ~ seqDepthK + TIMEPOINT )
# summary(diversity.lm)
# diversity.lm = lm(data=meta, formula = alpha.div.raw ~ seqDepthK )
# summary(diversity.lm)
diversity.lm = lm(data=meta, formula = alpha.div.raw ~ seqDepthK + LOCATION * TIMEPOINT)
summary(diversity.lm)

#' 
#' No.  It looks like location does impact diversity, and timepoint does.  But not sequencing depth.  So there's not much justification for rarefying the data to calculate the diversity.
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------
# # Well, I only think that the very low sequencing depth cases are really influenced by read depth.
# 
# diversity.lm = lm(data=meta[meta$seqDepthK < 25,], formula = alpha.div.raw ~ seqDepthK + TIMEPOINT + LOCATION )
# summary(diversity.lm)
# diversity.lm = lm(data=meta[meta$seqDepthK < 25,], formula = alpha.div.raw ~ seqDepthK + TIMEPOINT )
# summary(diversity.lm)
# diversity.lm = lm(data=meta[meta$seqDepthK < 25,], formula = alpha.div.raw ~ seqDepthK )
# summary(diversity.lm)
# diversity.lm = lm(data=meta[meta$seqDepthK < 25,], formula = alpha.div.raw ~ seqDepthK + LOCATION * TIMEPOINT)
# summary(diversity.lm)
# 
# # Still no.  

#' 
#' How does that affect richness?
## -------------------------------------------------------------------------------------------------------------------------
depthPlot = ggplot(meta, aes(x=seqDepthK, y=richness.raw, col=LOCATION)) +
  geom_point() +
  scale_colour_manual(values = myColorPalette)
depthPlot
depthPlot + facet_wrap("LOCATION")
depthPlot + facet_grid(LOCATION ~ TIMEPOINT)

#' 
#' ## save results
#' 
## -------------------------------------------------------------------------------------------------------------------------
dataToSave = data.frame(DNA.ID = meta$DNA.ID,
                        shannon.diversity = meta$alpha.div.raw,
                        richness = meta$richness.raw)

filename = file.path(outDir, paste0("diversity-and-richness_", taxaLevel, ".txt"))
write.table(x=dataToSave, file=filename, sep="\t", quote=F, row.names = F)
message("Saved file to: ", filename)

#' 
#' # Conclussions
#' 
#' At Acute only, T2 samples have significantly less shannon diversity and richness compared to the T1 samples from that site, the HC samples from that site, or all HC's from all site.
#' There is no significant difference in richness at either of the other sites. For diversity at fargo, there is a modest signal for both T1 and T2 to be greater than the diversity in the HC group, but there is no significant difference at CEED.  The difference at Farger is significant even when using all site's controls, but the number of T1 and T2 samples is still very small. 
#' 
#' I am inclined to think that there is not any difference in the richness or diversity between HC and T1 patients at any site, and that ACUTE has a site-specific affect that decreases both richness and diversity. 
#' 
## -------------------------------------------------------------------------------------------------------------------------
plotDiv(meta, column="alpha.div.raw", saveToDir=NA)
plotDiv(meta %>% filter(LOCATION=="ACUTE"), column="alpha.div.raw", saveToDir=NA) + ggtitle("ACUTE")
plotDiv(meta %>% filter(LOCATION=="CEED"), column="alpha.div.raw", saveToDir=NA) + ggtitle("CEED")
plotDiv(meta %>% filter(LOCATION=="FARGO"), column="alpha.div.raw", saveToDir=NA) + ggtitle("FARGO")

plotRich(meta, column="richness.raw", saveToDir=NA)
plotRich(meta %>% filter(LOCATION=="ACUTE"), column="richness.raw", saveToDir=NA) + ggtitle("ACUTE")
plotRich(meta %>% filter(LOCATION=="CEED"), column="richness.raw", saveToDir=NA) + ggtitle("CEED")
plotRich(meta %>% filter(LOCATION=="FARGO"), column="richness.raw", saveToDir=NA) + ggtitle("FARGO")

#' 
## -------------------------------------------------------------------------------------------------------------------------
sessionInfo()
sizeByP
plotRichness
plotDiversity

#' 
