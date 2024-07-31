#' ---
#' title: "Diversity-interactions"
#' output: html_document
#' date: "2024-01-05"
#' ---
#' 
## ----include=FALSE-------------------------------------------------------------------------------------
# I prefer to edit the Rmd file and then use purl to convert into a R script.
# knitr::purl(input=dir(pattern="Rmd"), documentation = 2L)
knitr::opts_chunk$set(echo = TRUE)

#' 
#' 
#' # Setup
#' 
#' ### libraries
#' 
## ----results = "hold"----------------------------------------------------------------------------------
tellme <- function(name){print(paste0("Package ", name, " version: ", packageVersion(name)))}

library(tidyr); tellme("tidyr")
suppressPackageStartupMessages(library(dplyr)); tellme("dplyr")
library(ggplot2); tellme("ggplot2")
library(ggrepel); tellme("ggrepel")

#' ### Colors
#' 
#' A named vector of color vectors.  Most of these match the previous paper. 
## ----fig.height=4, fig.width=2, echo=FALSE, include=FALSE----------------------------------------------
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
#' Pick a taxonomic level. Take an argument if one is given.
## ------------------------------------------------------------------------------------------------------
args = commandArgs(trailingOnly=TRUE)
if (length(args) > 0){
    taxaLevel = args[1]
}else{
    taxaLevel = "species"
}

#' 
#' Direct output
## ------------------------------------------------------------------------------------------------------
outDir = file.path("..", "output", taxaLevel)
suppressWarnings( dir.create(outDir, recursive = T) )

#' 
#' Output will be saved to: ``r outDir``
#' 
#' # Main
#' 
#' ## Read meta data.
#' 
#' Read the original metadata file.
## ------------------------------------------------------------------------------------------------------
meta = read.delim("../../input/meta/ANIGMA-metadata.txt") %>%
  select(PARTICIPANT.ID, LOCATION, TIMEPOINT, AGE, SUBTYPE, BMI, 
         STAI_Y1, STAI_Y2, STAI_TOTAL, PSS, DAYS_TREAT, Weight_kg, DNA.ID, DUR_ILLNESS_YRS) 
dim(meta)

#' 
#' Check metadata.
## ------------------------------------------------------------------------------------------------------
if (any(!is.na(meta$DAYS_TREAT[meta$TIMEPOINT=="HC"]))){
    message("For any Healthy Control participant, the DAYS_TREAT should be NA (not 0).")
    meta$DAYS_TREAT[meta$TIMEPOINT=="HC"] = NA
}else{
    message("Ah, good, for Healthy Control participants, the DAYS_TREAT is NA (not 0).")
}

#' 
#' ## Read diversity data
#' 
#' 
## ------------------------------------------------------------------------------------------------------
pipeDir="../.."
inputMod = dir(pipeDir, pattern="Diversity_Metrics", full.names = T)

inputFileNames = c(
    domain="diversity-and-richness_domain.txt",
    phylum="diversity-and-richness_phylum.txt",
    class="diversity-and-richness_class.txt",
    order="diversity-and-richness_order.txt",
    family="diversity-and-richness_family.txt",
    genus="diversity-and-richness_genus.txt",
    species="diversity-and-richness_species.txt")

inputFile=file.path(inputMod, "output", taxaLevel, inputFileNames[taxaLevel])

#' 
#' Read file ``r inputFile``.
#' 
## ------------------------------------------------------------------------------------------------------
divdata = read.delim2(inputFile) %>% select(DNA.ID, shannon.diversity, richness)
divdata$shannon.diversity = as.numeric(divdata$shannon.diversity)
dim(divdata)

#' 
#' ## Feature engineering
#' 
#' For subtype, we are only interested in the AN cases. HC is not really an AN subtype. And we need to split samples between T1 and T2 so we are not double-dipping with each paired sample.
## ------------------------------------------------------------------------------------------------------
meta$SUBTYPE.AN = meta$SUBTYPE
meta$SUBTYPE.AN[meta$SUBTYPE.AN=="HC"] = NA

#' 
#' The anorexia signal is what we would see by comparing T1 to HC or T2 to HC, which is the "TIMEPOINT" variable. Lets call it the ANOREXIA variable so that its clearer in the summaries.
## ------------------------------------------------------------------------------------------------------
meta$ANOREXIA = meta$TIMEPOINT

#' 
#' 
#' ## Previous feature engineering
#' 
#' Read the diffs metadata - data where diffs have been calculated.
## ------------------------------------------------------------------------------------------------------
prevModule = dir(path="../..", pattern="Participant_Metadata", full.names = T)
diffsFile = file.path(prevModule, "output", "ANIGMA-metadata_by_AN_participant.txt")
diffs = read.delim(diffsFile)
row.names(diffs) = diffs$PARTICIPANT.ID
dim(diffs)

#' 
#' Read in modified metadata from: ``r diffsFile``
#' 
#' This table has ``r nrow(diffs)`` rows and ``r ncol(diffs)`` columns.
#' 
#' Match this data withe corresponding DNA ID for T1 and T2.
## ------------------------------------------------------------------------------------------------------
diffKey = meta %>% 
    select(PARTICIPANT.ID, TIMEPOINT, DNA.ID) %>%
    pivot_wider(id_cols = PARTICIPANT.ID, names_from = TIMEPOINT, values_from = DNA.ID, names_prefix = "DNA.ID.") %>%
    select(-DNA.ID.HC)
diffs = merge(diffs, diffKey, all.x=T, by="PARTICIPANT.ID")

#' 
#' 
#' ## merge
#' 
#' Merge meta and data.
## ------------------------------------------------------------------------------------------------------
data = merge(divdata, meta, by="DNA.ID")

#' 
#' 
## ----include=FALSE-------------------------------------------------------------------------------------
# # Make a scrambled version. We can use this to verify that we see null results, and our tests are not inherently flawed.
# scram.counts = counts.norm
# row.names(scram.counts) = sample(row.names(scram.counts), size=nrow(scram.counts), replace = FALSE)
# scrambled.data = merge(scram.counts, meta, by.x=0, by.y="DNA.ID")
# rm(scram.counts)

#' 
#' Merge the T1 diversity data to the per-participant differences data. And T2.
## ------------------------------------------------------------------------------------------------------
diffData1 = merge(divdata, diffs, by.x="DNA.ID", by.y="DNA.ID.T1")
diffData2 = merge(divdata, diffs, by.x="DNA.ID", by.y="DNA.ID.T2")

#' 
#' 
#' Make sure Patient ID is not treated as a numerical value.
## ------------------------------------------------------------------------------------------------------
data$PARTICIPANT.ID = as.character(data$PARTICIPANT.ID)
# scrambled.data$PARTICIPANT.ID = as.character(scrambled.data$PARTICIPANT.ID)

#' 
#' # Test
#' 
#' ## test types
#' 
#' Group the metadata variables based on how they should be tested.
#' 
#' We have our categorical variables (two or more categories). These can be split in to variables that should be handled across time (T1 and T2 samples are both included in the same test) or by time (the test should be exclude one or the other time point so all samples in the test are independent). 
#' 
#' T1 and T2, but not HC.
## ------------------------------------------------------------------------------------------------------
# variables_acrossTime = c("PARTICIPANT.ID", "TIMEPOINT")
variables_acrossTime = c("TIMEPOINT")
variables_acrossTime

#' Handle by time: T1 (HC and T1) or T2 (HC and T2)
## ------------------------------------------------------------------------------------------------------
variables_byTime = c("LOCATION", "ANOREXIA", "SUBTYPE.AN")
variables_byTime

#' 
#' 
#' A subset of those are binary, and we can use the t-test.
## ------------------------------------------------------------------------------------------------------
binary = c("TIMEPOINT.AN")
# HC vs T1
# HC vs T2
binary

#' 
#' Continuous variables come in two forms: 
#' 
#'  * constants - measurements that are assumed to be the same for a given person at T1 and T2, such as age.
#'  * diffables - measurements where the difference could be calculated and is potentially meaningful. These are metrics that:
#'  
#'    - could be tested at either time point, and 
#'    - should not be tested with both time points simultaneously because there are many pairs of non-independent samples, and 
#'    - we could test difference against taxa_metrics in addition to testing the T1 and the T2 values.
#'  
## ------------------------------------------------------------------------------------------------------
constants = c("AGE", "DUR_ILLNESS_YRS", "DAYS_TREAT")

#'  
## ------------------------------------------------------------------------------------------------------
diffables =c("Weight_kg", "BMI", "STAI_Y1", "STAI_Y2", "STAI_TOTAL", "PSS")
diffables

#' 
#' We have previously calculated differences, and a couple other per-patient numerical features: T1.severity and BMI.gain.per.day.
## ------------------------------------------------------------------------------------------------------
# deltas = names(diffs) %>% grep(pattern="diff", value = T)
deltas = c("T1.severity", "Weight_kg.diff", "BMI.diff", "BMI.gain.per.day", "STAI_Y1.diff", "STAI_Y2.diff", "STAI_TOTAL.diff", "PSS.diff")
deltas

#' 
#' taxa_metrics will be the variable name to refer to 
## ------------------------------------------------------------------------------------------------------
taxa_metrics = c("shannon.diversity", "richness")

#' 
#' We have ``r length(taxa_metrics)`` to test.
#' 
#' ## Categorical
#' 
#' Compare the values for each taxon against each categorical feature. Use the 1-way-anova and the Kruskal test.
#' 
## ----TaxaVsCategoryTest--------------------------------------------------------------------------------
displayTaxonName <- function(string, string2){
  return(string)
}


TaxaVsCategoryTest <- function(dataDF, taxa_metrics, variables, doPlot=TRUE, redPval=0.05, testType="anova", fileNameBase=NULL, minGroupSize=2, pairID="PARTICIPANT.ID"){
    # testType - one of "anova" or "kruskal.test"
    # taxa_metrics - a subset of columns representing taxonomic features
    # variables - a different subset of columns

    # check args
    if (length(setdiff(taxa_metrics, names(dataDF))) > 0) {
        stop("Argument 'taxa_metrics' should be a subset of the column names of 'dataDF'.")
    }
    if (length(setdiff(variables, names(dataDF))) > 0) {
        stop("Argument 'variables' should be a subset of the column names of 'dataDF'.")
    }

    pvalTable = data.frame(matrix(data=NA, nrow=length(taxa_metrics), ncol=length(variables)))
    names(pvalTable) = variables
    row.names(pvalTable) = taxa_metrics

    # adjusted pvalues have the same layout
    adjPvalTable = pvalTable

    message("p-values will be calculated based on the ", testType, " test.")
    saveToFile = !is.null(fileNameBase)

    pvalCountTable = data.frame(variable="TOTAL",
                                testType=testType,
                                nVals=NA,
                                levels=NA,
                                testWithPvalue=NA,
                                rawPvalSig=NA)

    for (variable in variables){
        variablePlotList = list()
        variablePvals = c()

        xAxis = dataDF[, variable]

        # if any category only has one sample (or fewer than minGroupSize), omit that category.
        tooFew = names(which(table(xAxis) < minGroupSize))
        if (length(tooFew) > 0){
            xAxis[xAxis %in% tooFew] = NA
            message("Omitted groups with fewer than [", minGroupSize, "] samples: ", paste0(tooFew, collapse=", "))
        }

        for (taxon in taxa_metrics){
            taxonVals = dataDF[,taxon]
            # message("Testing variable ", variable, " against taxon ", taxon, "...")

            if (testType == "anova"){
                pval = anova(lm(taxonVals ~ xAxis))$`Pr(>F)`[1]
            }
            if (testType == "kruskal.test"){
                pval = kruskal.test( taxonVals ~ xAxis, na.action=na.exclude )$p.value
            }
            if (testType == "paired" | testType == "wilcox"){
                pairedDF = data.frame(taxonVals = dataDF[, taxon],
                                      xAxis = dataDF[, variable],
                                      id = dataDF[, pairID]) %>%
                    pivot_wider(id_cols=id, names_from = xAxis, values_from = taxonVals) %>%
                    filter(complete.cases(.)) %>%
                    data.frame()
                # if (no0) pairedDF = pairedDF %>% filter(T1 > 0 | T2 > 0)
                categories = unique(dataDF[,variable])
                if(nrow(pairedDF) >= minGroupSize){
                    if (testType == "paired"){
                        pval = t.test(x=pairedDF[, categories[1]], 
                                      y=pairedDF[, categories[2]], paired = TRUE )$p.value
                    }
                    if (testType == "wilcox"){
                        pval = wilcox.test(x=pairedDF[, categories[1]], 
                                           y=pairedDF[, categories[2]], paired = TRUE )$p.value
                        # pval = coin::wilcox_test(x=pairedDF[, categories[1]], 
                        #               y=pairedDF[, categories[2]], paired = TRUE )$p.value
                    }
                }else{
                    pval = NA
                }
                
            }

            pvalTable[taxon, variable] = pval
            variablePvals[taxon] = pval

            if (doPlot){
                plotdf = data.frame(x=xAxis, y=dataDF[,taxon], TIMEPOINT=dataDF$TIMEPOINT)
                gplot = ggplot(data=plotdf, aes(x=x, y=y, col=x)) +
                    xlab(variable) +
                    ylab(displayTaxonName(taxon, taxaLevel)) +
                    geom_boxplot(outlier.alpha = 0) +
                    # geom_point() +
                    geom_jitter(position=position_jitterdodge(jitter.width = .09, jitter.height = 0), size=.5) +
                    scale_colour_manual(variable, values = myColorPalette) +
                    ggtitle( paste0(testType, " p-value: ", signif(pval, 5)) ) +
                    theme(plot.title = element_text(colour = ifelse(pval<=redPval, "red", "black")))
                variablePlotList[[taxon]] = gplot
            }
        }# taxon for loop


        # adjustedPvals
        padj.method="fdr"
        adjustedPvals = p.adjust(variablePvals, method = padj.method)
        adjPvalTable[,variable] = adjustedPvals

        # add row to summary table
        pvalCountTable = rbind(pvalCountTable,
                               c(
                                   variable=variable,
                                   testType=testType,
                                   nVals=sum(!is.na(xAxis)),
                                   levels=paste0(unique(xAxis), collapse=","),
                                   testWithPvalue=sum(!is.na(variablePvals)),
                                   rawPvalSig=sum(variablePvals <= redPval, na.rm=T)
                               )
        )


        if (doPlot){
            ord = order(variablePvals)
            if(saveToFile) {
                pdfname = paste0(fileNameBase, "_plots_", variable, ".pdf")
                pdf(pdfname)
                message("Saving file: ", pdfname)
            }
            for (i in ord){
                annotColor = ifelse(adjustedPvals[i] <= redPval, "red", "black")
                annotText = paste0(padj.method, "-adjusted p-value: ", signif(adjustedPvals[i], 5))
                show(
                    variablePlotList[[i]] +
                        annotate(geom="text", label=annotText, col=annotColor,
                                 x=Inf, y=Inf, hjust="inward", vjust="inward")
                )
            }
            if(saveToFile) {
                dev.off()
            }
        }

    }# variable for-loop

    if (saveToFile){
        # save summary table
        table.file = paste0(fileNameBase, "_summary.txt")
        message("Saving file: ", table.file)
        write.table(file=table.file, pvalCountTable, sep="\t", quote=F, row.names = F)
    }

    return(pvalTable)
}

#' 
## ----runTaxaVsCategoryTest-----------------------------------------------------------------------------
# parametric test - anova
acrossTimeTable = TaxaVsCategoryTest(data %>% filter(TIMEPOINT != "HC"),
                                     taxa_metrics, variables_acrossTime,
                                     fileNameBase=file.path(outDir, "T1T2.anova"))
acrossTimeTable.acute = TaxaVsCategoryTest(data %>% filter(TIMEPOINT != "HC" & LOCATION=="ACUTE"),
                                     taxa_metrics, variables_acrossTime,
                                     fileNameBase=file.path(outDir, "T1T2.ACUTE.anova"))
acrossTimeTable.ceed = TaxaVsCategoryTest(data %>% filter(TIMEPOINT != "HC" & LOCATION=="CEED"),
                                     taxa_metrics, variables_acrossTime,
                                     fileNameBase=file.path(outDir, "T1T2.CEED.anova"))
acrossTimeTable.fargo = TaxaVsCategoryTest(data %>% filter(TIMEPOINT != "HC" & LOCATION=="CEED"),
                                     taxa_metrics, variables_acrossTime,
                                     fileNameBase=file.path(outDir, "T1T2.FARGO.anova"))

# T1T2 paired
acrossTimePairedTable = TaxaVsCategoryTest(data %>% filter(TIMEPOINT != "HC"), 
                                     taxa_metrics, "TIMEPOINT", testType = "paired",
                                     fileNameBase=file.path(outDir, "T1T2.pairedT.all"))
acrossTimePairedTable.acute = TaxaVsCategoryTest(data %>% filter(TIMEPOINT != "HC") %>% filter(LOCATION != "ACUTE"), 
                                     taxa_metrics, "TIMEPOINT", testType = "paired", 
                                     fileNameBase=file.path(outDir, "T1T2.pairedT.acute"))
acrossTimePairedTable.ceed = TaxaVsCategoryTest(data %>% filter(TIMEPOINT != "HC") %>% filter(LOCATION != "CEED"), 
                                     taxa_metrics, "TIMEPOINT", testType = "paired", 
                                     fileNameBase=file.path(outDir, "T1T2.pairedT.ceed"))
acrossTimePairedTable.fargo = TaxaVsCategoryTest(data %>% filter(TIMEPOINT != "HC") %>% filter(LOCATION != "FARGO"), 
                                     taxa_metrics, "TIMEPOINT", testType = "paired", 
                                     fileNameBase=file.path(outDir, "T1T2.pairedT.fargo"))

# randomized.acrossTimeTable = TaxaVsCategoryTest(scrambled.data, taxa_metrics, variables_acrossTime, doPlot=F)

anovaAtT1 = TaxaVsCategoryTest(data %>% filter(TIMEPOINT != "T2"),
                               taxa_metrics, variables_byTime, minGroupSize = 5,
                               fileNameBase=file.path(outDir, "HCT1.anova"))

anovaAtT2 = TaxaVsCategoryTest(data %>% filter(TIMEPOINT != "T1"),
                               taxa_metrics, variables_byTime, minGroupSize = 5,
                               fileNameBase=file.path(outDir, "HCT2.anova"))

anovaAtHC = TaxaVsCategoryTest(data %>% filter(TIMEPOINT == "HC"),
                               taxa_metrics, "LOCATION",
                               fileNameBase=file.path(outDir, "HC.anova"))

# nonparametric test
kwAcrossTime = TaxaVsCategoryTest(data %>% filter(TIMEPOINT != "HC"),
                                  taxa_metrics, variables_acrossTime, doPlot=F, testType = "kruskal.test")
# randomized.kwAcrossTime = TaxaVsCategoryTest(scrambled.data, taxa_metrics, variables_acrossTime, doPlot=F, testType = "kruskal.test")
kwAtT1 = TaxaVsCategoryTest(data %>% filter(TIMEPOINT != "T2"),
                            taxa_metrics, variables_byTime, doPlot=F, testType = "kruskal.test")
kwAtT2 = TaxaVsCategoryTest(data %>% filter(TIMEPOINT != "T1"),
                            taxa_metrics, variables_byTime, doPlot=F, testType = "kruskal.test")
kwAtHC = TaxaVsCategoryTest(data %>% filter(TIMEPOINT == "HC"),
                            taxa_metrics, "LOCATION", doPlot=F, testType = "kruskal.test")

#' 
#' 
#' ## binary tests - t-test
#' 
#' This is basically the same as the anova test with two categories, so we my not bother.
#' 
#' ## Numeric feature tests
#' 
#' Compare the values for each taxon against each numeric feature.
#' 
## ----TaxaVsNumericTest---------------------------------------------------------------------------------
TaxaVsNumericTest <- function(dataDF, taxa_metrics, variables, doPlot=TRUE, redPval=0.05, testType="pearson", fileNameBase=NULL){
    # testType - one of "pearson" or "kendall"
    # taxa_metrics - a subset of columns representing taxonomic features
    # variables - a different subset of columns
    
    # check args
    if (length(setdiff(taxa_metrics, names(dataDF))) > 0) {
        stop("Argument 'taxa_metrics' should be a subset of the column names of 'dataDF'.")
    }
    if (length(setdiff(variables, names(dataDF))) > 0) {
        stop("Argument 'variables' should be a subset of the column names of 'dataDF'.")
    }
    
    pvalTable = data.frame(matrix(data=NA, nrow=length(taxa_metrics), ncol=length(variables)))
    names(pvalTable) = variables
    row.names(pvalTable) = taxa_metrics
    
    # adjusted pvalues have the same layout
    adjPvalTable = pvalTable
    
    message("p-values will be calculated based on the ", testType, " test.")
    saveToFile = !is.null(fileNameBase)
    
    pvalCountTable = data.frame(variable="TOTAL",
                                testType=testType,
                                nVals=NA,
                                levels=NA,
                                testWithPvalue=NA,
                                rawPvalSig=NA)
    
    for (variable in variables){
        variablePlotList = list()
        variablePvals = c()
        
        xAxis = dataDF[, variable]
        
        for (taxon in taxa_metrics){
            taxonVals = dataDF[,taxon]
            # message("Testing variable ", variable, " against taxon ", taxon, "...")
            xAxis = dataDF[, variable]
            if (testType == "pearson"){
                pval = cor.test( taxonVals, xAxis, na.action=na.exclude, method="pearson")$p.value
            }
            if (testType == "kendall"){
                pval = cor.test( taxonVals, xAxis, na.action=na.exclude, method="kendall")$p.value
            }
            
            pvalTable[taxon, variable] = pval
            variablePvals[taxon] = pval
            
            if (doPlot){
                plotdf = data.frame(x=dataDF[,variable], y=dataDF[,taxon], LOCATION=dataDF$LOCATION)
                gplot = ggplot(data=plotdf, aes(x=x, y=y, col=LOCATION)) +
                    xlab(variable) +
                    ylab(displayTaxonName(taxon, taxaLevel)) +
                    geom_point() + 
                    scale_colour_manual(variable, values = myColorPalette) +
                    ggtitle( paste0("p-value: ", signif(pval, 5)) ) +
                    theme(plot.title = element_text(colour = ifelse(pval<=redPval, "red", "black"))) 
                variablePlotList[[taxon]] = gplot
            }
        }# taxon for loop
        
        
        # adjustedPvals
        padj.method="fdr"
        adjustedPvals = p.adjust(variablePvals, method = padj.method)
        adjPvalTable[,variable] = adjustedPvals
        
        # add row to summary table
        pvalCountTable = rbind(pvalCountTable, 
                               c(
                                   variable=variable,
                                   testType=testType,
                                   nVals=sum(!is.na(xAxis)),
                                   levels=paste0("min: ", min(xAxis, na.rm=T), "; mean: ", mean(xAxis, na.rm=T), "; median: ", median(xAxis, na.rm=T), "; max: ", max(xAxis, na.rm=T)),
                                   testWithPvalue=sum(!is.na(variablePvals)),
                                   rawPvalSig=sum(variablePvals <= redPval, na.rm=T)
                               ) 
        )
        
        
        if (doPlot){
            ord = order(variablePvals)
            if(saveToFile) {
                pdfname = paste0(fileNameBase, "_plots_", variable, ".pdf")
                pdf(pdfname)
                message("Saving file: ", pdfname)
            }
            for (i in ord){
                annotColor = ifelse(adjustedPvals[i] <= redPval, "red", "black")
                annotText = paste0(padj.method, "-adjusted p-value: ", signif(adjustedPvals[i], 5))
                show(
                    variablePlotList[[i]] +
                        annotate(geom="text", label=annotText, col=annotColor,
                                 x=Inf, y=Inf, hjust="inward", vjust="inward")
                )
            }
            if(saveToFile) {
                dev.off()
            }
        }
        
    }# variable for-loop
    
    if (saveToFile){
        # save summary table
        table.file = paste0(fileNameBase, "_summary.txt")
        message("Saving file: ", table.file)
        write.table(file=table.file, pvalCountTable, sep="\t", quote=F, row.names = F)
    }
    
    return(pvalTable)
}

#' 
## ----runTaxaVsNumericTest------------------------------------------------------------------------------
suppressWarnings({
    suppressMessages({
        peAtT1 = TaxaVsNumericTest(data %>% filter(TIMEPOINT == "T1"), 
                                   taxa_metrics, diffables, doPlot=T, testType = "pearson",
                                   fileNameBase = file.path(outDir, "T1.pearson"))
        keAtT1 = TaxaVsNumericTest(data %>% filter(TIMEPOINT == "T1"), 
                                   taxa_metrics, diffables, doPlot=F, testType = "kendall")
        peAtT2 = TaxaVsNumericTest(data %>% filter(TIMEPOINT == "T2"), 
                                   taxa_metrics, diffables, doPlot=F, testType = "pearson")
        keAtT2 = TaxaVsNumericTest(data %>% filter(TIMEPOINT == "T2"), 
                                   taxa_metrics, diffables, doPlot=F, testType = "kendall")
        peAtDif1 = TaxaVsNumericTest(diffData1, taxa_metrics, deltas, doPlot=F, testType = "pearson")
        keAtDif1 = TaxaVsNumericTest(diffData1, taxa_metrics, deltas, doPlot=F, testType = "kendall")
        peAtDif2 = TaxaVsNumericTest(diffData2, taxa_metrics, deltas, doPlot=F, testType = "pearson")
        keAtDif2 = TaxaVsNumericTest(diffData2, taxa_metrics, deltas, doPlot=F, testType = "kendall")
    })
})

#' 
#' # Test Summaries
#' 
#' ### Combine test results
#' 
#' Merge to make one large heatmap.
#' 
#' Merge the categorical values.
## ------------------------------------------------------------------------------------------------------
appendName <- function(df, suffix){
    names(df) = paste0(names(df), suffix)
    return(df)
}
mergeList = list(
    anovaAtT1 = appendName(anovaAtT1, ".HCT1.anova"),
    anovaAtT2 = appendName(anovaAtT2, ".HCT2.anova"),
    anovaAtHC = appendName(anovaAtHC, ".HC.anova"),
    acrossTimeTable = appendName(acrossTimeTable, ".T1T2.anova"),
    acrossTimeTable.acute = appendName(acrossTimeTable.acute, ".T1T2.Acute.anova"),
    acrossTimeTable.ceed = appendName(acrossTimeTable.ceed, ".T1T2.Ceed.anova"),
    acrossTimeTable.fargo = appendName(acrossTimeTable.fargo, ".T1T2.Fargo.anova"),
    
    acrossTimePairedTable = appendName(acrossTimePairedTable, ".T1T2.pairedT"),
    acrossTimePairedTable.acute = appendName(acrossTimePairedTable.acute, ".T1T2.pairedT.acute"),
    acrossTimePairedTable.ceed = appendName(acrossTimePairedTable.ceed, ".T1T2.pairedT.ceed"),
    acrossTimePairedTable.fargo = appendName(acrossTimePairedTable.fargo, ".T1T2.pairedT.fargo"),
    
    kwAcrossTime = appendName(kwAcrossTime, ".T1T2.Kruskal"),
    kwAtT1 = appendName(kwAtT1, ".HCT1.Kruskal"),
    kwAtT2 = appendName(kwAtT2, ".HCT2.Kruskal"),
    kwAtHC = appendName(kwAtHC, ".HC.Kruskal")
    )

# merge
m.all.1 = data.frame(Row.names=taxa_metrics)
for (df in mergeList){
    m.all.1 = merge(m.all.1, df, by.x="Row.names", by.y=0)
}

# move row names
row.names(m.all.1) = m.all.1$Row.names
m.all.1 = m.all.1 %>% select(-Row.names)

# sort columns
m.all.1 = m.all.1[, order(names(m.all.1))]
# order groups of columns
m.all.1 = m.all.1 %>% select(starts_with("TIMEPOINT"),
                             starts_with("HCvsAN"),
                             starts_with("SUBTYPE.AN"),
                             starts_with("ANOREXIA"),
                             starts_with("LOCATION"),
                             everything())

#' 
#' 
#' Merge the numeric feature test results.
## ------------------------------------------------------------------------------------------------------
appendName <- function(df, suffix){
    names(df) = paste0(names(df), suffix)
    return(df)
}
mergeList.numeric = list(
    peAtT1 = appendName(peAtT1, ".T1.pearson"),
    keAtT1 = appendName(keAtT1, ".T1.kendall"),
    peAtT2 = appendName(peAtT2, ".T2.pearson"),
    keAtT2 = appendName(keAtT2, ".T2.kendall"),
    peAtDif1 = appendName(peAtDif1, ".T1.pearson"),
    keAtDif1 = appendName(keAtDif1, ".T1.kendall"),
    peAtDif2 = appendName(peAtDif2, ".T2.pearson"),
    keAtDif2 = appendName(keAtDif2, ".T2.kendall")
    )

# merge
m.all.2 = data.frame(Row.names=taxa_metrics)
for (df in mergeList.numeric){
    m.all.2 = merge(m.all.2, df, by.x="Row.names", by.y=0)
}

# move row names
row.names(m.all.2) = m.all.2$Row.names
m.all.2 = m.all.2 %>% select(-Row.names)

# sort columns
m.all.2 = m.all.2[, order(names(m.all.2))]

# order groups of columns
m.all.2 = m.all.2 %>% select(starts_with("Weight_kg"),
                             starts_with("BMI"),
                             starts_with("PSS"),
                             starts_with("STAI"),
                             everything())

#' 
#' 
#' Merge into a single results table
## ------------------------------------------------------------------------------------------------------
m.all = merge(m.all.1, m.all.2, by=0)

# move row names
row.names(m.all) = m.all$Row.names
m.all = m.all %>% select(-Row.names)

# orderRows = order(apply(m.all, MARGIN = 1, function(row){
#     sum(row < 0.05, na.rm=T)
#     }), decreasing = T)
orderRows = order(-log10(m.all$ANOREXIA.HCT1.anova), decreasing = T)
m.all = m.all[ orderRows, ]

#' 
#' ### Save tables
#' 
#' Adjust p-values within each column, and save that table.  Add on the full taxon names, and keep row order.
## ------------------------------------------------------------------------------------------------------
# m.all.adj = apply(m.all, MARGIN = 2, FUN=p.adjust, method="fdr")
m.all.adj = m.all
for( test in names(m.all)){
    m.all.adj[,test] = p.adjust(m.all[,test], method = "fdr")
}

# row.names(nameKey) = nameKey$shortName
# m.all.adj.labeled = cbind(taxon.originalName=nameKey[row.names(m.all.adj),"originalName"], 
#                       taxon=row.names(m.all.adj),
#                       m.all.adj)
m.all.adj.labeled = cbind(taxon=row.names(m.all.adj),
                      m.all.adj)

write.table(m.all.adj.labeled, file = file.path(outDir, paste0("bigList_pvalues_fdr-adjusted_", taxaLevel, ".txt")),
            row.names=F, quote=F, sep="\t")

#' 
#' Build a summary table for each variable tested:
#' 
#'   * How many tests have non-NA p-values?
#'   * How many raw p-values are significant?
#'   * How many are significant after adjusting for the number of taxa_metrics.
## ------------------------------------------------------------------------------------------------------
summary = data.frame(testName="TOTAL",
                     attemptedTests=NA,
                     testWithPvalue=NA,
                     rawPvalSig=NA,
                     adjPvalSig_nTaxa=NA,
                     adjPvalSig_nTests=NA)

signifP = 0.05
for (testName in names(m.all)){
    # add row to summary table
    variablePvals = m.all[,testName]
    adjustedPvals = p.adjust(variablePvals, method="fdr")
    very.adjustedPvals = p.adjust(variablePvals, method="fdr", n = nrow(m.all) * ncol(m.all))
    summary = rbind(summary, 
                           c(
                               testName=testName,
                               attemptedTests=length(variablePvals),
                               testWithPvalue=sum(!is.na(variablePvals)),
                               rawPvalSig=sum(variablePvals <= signifP, na.rm=T),
                               adjPvalSig_nTaxa=sum(adjustedPvals <= signifP, na.rm=T),
                               adjPvalSig_nTests=sum(very.adjustedPvals <= signifP, na.rm=T)
                           ) 
    )
}
summary = summary %>% filter(testName != "TOTAL")

write.table(summary, file = file.path(outDir, paste0("bigList_summary_", taxaLevel, ".txt")),
            row.names=F, quote=F, sep="\t")

#' 
#' 
#' ### pvalue heat map
#' 
#' Show the p-value table as a heat map.
## ----pvalHeat------------------------------------------------------------------------------------------
shortPathwayNames = function(longName){
  medName = gsub("[^ ]*: ", "", longName)
  isTooLong = nchar(medName) > 50
  if (isTooLong){
    trim1 = substr(medName, 0, 50)
    words = strsplit(trim1, split=" ")[[1]]
    lessWords = words[1:(length(words)-1)]
    trim2 = paste(c(lessWords, "..."), collapse = " ")
    medName = trim2
  }
  return(medName)
}

caption.about.names = "Pathway names were shortened. The pathway id was removed leaving the description.
Numbers were appened because the truncated names are not all unique.
Row order here (bottom to top) matches the row order in the corresponding p-value table."


pvalTableToHeatMap = function(pvalTable, midpointPval=0.05, bluePvalue=0.05, redPvalue=0.00001, title="-log10(p-values)", displayName.FUN=shortPathwayNames){
    # pvalTable - data frame of pvalues, with columns corresponding to variables (metadata) and named rows for taxa_metrics.
    # bluePval - pvalues worse than this will all be shown in blue.
    # midpointPval - these will be shown in white, as a middle point between the blue and red gradients.
    # all p-values better than this will be shown in solid red.
    pvalTable[pvalTable < redPvalue] = redPvalue
    pvalTable[pvalTable > bluePvalue] = bluePvalue
    
    taxon = sapply( row.names(pvalTable), displayName.FUN )
    if (length(unique(taxon)) < nrow(pvalTable)){
        taxon = paste0(taxon, "[", 1:nrow(pvalTable), "]")
    }
    row.names(pvalTable) = taxon
    
    longTab = -log10(pvalTable) %>% 
        mutate(taxon= row.names(pvalTable) ) %>%
        pivot_longer(cols=-taxon, names_to = "var", values_to="pval")
    longTab$var = factor(x=as.character(longTab$var), levels=names(pvalTable))
    longTab$taxon = factor(x=as.character(longTab$taxon), levels=row.names(pvalTable))
    ggplot(longTab, aes(x=var, y=taxon, fill=pval)) +
      geom_tile(col="gray") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = -log10(midpointPval),
                           limit = c(-log10(bluePvalue),-log10(redPvalue)),
                           space = "Lab",
                           name=paste0("p-value threshold: ", midpointPval, "\n\n-log10(p-value)"),
                           na.value = gray(.9)) +
      theme_minimal()+ # minimal theme
      ggtitle(title) +
      xlab("") +
      ylab(taxaLevel) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 8, hjust = 1)) +
      coord_fixed() +
      labs(caption=caption.about.names)
}

#' 
#' 
#' Heat-map for categorical feature p-values.
## ------------------------------------------------------------------------------------------------------
pvalHeatMap.categories.file = file.path(outDir, paste0("heatmap_categoricalFeatures_raw-p-values_", taxaLevel, ".pdf"))
pdf(file=pvalHeatMap.categories.file,
    height = 3 + nrow(m.all.1) / 6,
    width = 5 + ncol(m.all.1) / 6)
pvalTableToHeatMap(m.all.1, title="catogrical-features raw p-values")
dev.off()

#' 
#' Heat-map for numerical feature p-values.
## ------------------------------------------------------------------------------------------------------
pvalHeatMap.numeric.file = file.path(outDir, paste0("heatmap_numericFeatures_raw-p-values_", taxaLevel, ".pdf"))
pdf(file=pvalHeatMap.numeric.file,
    height = 3 + nrow(m.all.2) / 6,
    width = 5 + ncol(m.all.2) / 6)
pvalTableToHeatMap(m.all.2, title="numeric-features raw p-values")
dev.off()

#' 
#' 
#' Save a single large heatmap with all raw p-values
## ------------------------------------------------------------------------------------------------------
pvalHeatMap.big.file = file.path(outDir, paste0("bigList_heatmap_raw-p-values_", taxaLevel, ".pdf"))
pdf(file=pvalHeatMap.big.file,
    height = 3 + nrow(m.all) / 6,
    width = 5 + ncol(m.all) / 6)
pvalTableToHeatMap(m.all, title="the big heatmap - raw p-values")
dev.off()

#' 
#' Save a single large heatmap with all ajdusted p-values
## ------------------------------------------------------------------------------------------------------
pvalHeatMap.big.adj.file = file.path(outDir, paste0("bigList_heatmap_fdr.adj-p-values_", taxaLevel, ".pdf"))
pdf(file=pvalHeatMap.big.adj.file,
    height = 3 + nrow(m.all) / 6,
    width = 5 + ncol(m.all) / 6)
pvalTableToHeatMap(m.all.adj, title="the big heatmap - fdr-adjusted p-values")
dev.off()

#' 
#' ### p-value histograms
#' 
#' Draw p-value histograms for all variables.
## ------------------------------------------------------------------------------------------------------
pvalHist = function(pvalTable, title="", pdfName=title, saveToFolder=outDir, taxalevel=taxaLevel){
    longTab = cbind(taxon=row.names(pvalTable), pvalTable) %>% 
        pivot_longer(cols=-taxon, names_to = "variable", values_to = "value")
    # show(suppressMessages(
    #     ggplot(longTab, aes(x=value, fill=variable)) +
    #         geom_histogram() +
    #         ggtitle(paste0(title,"(", taxalevel, ")"))
    # ))
    if ( !is.null(saveToFolder) ){
        pdfName = file.path(outDir, paste0(title, "_pval-hist-", taxalevel, ".pdf"))
        message("Saving file: ", pdfName)
        pdf(pdfName, width=3, height = 3)
        for (column in names(pvalTable)){
            show(suppressMessages(
                ggplot(data = data.frame(x=pvalTable[,column]), aes(x=x)) +
                    geom_histogram(fill="dodgerblue") +
                    xlab("p-values") +
                    ggtitle(column)  
            ))
        }
        dev.off()
    }
    
}

#' 
#' 
## ------------------------------------------------------------------------------------------------------
pvalHist(m.all, "bigList")

#' 
## ------------------------------------------------------------------------------------------------------
sessionInfo()

