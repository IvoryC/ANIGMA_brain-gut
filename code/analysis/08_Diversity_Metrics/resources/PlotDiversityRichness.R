
# Plot diversity and richness

# These methods are built for a data frame that has a column "TIMEPOINT" 
# and TIMEPOINT has 3 values: "HC", "T1", "T2" (in that order)

#### size By p-value

sizeByP <- function(p, sig){
    size = ifelse(p < sig, (-log(p)-1), 1)
    return(size)
}

# example usage: sizeByP(0.003, sig=0.05) # 4.8
# example usage: sizeByP(0.03, sig=0.05) # 2.5
# example usage: sizeByP(0.3, sig=0.05) # 1

####

#### plot diversity ####

plotDiversity = function(df, column, sig=0.05, saveToDir=NA, tLevel=NA){
    df$diversity  = df[,column]
    
    p = ggplot(data=df, aes(x=TIMEPOINT, y=diversity, fill=TIMEPOINT)) +
        geom_boxplot() +
        geom_violin(fill=NA) +
        geom_jitter(height = 0, width = .1) +
        scale_fill_manual(values = myColorPalette) +
        ggtitle(column)
    
    if (!is.na(tLevel)){
        p = p +
            labs(subtitle = paste0(tLevel, " level"))
    }
    
    if (!is.na(sig)){
        
        topY = max(df$diversity, na.rm=T)
        bitHigher = topY * 0.05
        ysigs = c(HCvT1p=topY+(bitHigher*1), HCvT2p=topY+(bitHigher*2), T2vT1p=topY+(bitHigher*3))
        
        HCvT1p = t.test(df[df$TIMEPOINT=="HC", "diversity"],
                        df[df$TIMEPOINT=="T1", "diversity"])$p.value
        p = p +
            geom_segment(x=1, xend=2, y=ysigs["HCvT1p"], yend=ysigs["HCvT1p"],
                         col=ifelse(HCvT1p < sig, "black", "gray"),
                         size=sizeByP(HCvT1p, sig=sig))
        
        HCvT2p = t.test(df[df$TIMEPOINT=="HC", "diversity"],
                        df[df$TIMEPOINT=="T2", "diversity"])$p.value
        p = p +
            geom_segment(x=.8, xend=3.2, y=ysigs["HCvT2p"], yend=ysigs["HCvT2p"],
                         col=ifelse(HCvT2p < sig, "black", "gray"),
                         size=sizeByP(HCvT2p, sig=sig))
        
        T2vT1p = t.test(df[df$TIMEPOINT=="T2", "diversity"],
                        df[df$TIMEPOINT=="T1", "diversity"])$p.value
        p = p +
            geom_segment(x=2, xend=3, y=ysigs["T2vT1p"], yend=ysigs["T2vT1p"],
                         col=ifelse(T2vT1p < sig, "black", "gray"),
                         size=sizeByP(T2vT1p, sig=sig))
        
        p = p +
            annotate("text", x=-Inf, y=ysigs, 
                     label=signif(c(HCvT1p, HCvT2p, T2vT1p), 
                                  digits=3), hjust = "inward")
        
        p = p +
            labs(caption=paste0("Horizontal bars indicated if t-test was significant.
             Bars are black if p-value is less than ", sig, ", otherwise gray. 
             Line thickness is proportional to the neg. log p."))
    }
    
    p = p +  scale_y_continuous(expand = expansion(add = 0.5))
    
    if (!is.na(saveToDir)){
        filename = filename = file.path(saveToDir, paste0(column, ".png"))
        ggsave(filename = filename)
        message("Saved image as: ", filename)
    }
    
    return(p)
}

# example usage: plotDiversity(meta, column="alpha.div.raw", sig=0.05)
# example usage: plotDiversity(meta, column="alpha.div.raw", sig=0.05, saveToDir="../output")
# example usage: plotDiversity(meta, column="alpha.div.raw", sig=0.05, tLevel="species")


#### plot richness ####
plotRichness = function(df, column, sig=0.05, saveToDir=NA, tLevel=NA){
    df$richness  = df[,column]
    p = ggplot(data=df, aes(x=TIMEPOINT, y=richness, fill=TIMEPOINT)) +
        # scale_y_continuous(breaks=seq(0, ncol(counts), 5)) +
        geom_hline(yintercept=c(0, ncol(counts)), col="white", size=2) +
        geom_boxplot(outlier.alpha = 0) +
        geom_violin(fill=NA) +
        geom_jitter(height = 0, width = .1) +
        scale_fill_manual(values = myColorPalette) +
        ggtitle(column)
    
    if (!is.na(tLevel)){
        p = p +
            labs(subtitle = paste0(tLevel, " level"))
    }
    
    if (!is.na(sig)){
        
        topY = max(df$richness, na.rm=T)
        bitHigher = topY * 0.05
        ysigs = c(HCvT1p=topY+(bitHigher*1), HCvT2p=topY+(bitHigher*2), T2vT1p=topY+(bitHigher*3))
        
        HCvT1p = t.test(df[df$TIMEPOINT=="HC",column], 
                        df[df$TIMEPOINT=="T1",column])$p.value
        p = p + 
            geom_segment(x=1, xend=2, y=ysigs["HCvT1p"], yend=ysigs["HCvT1p"], 
                         col=ifelse(HCvT1p < sig, "black", "gray"),
                         size=sizeByP(HCvT1p, sig=sig))
        
        HCvT2p = t.test(df[df$TIMEPOINT=="HC",column], 
                        df[df$TIMEPOINT=="T2",column])$p.value
        p = p + 
            geom_segment(x=1, xend=3, y=ysigs["HCvT2p"], yend=ysigs["HCvT2p"], 
                         col=ifelse(HCvT2p < sig, "black", "gray"),
                         size=sizeByP(HCvT2p, sig=sig))
        
        T2vT1p = t.test(df[df$TIMEPOINT=="T2",column], 
                        df[df$TIMEPOINT=="T1",column])$p.value
        p = p + 
            geom_segment(x=2, xend=3, y=ysigs["T2vT1p"], yend=ysigs["T2vT1p"], 
                         col=ifelse(T2vT1p < sig, "black", "gray"),
                         size=sizeByP(T2vT1p, sig=sig))
        
        p = p +
            annotate("text", x=-Inf, y=ysigs, 
                     label=signif(c(HCvT1p, HCvT2p, T2vT1p), 
                                  digits=3), hjust = "inward")
        
        p = p +
            labs(caption=paste0("Horizontal bars indicated if t-test was significant.
             Bars are black if p-value is less than ", sig, ", otherwise gray. 
             Line thickness is proportional to the neg. log p."))
    }
    
    if (!is.na(saveToDir)){
        filename = filename = file.path(saveToDir, paste0(column, ".png"))
        ggsave(filename = filename)
        message("Saved image as: ", filename)
    }
    
    return(p)
}

# example usage: plotRichness(data, column="richness.raw")
# example usage: plotRichness(data, column="richness.raw", saveToDir="../output")
# example usage: plotRichness(data, column="richness.raw", tLevel="species")

