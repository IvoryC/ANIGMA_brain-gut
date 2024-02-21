# themes

getColorPalette <- function(){
    # Most of these match the previous paper. The Fargo color was newly added here.
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
        Denver_T2 = "#6A3D9A",
        # SUBTYPE labels
        ANR="violet", 
        ARFID="gray",
        BP="darkred",
        EDNOS="pink"
    )
    return(myColorPalette)
}

displayColorPalette <- function(myColorPalette){
    colorKey = data.frame(color=myColorPalette, key=names(myColorPalette))
    if (require(ggplot2)){
        ggplot(data=colorKey, aes(x=1, y = 1:nrow(colorKey), fill=key, label=key)) +
            geom_tile() +
            scale_fill_manual(values = myColorPalette) +
            theme_void()+
            theme(legend.position="none") + 
            geom_text()
    }else{
        warning("Skipping display because package ggplot2 is not available.")
    }
}
