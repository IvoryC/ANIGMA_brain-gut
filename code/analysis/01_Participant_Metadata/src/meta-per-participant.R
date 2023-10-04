

# The meta data is given on a per-sample basis.  
# For some metrics, particularly the _changes in a given metric_ it is much 
# more intuitive to work in a per-participant framework. 
# Here, we rearrange the data, and add some features.


#### Libraries ####

tellme <- function(name){message("Package ", name, " version: ", packageVersion(name))}

library(tidyr); tellme("tidyr")
suppressPackageStartupMessages(library(dplyr)); tellme("dplyr")

#### Setup ####

outdir = "../output"
suppressWarnings( dir.create(outdir) )
message("Output will be saved to:", outdir)


#### Read meta data ####

meta = read.delim("../../input/meta/ANIGMA-metadata.txt") %>%
    select(PARTICIPANT.ID, LOCATION, TIMEPOINT, AGE, SUBTYPE, BMI, 
           STAI_Y1, STAI_Y2, STAI_TOTAL, PSS, DAYS_TREAT, Weight_kg, 
           DUR_ILLNESS_YRS)
dim(meta)

message("This meta data has ", nrow(meta), " rows and ", ncol(meta), " columns.")


### Remove/modify individual data.

# Note: 469017 and 469019 are the outliers (same as before!) who lost weight during their stay, this was attributed to medication they had been taking ahead of their stay that lead to inflated weight values on arrival.  So the T1 weight (and by extention bmi) is not a valid measure here.

meta[meta$PARTICIPANT.ID=="469017", "BMI"] = NA
meta[meta$PARTICIPANT.ID=="469019", "BMI"] = NA


# Note: 469021 has a T2 PSS score of 0, which we believe is a data entry error.
# Likewise for patient 469101, they have a pss score of 0 at T2.
meta[meta$PARTICIPANT.ID=="469021" & meta$TIMEPOINT=="T2", "PSS"] = NA
meta[meta$PARTICIPANT.ID=="469101" & meta$TIMEPOINT=="T1", "PSS"] = NA

message("After this modification, this meta data has ", nrow(meta), " rows and ", ncol(meta), " columns.")


#### MAIN - arrange by participant ####

# Pull out the constants, 
diffs = meta %>% filter(TIMEPOINT=="T1") %>%
    mutate(T1.severity = 18.5 - BMI) %>% 
    select(PARTICIPANT.ID, LOCATION, AGE, SUBTYPE, T1.severity, DAYS_TREAT, DUR_ILLNESS_YRS)

# Loop through all the things that have a T2-T1 difference.
diffables = c("STAI_Y1", "STAI_Y2", "STAI_TOTAL", "PSS", "BMI", "Weight_kg")
for (feature in diffables){
    t1t2diff = meta %>% 
        select(PARTICIPANT.ID, all_of(feature), TIMEPOINT) %>% 
        pivot_wider(id_cols = PARTICIPANT.ID, names_from = TIMEPOINT, values_from = all_of(feature)) %>%
        select(PARTICIPANT.ID, T1, T2) %>% 
        mutate(diff = T2 - T1) %>%
        rename_with( function(name){ 
            name = ifelse(name=="diff", paste0(feature, ".diff"), name) 
            name = ifelse(name=="T1", paste0(feature, ".T1"), name)
            name = ifelse(name=="T2", paste0(feature, ".T2"), name)
            return(name)
        } ) 
    # %>%
    #   select(PARTICIPANT.ID, all_of(feature))
    diffs = merge(diffs, t1t2diff, by="PARTICIPANT.ID", all.x=T)
}

dim(diffs)

message("This form of the data only has the AN participants. The new form has", nrow(diffs), " rows and ", ncol(diffs), " columns.")

#### save ####
# Export the wide form data.

fileName = file.path(outdir, "ANIGMA-metadata_by_AN_participant.txt")
write.table(diffs, file=fileName, sep="\t", quote=F, row.names = F)

message("The reshaped data was saved to: ", fileName)

# This form only has the participants who has anorexia.  The same shape of data for the healthy control (non-eating disorder) participants, already exists as a subset of the original metadata.  For the those participants, there is no T1/T2 distinction, and no differences to be calculated. 

sessionInfo()
