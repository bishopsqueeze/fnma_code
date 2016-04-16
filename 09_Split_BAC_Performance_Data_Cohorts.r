##-----------------------------------------------------------------------
##
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------
## load libraries
##-----------------------------------------------------------------------
source("/Users/alexstephens/Development/fnma/code/99_Load_Libraries.r")

##-----------------------------------------------------------------------
## clean the cache
##-----------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Define the parallel flag
##------------------------------------------------------------------
DOPARALLEL  <- TRUE

##------------------------------------------------------------------
## Register the clusters
##------------------------------------------------------------------
if (DOPARALLEL) {
    library(doMC)
    registerDoMC(2)
}

##-----------------------------------------------------------------------
## set the working & output directories
##-----------------------------------------------------------------------
setwd("/Users/alexstephens/Development/fnma/data/bac/cohort_proc")
cohDirectory <- "/Users/alexstephens/Development/fnma/data/bac/cohort_proc"
odadDirectory <- "/Users/alexstephens/Development/fnma/data/bac/cohort_odad"
modsDirectory <- "/Users/alexstephens/Development/fnma/data/bac/cohort_mods"

##-----------------------------------------------------------------------
## get the input files
##-----------------------------------------------------------------------
file.list   <- dir(path=".", pattern="Combined_Data_BAC.Rda$")
file.num    <- length(file.list)


##-----------------------------------------------------------------------
## loop over each input file; split into two sets of data
##  - Once-defaulted-always-defaulted data (so not chance to cure)
##  - Data that checks the pre- and post-mod history for defaults
##-----------------------------------------------------------------------
#åfor (i in 1:file.num) {
foreach (i=1:file.num) %dopar% {
    
    ## set-up
    file.tmp    <- file.list[i]
    coh         <- substr(file.tmp,1,6)
    file.odad   <- paste0(coh,"_Combined_Data_BAC_ODAD.Rda")
    file.mods   <- paste0(coh,"_Combined_Data_BAC_MODS.Rda")
    
    ## load the data
    message("processing cohort ", coh)
    load(file=file.tmp)
    
    ##-----------------------------------------------------------------------
    ## Create the Once-Default-Always-Default file;  Only include cases
    ## where the default counter is >=0 (so pre-default or at default)
    ## or cases where the default counter (FCE_CTR) is null ... so the
    ## loan never defaults
    ##-----------------------------------------------------------------------
    setkey(Tmp_C, LOAN_ID)
    Odad_C = Tmp_C[which( is.na(FCE_CTR) | (FCE_CTR >= 0))]
    save(Odad_C, file=paste0(odadDirectory,"/",file.odad))
    
    ##-----------------------------------------------------------------------
    ## Create the Mod Once-Default-Always-Default file; Only include cases
    ## where the default counter is >=0 (so pre-default or at default)
    ## or cases where the default counter (FCE_CTR) is null ... so the
    ## loan never defaults
    ##----------------å-------------------------------------------------------
    setkey(Tmp_C, MOD_ID)
    Mods_C = Tmp_C[which( is.na(FCE_CTR.M) | (FCE_CTR.M >= 0) )]
    save(Mods_C, file=paste0(modsDirectory,"/",file.mods))
}




