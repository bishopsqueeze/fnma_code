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
setwd("/Users/alexstephens/Development/fnma/data/bac/cohort")
cohDirectory <- "/Users/alexstephens/Development/fnma/data/bac/cohort"
outDirectory <- "/Users/alexstephens/Development/fnma/data/bac/cohort_proc"

##-----------------------------------------------------------------------
## get the input files
##-----------------------------------------------------------------------
file.list   <- dir(path=".", pattern="Combined_Data_BAC.Rda$")
file.num    <- length(file.list)

##-----------------------------------------------------------------------
## get cohort indicators
##-----------------------------------------------------------------------
file.coh        <- unique(substr(file.list,1,6))
file.coh.num    <- length(file.coh)

##-----------------------------------------------------------------------
## loop over all vintage-cohort files (i.e., each file contains observations
## from a single month (e.g., 05/2004) that may have been observed in
## different vintage files (e.g., 2001V, 2002V, etc.) ... we take each of
## these files and combine all data into a single file for each monthly
## cohort ... with the goal of creating something akin to what is done @BAC
##-----------------------------------------------------------------------
#for (i in 1:file.coh.num) {
foreach (i=1:file.coh.num) %dopar% {
    
    ## set-up
    coh         <- file.coh[i]
    read.list   <- dir(path=".", pattern=paste0("^",coh))   ## get all of the cohort-vintage files
    read.num    <- length(read.list)                        ## number of cohort-vintage files
    file.out    <- paste0(coh,"_Combined_Data_BAC.Rda")
    
    message("processing cohort ", coh)
    
    ##-----------------------------------------------------------------------
    ## loop over all cohort-vintage files and concat into a single data.table
    ##-----------------------------------------------------------------------
    for (j in 1:read.num) {
    #foreach (i=1:read.num) %dopar% {
    
        load(file=read.list[j])
     
        if (j == 1) {
            Tmp_C = Data_C
        } else {
            Tmp_C = rbind(Tmp_C, Data_C)
        }
        
    rm("Data_C")
    }
    
    ## save the results for this cohort
    save(Tmp_C, file=paste0(outDirectory,"/",file.out))
}




