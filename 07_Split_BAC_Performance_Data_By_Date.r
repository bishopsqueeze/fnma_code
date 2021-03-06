##-----------------------------------------------------------------------
##
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------
## clean the cache
##-----------------------------------------------------------------------
#rm(list=ls())

##-----------------------------------------------------------------------
## load libraries
##-----------------------------------------------------------------------
source("/Users/alexstephens/Development/fnma/fnma_code/99_Load_Libraries.r")

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
## set the working directory
##-----------------------------------------------------------------------
setwd("/Users/alexstephens/Development/fnma/data/bac/06_Processed_Performance_Data")
bacDirectory <- "/Users/alexstephens/Development/fnma/data/bac/06_Processed_Performance_Data"
vinDirectory <- "/Users/alexstephens/Development/fnma/data/bac/07_Split_BAC_Data_By_Vintage"
cohDirectory <- "/Users/alexstephens/Development/fnma/data/bac/07_Split_BAC_Data_By_Cohort"

##-----------------------------------------------------------------------
## get the base BAC default file list
##-----------------------------------------------------------------------
file.list   <- dir(path=".", pattern="Combined_Data_BAC.Rda$")
file.num    <- length(file.list)

##-----------------------------------------------------------------------
## get the quarters
##-----------------------------------------------------------------------
file.qtr        <- unlist(lapply(file.list, substr, 1, 6))
file.qtr        <- data.frame(year=as.numeric(substr(file.qtr,1,4)),qtr=as.numeric(substr(file.qtr,6,6)))
file.year       <- unique(file.qtr$year)
file.year.num   <- length(file.year)

##-----------------------------------------------------------------------
## combine all originations from a single year
##-----------------------------------------------------------------------
for (i in 1:file.year.num) {

    yy <- file.year[i]
    
    message("processing year ", yy)
    
    ##-----------------------------------------------------------------------
    ## concat all 4 quarters
    ##-----------------------------------------------------------------------
    for (j in 1:4)
        {
            load(file=paste0(yy,"Q",j,"_Combined_Data_BAC.Rda"))
            
            if (j == 1) {
                Tmp_C = Data_C
            } else {
                Tmp_C = rbindlist( list(Tmp_C, Data_C), use.names=TRUE, fill=TRUE )
            }
            rm("Data_C")
        }
        save(Tmp_C, file=paste0(vinDirectory,"/",yy,"V","_Combined_Data_BAC.Rda"))
    
    ##-----------------------------------------------------------------------
    ## split by observation date into separate files
    ##-----------------------------------------------------------------------
    date.vec        <- Tmp_C[,unique(Monthly.Rpt.Prd)]
    date.vec.num    <- length(date.vec)
    for (k in 1:date.vec.num)
        {
            tmp.date    <- date.vec[k]
            tmp.mm      <- month(tmp.date)
            tmp.yy      <- year(tmp.date)
            
            tmp.hdr     <- paste0(tmp.yy,ifelse(tmp.mm<10,paste0("0",tmp.mm),tmp.mm),"_",yy,"V","_Combined_Data_BAC.Rda")
            Data_C      <- Tmp_C[ Monthly.Rpt.Prd %in% date.vec[k] ]
            
            save(Data_C, file=paste0(cohDirectory,"/",tmp.hdr))
        }
    
}



