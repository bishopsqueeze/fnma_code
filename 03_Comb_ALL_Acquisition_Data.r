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

##-----------------------------------------------------------------------
## set the working directory
##-----------------------------------------------------------------------
setwd("/Users/alexstephens/Development/fnma/data/proc")
allDirectory <- "/Users/alexstephens/Development/fnma/data/proc"

##-----------------------------------------------------------------------
## get the acquisitions file list
##-----------------------------------------------------------------------
file.list   <- dir(path=".", pattern="Acquisitions_Data.Rda$")
file.num    <- length(file.list)

##-----------------------------------------------------------------------
## loop over all files and collect the universe of sellers
##-----------------------------------------------------------------------
for (i in 1:file.num)
{
    tmp.file    <- file.list[i]
    tmp.hdr     <- substr(tmp.file, 1, 6)
    load(tmp.file)
    
    ## after the first step, union files
    if (i == 1) {
        Acquisitions_Data.all <- Acquisitions_Data
    } else {
        Acquisitions_Data.all <- rbind(Acquisitions_Data.all, Acquisitions_Data)
    }
    
    ## remove the loaded acquisitions file
    rm("Acquisitions_Data")
}


##-----------------------------------------------------------------------
## Define the data.table key (LOAN_ID) in the merged data
##-----------------------------------------------------------------------
setkey(Acquisitions_Data.all, LOAN_ID)

##-----------------------------------------------------------------------
## Save the merged data
##-----------------------------------------------------------------------
save(Acquisitions_Data.all, file=paste0(allDirectory,"/","Acquisitions_Data_All.Rda"))



##-----------------------------------------------------------------------
## Tabulate basic stats
##-----------------------------------------------------------------------

## create a numeric origination year
Acquisitions_Data.all$ORIG_YY <- as.integer(Acquisitions_Data.all[,substr(ORIG_DTE,4,8)])

## compute yearly (count, UPB) total to cross-check against FNMA acquisition stats
ORIG_UPB_YY_MM  <- xtabs(ORIG_AMT ~ ORIG_YY , data=Acquisitions_Data.all[,])/1e06
LOAN_CNT_YY     <- Acquisitions_Data.all[,table(ORIG_YY)]

## tabule categorical variables by origination year
Acquisitions_Data.all[,table(ORIG_CHN,ORIG_YY)]
Acquisitions_Data.all[,table(Seller.Name,ORIG_YY)]
Acquisitions_Data.all[,table(ORIG_TRM,ORIG_YY)]
Acquisitions_Data.all[,table(NUM_BO,ORIG_YY)]
Acquisitions_Data.all[,table(FTHB_FLG,ORIG_YY)]
Acquisitions_Data.all[,table(PROP_TYP,ORIG_YY)]
Acquisitions_Data.all[,table(NUM_UNIT,ORIG_YY)]
Acquisitions_Data.all[,table(OCC_STAT,ORIG_YY)]
Acquisitions_Data.all[,table(STATE,ORIG_YY)]

## tabulate NA totals for quasi-numeric variables by origination year
Acquisitions_Data.all[,table(is.na(ORIG_RT))]
Acquisitions_Data.all[,table(is.na(ORIG_AMT))]
Acquisitions_Data.all[,table(is.na(OLTV))]
Acquisitions_Data.all[,table(is.na(DTI))]
Acquisitions_Data.all[,table(is.na(CSCORE_B))]
Acquisitions_Data.all[,table(is.na(CSCORE_C))]
Acquisitions_Data.all[,table(NUM_BO, is.na(CSCORE_C))]
Acquisitions_Data.all[,table(is.na(ZIP_3))]


