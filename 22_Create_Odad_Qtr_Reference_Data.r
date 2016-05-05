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

##-----------------------------------------------------------------------
## set the working & output directories
##-----------------------------------------------------------------------
smpDirectory  <- "/Users/alexstephens/Development/fnma/data/bac/21_ODAD_NonOverlap_Sample"
outDirectory  <- "/Users/alexstephens/Development/fnma/data/bac/22_ODAD_NonOverlap_Qtr_Reference_Data"

##-----------------------------------------------------------------------
## set working directory
##-----------------------------------------------------------------------
setwd(smpDirectory)

##-----------------------------------------------------------------------
## Define the list of quarters to combine
##-----------------------------------------------------------------------
qtr.list    <- list(q1=c(1,4,7,10), q2=c(2,5,8,11), q3=c(3,6,9,12))

##-----------------------------------------------------------------------
## loop over each month and create a sample
##-----------------------------------------------------------------------
for (j in 1:length(qtr.list))
{

    tmp.mos <- qtr.list[[j]]
    tmp.qtr <- paste0("Q",j)
    message("processing list :: ", j)

    for (i in 1:length(tmp.mos))
    {
        tmp.mo  <- tmp.mos[i]
        tmp.hdr <- ifelse(tmp.mo<10,paste0("0",tmp.mo),tmp.mo)
        
        message("processing month :: ", tmp.hdr)
        load(file=paste0(smpDirectory,"/",paste0("M",tmp.hdr,"_Combined_Data_BAC_ODAD_NonOverlap_CUR_Sample_SLIM.Rda")))
        load(file=paste0(smpDirectory,"/",paste0("M",tmp.hdr,"_Combined_Data_BAC_ODAD_NonOverlap_DLQ_Sample_SLIM.Rda")))
        
        if (i == 1) {
            Ref_CUR <- Smp_CUR
            Ref_DLQ <- Smp_DLQ
        } else {
            Ref_CUR <- rbindlist( list(Ref_CUR, Smp_CUR), use.names=TRUE, fill=TRUE)
            Ref_DLQ <- rbindlist( list(Ref_DLQ, Smp_DLQ), use.names=TRUE, fill=TRUE)
        }
    }

    ##-----------------------------------------------------------------------
    ## save raw files
    ##-----------------------------------------------------------------------
    save(Ref_CUR, file=paste0(outDirectory,"/",paste0(tmp.qtr,"_Combined_Data_BAC_ODAD_NonOverlap_Qtr_CUR.Rda")))
    save(Ref_DLQ, file=paste0(outDirectory,"/",paste0(tmp.qtr,"_Combined_Data_BAC_ODAD_NonOverlap_Qtr_DLQ.Rda")))

} ## end of j-loop





