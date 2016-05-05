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
odadDirectory <- "/Users/alexstephens/Development/fnma/data/bac/09_ODAD_BAC_Data_By_Cohort"
modsDirectory <- "/Users/alexstephens/Development/fnma/data/bac/09_MODS_BAC_Data_By_Cohort"
outDirectory <- "/Users/alexstephens/Development/fnma/data/bac/20_ODAD_NonOverlap"

odad.list   <- list()
odad.loan   <- list()
mods.list   <- list()

odad.out    <- "BAC_ODAD_Default_Prepay_Count.Rda"
mods.out    <- "BAC_MODS_Default_Prepay_Count.Rda"


##-----------------------------------------------------------------------
## [ODAD] get the input files
##-----------------------------------------------------------------------
setwd(odadDirectory)

file.list   <- dir(path=odadDirectory, pattern="Combined_Data_BAC_ODAD.Rda$")
file.num    <- length(file.list)

##-----------------------------------------------------------------------
## create summaries
##-----------------------------------------------------------------------
for (i in 1:file.num)
{
    ## set-up
    file.tmp    <- file.list[i]
    hdr         <- substr(file.tmp,1,6)
    coh         <- file.tmp
    last.stat   <- c('C',1:9,'P','S','R','F','X')   ## vector of all possible delinq states
    
    ## load the data
    message("processing ODAD file :: ", coh)
    load(file=file.tmp)
    
    ##-----------------------------------------------------------------------
    ## collect monthly default & delinquency data
    ##-----------------------------------------------------------------------
    tmp.tot     <- nrow(Odad_C)
    
    ## capture delinquency data
    tmp.stat            <- rep(0,length(last.stat))
    tmp.dlq             <- as.data.frame(Odad_C[,table(LAST_STAT)])
    tmp.dlq.ch          <- as.character(tmp.dlq[,1])
    idx.dlq             <- match(last.stat,tmp.dlq.ch)[!is.na(match(last.stat,tmp.dlq.ch))]
    idx.stat            <- match(tmp.dlq.ch,last.stat)[!is.na(match(tmp.dlq.ch,last.stat))]
    tmp.stat[idx.stat]  <- tmp.dlq[,2]
    
    ## create a temporary DF of default/non-default counts
    tmp.df.1    <- data.frame(
                        hdr         = as.numeric(hdr),
                        tot         = tmp.tot,
                        ndef.cnt    = Odad_C[,sum(DEF==0)],
                        def.cnt     = Odad_C[,sum(DEF==1)],
                        ndef.pct    = Odad_C[,sum(DEF==0)]/tmp.tot,
                        def.pct     = Odad_C[,sum(DEF==1)]/tmp.tot,
                        ndef1y.cnt  = Odad_C[,sum(DEF1Y==0)],
                        def1y.cnt   = Odad_C[,sum(DEF1Y==1)],
                        ndef1y.pct  = Odad_C[,sum(DEF1Y==0)]/tmp.tot,
                        def1y.pct   = Odad_C[,sum(DEF1Y==1)]/tmp.tot
                    )
    
    ## create a temporary DF of delinquency state counts
    tmp.df.2         <- as.vector(tmp.stat)
    names(tmp.df.2)  <- last.stat
    
    ## combine the default & delinquency data
    odad.list[[i]] <- unlist(c(tmp.df.1,tmp.df.2))
    
    ##-----------------------------------------------------------------------
    ## create a picklist of loans by pulling LOAN_ID and Default flag for each loan
    ##-----------------------------------------------------------------------
    if (i == 1) {
        odad.picklist <- Odad_C[,.(LOAN_ID, Monthly.Rpt.Prd, LAST_STAT, DEF, DEF1Y)]
    } else {
        odad.picklist <- rbindlist( list(odad.picklist, Odad_C[,.(LOAN_ID, Monthly.Rpt.Prd, LAST_STAT, DEF, DEF1Y)]), use.names=TRUE, fill=TRUE)
    }
}

##-----------------------------------------------------------------------
##
##-----------------------------------------------------------------------
odad.dlq.def.tbl    <- do.call(rbind,odad.list)
save(odad.dlq.def.tbl, file=paste0(outDirectory,"/","Combined_Data_BAC_ODAD_DlqDef.Rda"))


##-----------------------------------------------------------------------
## break the full picklist into 12 sets of non-overlapping cohorts
##  - will facilitate a model of the 1Y default rate
##-----------------------------------------------------------------------

## define the year range as [2001, 2014] (exclude 2000 b/c of sparsity)
yy  <- 2000 + seq(1,14)

## loop over each month
for (mm in 1:12)
{
    
    ## create a vector of dates for the non-overlapping cohorts
    dd  <- as.Date(paste0(yy,"-",ifelse(mm<10,paste0(0,mm),mm),"-","01"),"%Y-%m-%d")

    ## loop over each date and isolate CUR & DLQ
    for (i in 1:length(dd))
    {
        tmp.dt      <- odad.picklist[(Monthly.Rpt.Prd == dd[i]),]
        tmp.dt$U01  <- runif(dim(tmp.dt)[1])                           ## add a U[0,1]

        ## create separate picklists for non-defaulted CUR and DLQ loans
        if (i == 1) {
            tmp.cur <- tmp.dt[ (LAST_STAT == "C") & (DEF == 0), ]
            tmp.dlq <- tmp.dt[ (LAST_STAT %in% c(1:5)) & (DEF == 0), ]
        } else {
            tmp.cur <- rbindlist( list(tmp.cur, tmp.dt[ (LAST_STAT == "C") & (DEF == 0), ]), use.names=TRUE, fill=TRUE)
            tmp.dlq <- rbindlist( list(tmp.dlq, tmp.dt[ (LAST_STAT %in% c(1:5)) & (DEF == 0), ]), use.names=TRUE, fill=TRUE)
        }
    }
    ## set the key
    setkey(tmp.cur, LOAN_ID, Monthly.Rpt.Prd)
    setkey(tmp.dlq, LOAN_ID, Monthly.Rpt.Prd)
    
    ## save the picklists
    picklist <- list(cur=tmp.cur, dlq=tmp.dlq)
    save(picklist, file=paste0(outDirectory,"/","M",ifelse(mm<10,paste0(0,mm),mm),"_Combined_Data_BAC_ODAD_NonOverlap_Picklist.Rda"))
}


##-----------------------------------------------------------------------
## save the main picklist (large)
##-----------------------------------------------------------------------
save(odad.picklist, file=paste0(outDirectory,"/","Combined_Data_BAC_ODAD_Full_Picklist.Rda"))


