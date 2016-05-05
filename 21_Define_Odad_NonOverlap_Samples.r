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
resDirectory  <- "/Users/alexstephens/Development/fnma/data/bac/20_ODAD_NonOverlap"
smpDirectory  <- "/Users/alexstephens/Development/fnma/data/bac/21_ODAD_NonOverlap_Sample"
rateDirectory  <- "/Users/alexstephens/Development/fnma/data/rate"

##-----------------------------------------------------------------------
## load the current mortgage rate file
##-----------------------------------------------------------------------
load(file=paste0(rateDirectory,"/","FRED_30Y_Conventional_Mortgage_Rate.Rda"))

##-----------------------------------------------------------------------
## set working directory
##-----------------------------------------------------------------------
setwd(resDirectory)

##-----------------------------------------------------------------------
## loop over each month and create a sample
##-----------------------------------------------------------------------
for (j in 1:12)
{

    ##-----------------------------------------------------------------------
    ## load a picklist
    ##-----------------------------------------------------------------------
    tmp.hdr <- ifelse(j<10,paste0("0",j),j)
    load(file=paste0(resDirectory,"/",paste0("M",tmp.hdr,"_Combined_Data_BAC_ODAD_NonOverlap_Picklist.Rda")))

    message("processing month :: ", tmp.hdr)
    
    ##-----------------------------------------------------------------------
    ## split into CUR and DLQ & delete the main file
    ##-----------------------------------------------------------------------
    cur.picklist    <- picklist[["cur"]]; setkey(cur.picklist,LOAN_ID,Monthly.Rpt.Prd)
    dlq.picklist    <- picklist[["dlq"]]; setkey(dlq.picklist,LOAN_ID,Monthly.Rpt.Prd)
    rm("picklist")

    ##-----------------------------------------------------------------------
    ## define sample weights and the sample
    ##-----------------------------------------------------------------------
    cur.wt   <- 20
    dlq.wt   <- 1

    ## set seed
    set.seed(54321)
    
    cur.sample      <- cur.picklist[which((DEF1Y == 1) | (U01 <= 1/cur.wt)),]
    dlq.sample      <- dlq.picklist[which((DEF1Y == 1) | (U01 <= 1/dlq.wt)),]

    ## Compare
    tmp.cur.pop     <- as.matrix(cur.picklist[,table(Monthly.Rpt.Prd,DEF1Y)])
    tmp.cur.smp     <- as.matrix(cur.sample[,table(Monthly.Rpt.Prd,DEF1Y)])

    tmp.comp        <- data.frame(
                                date=as.Date(rownames(tmp.cur.pop)),
                                pop.nd = tmp.cur.pop[,1],
                                pop.d  = tmp.cur.pop[,2],
                                smp.nd = tmp.cur.smp[,1],
                                smp.d  = tmp.cur.smp[,2]
                            )

    ##-----------------------------------------------------------------------
    ## loop over each month represented in the sample, and extract the
    ## combined data from the relevant combined data file
    ##-----------------------------------------------------------------------
    cur.sample.date <- cur.sample[,sort(unique(Monthly.Rpt.Prd))]
    dlq.sample.date <- dlq.sample[,sort(unique(Monthly.Rpt.Prd))]

    for (i in 1:length(cur.sample.date))
    {
        tmp.date    <- cur.sample.date[i]
        tmp.yy      <- year(tmp.date)
        tmp.mm      <- month(tmp.date)
        tmp.mm      <- ifelse(tmp.mm<10,paste0("0",tmp.mm),tmp.mm)
        
        ## load the data
        tmp.file    <- paste0(odadDirectory,"/",tmp.yy,tmp.mm,"_Combined_Data_BAC_ODAD.Rda")
        load(tmp.file)
        
        cur.index   <- cur.sample[which(Monthly.Rpt.Prd == tmp.date),.(LOAN_ID,Monthly.Rpt.Prd)]
        setkey(cur.index,LOAN_ID,Monthly.Rpt.Prd)

        dlq.index   <- dlq.sample[which(Monthly.Rpt.Prd == tmp.date),.(LOAN_ID,Monthly.Rpt.Prd)]
        setkey(dlq.index,LOAN_ID,Monthly.Rpt.Prd)

        if (i == 1) {
            Smp_CUR <- Odad_C[cur.index,]
            Smp_DLQ <- Odad_C[dlq.index,]
        } else {
            Smp_CUR <- rbindlist(list(Smp_CUR, Odad_C[cur.index,]))
            Smp_DLQ <- rbindlist(list(Smp_DLQ, Odad_C[dlq.index,]))
        }
        
    }

    ##-----------------------------------------------------------------------
    ## CUR :: Append additional variables
    ##-----------------------------------------------------------------------

    ## weights
    Smp_CUR$wt <- ifelse(Smp_CUR$DEF1Y==1,1,cur.wt)
    
    ## date
    Smp_CUR[, c("RATE_DTE","MORTG") := rate[date %in% Smp_CUR[,ORIG_DTE],.(date,MORTG)]]

    ## Unemployment deltas
    Smp_CUR$UE_DEL_1Y       <- 100*((Smp_CUR$UNEMP/Smp_CUR$UNEMP_4Q )-1)
    Smp_CUR$UE_DEL_2Y       <- 100*((Smp_CUR$UNEMP/Smp_CUR$UNEMP_8Q )-1)
    Smp_CUR$UE_DEL_3Y       <- 100*((Smp_CUR$UNEMP/Smp_CUR$UNEMP_12Q)-1)
    Smp_CUR$UE_DEL_ORIG     <- 100*((Smp_CUR$UNEMP/Smp_CUR$ORIG_UNEMP)-1)

    ## HPI deltas
    Smp_CUR$HPI_DEL_1Y       <- 100*((Smp_CUR$RPT_IDX/Smp_CUR$RPT_IDX_4Q )-1)
    Smp_CUR$HPI_DEL_2Y       <- 100*((Smp_CUR$RPT_IDX/Smp_CUR$RPT_IDX_8Q )-1)
    Smp_CUR$HPI_DEL_3Y       <- 100*((Smp_CUR$RPT_IDX/Smp_CUR$RPT_IDX_12Q)-1)
    Smp_CUR$HPI_DEL_ORIG     <- 100*((Smp_CUR$RPT_IDX/Smp_CUR$ORIG_IDX)-1)
  
    ##
    Smp_CUR$IS_MOD          <- ifelse(substr(Smp_CUR$MOD_ID,13,13)=="Y",1,0)
    Smp_CUR$NUM_UNIT_BKT    <- ifelse(Smp_CUR$NUM_UNIT > 2, "3+", Smp_CUR$NUM_UNIT)
    Smp_CUR$NUM_BO_BKT      <- ifelse(((Smp_CUR$NUM_BO > 2) | (Smp_CUR$NUM_BO == "")), "3+", Smp_CUR$NUM_BO)    ## Include missings in 3+
    
    Smp_CUR[ , c("ORIG_RT_BKT") := list(as.character(cut(ORIG_RT, breaks = c(-Inf, seq(5,7.5,0.5), Inf),
                labels=c("(-Inf,5.0)","[5.0,5.5)","[5.5,6.0)","[6.0,6.5)","[6.5,7.0)","[7.0,7.5)","[7.5,Inf)"), right = TRUE, ordered = TRUE)))]

    setkey(Smp_CUR,LOAN_ID,Monthly.Rpt.Prd)
   
    ##-----------------------------------------------------------------------
    ## DLQ :: Append additional variables
    ##-----------------------------------------------------------------------
     
    ## weights
    Smp_DLQ$wt <- ifelse(Smp_DLQ$DEF1Y==1,1,dlq.wt)

    ## date
    Smp_DLQ[, c("RATE_DTE","MORTG") := rate[date %in% Smp_DLQ[,ORIG_DTE],.(date,MORTG)]]

    ## Unemployment deltas
    Smp_DLQ$UE_DEL_1Y       <- 100*((Smp_DLQ$UNEMP/Smp_DLQ$UNEMP_4Q )-1)
    Smp_DLQ$UE_DEL_2Y       <- 100*((Smp_DLQ$UNEMP/Smp_DLQ$UNEMP_8Q )-1)
    Smp_DLQ$UE_DEL_3Y       <- 100*((Smp_DLQ$UNEMP/Smp_DLQ$UNEMP_12Q)-1)
    Smp_DLQ$UE_DEL_ORIG     <- 100*((Smp_DLQ$UNEMP/Smp_DLQ$ORIG_UNEMP)-1)
    
    ## HPI deltas
    Smp_DLQ$HPI_DEL_1Y       <- 100*((Smp_DLQ$RPT_IDX/Smp_DLQ$RPT_IDX_4Q )-1)
    Smp_DLQ$HPI_DEL_2Y       <- 100*((Smp_DLQ$RPT_IDX/Smp_DLQ$RPT_IDX_8Q )-1)
    Smp_DLQ$HPI_DEL_3Y       <- 100*((Smp_DLQ$RPT_IDX/Smp_DLQ$RPT_IDX_12Q)-1)
    Smp_DLQ$HPI_DEL_ORIG     <- 100*((Smp_DLQ$RPT_IDX/Smp_DLQ$ORIG_IDX)-1)
    
    ##
    Smp_DLQ$IS_MOD          <- ifelse(substr(Smp_DLQ$MOD_ID,13,13)=="Y",1,0)
    Smp_DLQ$NUM_UNIT_BKT    <- ifelse(Smp_DLQ$NUM_UNIT > 2, "3+", Smp_DLQ$NUM_UNIT)
    Smp_DLQ$NUM_BO_BKT      <- ifelse(((Smp_DLQ$NUM_BO > 2) | (Smp_DLQ$NUM_BO == "")), "3+", Smp_DLQ$NUM_BO) ## Include missings in 3+
    Smp_DLQ[ , c("ORIG_RT_BKT") := list(as.character(cut(ORIG_RT, breaks = c(-Inf, seq(5,7.5,0.5), Inf),
    labels=c("(-Inf,5.0)","[5.0,5.5)","[5.5,6.0)","[6.0,6.5)","[6.5,7.0)","[7.0,7.5)","[7.5,Inf)"), right = TRUE, ordered = TRUE)))]

    setkey(Smp_DLQ,LOAN_ID,Monthly.Rpt.Prd)

    ##-----------------------------------------------------------------------
    ## save raw files
    ##-----------------------------------------------------------------------
    save(Smp_CUR, tmp.comp, file=paste0(smpDirectory,"/",paste0("M",tmp.hdr,"_Combined_Data_BAC_ODAD_NonOverlap_CUR_Sample.Rda")))
    save(Smp_DLQ, tmp.comp, file=paste0(smpDirectory,"/",paste0("M",tmp.hdr,"_Combined_Data_BAC_ODAD_NonOverlap_DLQ_Sample.Rda")))

    ##-----------------------------------------------------------------------
    ## create slim files
    ##-----------------------------------------------------------------------
    Smp_CUR[,c("UNEMP_1Q","UNEMP_2Q","UNEMP_3Q","UNEMP_4Q","UNEMP_8Q","UNEMP_12Q","ORIG_UNEMP") := NULL]
    Smp_CUR[,c("RPT_IDX_1Q","RPT_IDX_2Q","RPT_IDX_3Q","RPT_IDX_4Q","RPT_IDX_8Q","RPT_IDX_12Q","ORIG_IDX","RPT_IDX","MOD_IDX") := NULL]
    Smp_CUR[,c("i.Monthly.Rpt.Prd","LAST_UPB","Count","ORIG_TRM","FRST_DTE","ZIP_3","Maturity.Date",
    "LAG_STAT","MOD_DTE","PP_CTR","MOD_CTR","FCE_CTR","F180_CTR","REPO_CTR","FCE_CTR.M","F180_CTR.M","FMOD_DTE",
    "STATE","DEF1Y.M","DEF.M","NUM_UNIT","NUM_BO","MOD_ID","PP1Y","DEF","MOD_CNT","CSCORE_MN") := NULL]

    Smp_DLQ[,c("UNEMP_1Q","UNEMP_2Q","UNEMP_3Q","UNEMP_4Q","UNEMP_8Q","UNEMP_12Q","ORIG_UNEMP") := NULL]
    Smp_DLQ[,c("RPT_IDX_1Q","RPT_IDX_2Q","RPT_IDX_3Q","RPT_IDX_4Q","RPT_IDX_8Q","RPT_IDX_12Q","ORIG_IDX","RPT_IDX","MOD_IDX") := NULL]
    Smp_DLQ[,c("i.Monthly.Rpt.Prd","LAST_UPB","Count","ORIG_TRM","FRST_DTE","ZIP_3","Maturity.Date",
    "LAG_STAT","MOD_DTE","PP_CTR","MOD_CTR","FCE_CTR","F180_CTR","REPO_CTR","FCE_CTR.M","F180_CTR.M","FMOD_DTE",
    "STATE","DEF1Y.M","DEF.M","NUM_UNIT","NUM_BO","MOD_ID","PP1Y","DEF","MOD_CNT","CSCORE_MN") := NULL]

    ##-----------------------------------------------------------------------
    ## save raw files
    ##-----------------------------------------------------------------------
    save(Smp_CUR, tmp.comp, file=paste0(smpDirectory,"/",paste0("M",tmp.hdr,"_Combined_Data_BAC_ODAD_NonOverlap_CUR_Sample_SLIM.Rda")))
    save(Smp_DLQ, tmp.comp, file=paste0(smpDirectory,"/",paste0("M",tmp.hdr,"_Combined_Data_BAC_ODAD_NonOverlap_DLQ_Sample_SLIM.Rda")))


} ## end of j-loop





