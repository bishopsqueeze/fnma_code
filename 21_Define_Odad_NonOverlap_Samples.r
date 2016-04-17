##-----------------------------------------------------------------------
##
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------
## load libraries
##-----------------------------------------------------------------------
source("/Users/alexstephens/Development/fnma/fnma_code/99_Load_Libraries.r")

##-----------------------------------------------------------------------
## clean the cache
##-----------------------------------------------------------------------
rm(list=ls())

##-----------------------------------------------------------------------
## set the working & output directories
##-----------------------------------------------------------------------
odadDirectory <- "/Users/alexstephens/Development/fnma/data/bac/cohort_odad"
resDirectory  <- "/Users/alexstephens/Development/fnma/data/bac/results"
smpDirectory  <- "/Users/alexstephens/Development/fnma/data/bac/sample_m01"

##-----------------------------------------------------------------------
## load a picklist
##-----------------------------------------------------------------------
setwd(resDirectory)
load(file=paste0(resDirectory,"/","M01_Combined_Data_BAC_ODAD_NonOverlap_Picklist.Rda"))

##-----------------------------------------------------------------------
## split into CUR and DLQ & delete the main file
##-----------------------------------------------------------------------
cur.picklist    <- picklist[["cur"]]; setkey(cur.picklist,LOAN_ID,Monthly.Rpt.Prd)
dlq.picklist    <- picklist[["dlq"]]; setkey(dlq.picklist,LOAN_ID,Monthly.Rpt.Prd)
rm("picklist")

##-----------------------------------------------------------------------
##
##-----------------------------------------------------------------------
cur.wt   <- 10
dlq.wt   <- 1

cur.sample      <- cur.picklist[which((DEF1Y == 1) | (U01 < 1/cur.wt)),]
dlq.sample      <- dlq.picklist[which((DEF1Y == 1) | (U01 < 1/dlq.wt)),]

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

##cur.picklist[,length(unique(LOAN_ID))]/nrow(cur.picklist)
##dlq.picklist[,length(unique(LOAN_ID))]/nrow(dlq.picklist)

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
Smp_CUR$wt <- cur.wt
Smp_DLQ$wt <- dlq.wt


Smp_CUR$CSCORE_MN_NA    <- is.na(Smp_CUR$CSCORE_MN)
Smp_CUR$DTI_NA          <- is.na(Smp_CUR$DTI)
Smp_CUR$CSCORE_MN_CL    <- ifelse(is.na(Smp_CUR$CSCORE_MN),0,Smp_CUR$CSCORE_MN)
Smp_CUR$DTI_CL          <- ifelse(is.na(Smp_CUR$DTI),0,Smp_CUR$DTI)
Smp_CUR$DHPI_ORIG       <-100*((Smp_CUR$RPT_IDX/Smp_CUR$ORIG_IDX)-1)

Smp_DLQ$CSCORE_MN_NA    <- is.na(Smp_DLQ$CSCORE_MN)
Smp_DLQ$DTI_NA          <- is.na(Smp_DLQ$DTI)
Smp_DLQ$CSCORE_MN_CL    <- ifelse(is.na(Smp_DLQ$CSCORE_MN),0,Smp_DLQ$CSCORE_MN)
Smp_DLQ$DTI_CL          <- ifelse(is.na(Smp_DLQ$DTI),0,Smp_DLQ$DTI)
Smp_DLQ$DHPI_ORIG       <-100*((Smp_DLQ$RPT_IDX/Smp_DLQ$ORIG_IDX)-1)

Smp_CUR[,c("i.Monthly.Rpt.Prd") := NULL]
Smp_DLQ[,c("i.Monthly.Rpt.Prd") := NULL]

setkey(Smp_CUR,LOAN_ID,Monthly.Rpt.Prd)
setkey(Smp_DLQ,LOAN_ID,Monthly.Rpt.Prd)

##-----------------------------------------------------------------------
##
##-----------------------------------------------------------------------
save(Smp_CUR, file=paste0(smpDirectory,"/","M01_Combined_Data_BAC_ODAD_NonOverlap_CUR_Sample.Rda"))
save(Smp_DLQ, file=paste0(smpDirectory,"/","M01_Combined_Data_BAC_ODAD_NonOverlap_DLQ_Sample.Rda"))




cbind(Smp_CUR[,table(Monthly.Rpt.Prd,DEF1Y)],Smp_DLQ[,table(Monthly.Rpt.Prd,DEF1Y)])



#
a <- glm(DEF1Y ~ CSCORE_MN_CL + DTI_CL + OCLTV + UNEMP + DHPI_ORIG + as.factor(CSCORE_MN_NA) + as.factor(DTI_NA) + as.factor(NUM_BO) + as.factor(FTHB_FLG) + as.factor(PURPOSE) + as.factor(PROP_TYP) + as.factor(NUM_UNIT) + as.factor(OCC_STAT), data=Smp_CUR, family="binomial")

summary(a)

a2 <- glm(DEF1Y ~ CSCORE_MN_CL + OCLTV + Loan.Age + DHPI_ORIG + LAST_RT + as.factor(LAST_STAT), data=Smp_DLQ, family="binomial")
#+ as.factor(CSCORE_MN_NA) + as.factor(DTI_NA) + as.factor(NUM_BO) + as.factor(FTHB_FLG) + as.factor(PURPOSE) + as.factor(PROP_TYP) + as.factor(NUM_UNIT) + as.factor(OCC_STAT)  DTI_CL + OCLTV +
summary(a2)

tmp         <- Smp_DLQ
tmp$pred    <- predict(a2,data=tmp,type="response")

b <- tmp[,list(pred=mean(pred), act=sum(DEF1Y)/length(DEF1Y)), by="Monthly.Rpt.Prd"]
b <- b[order(Monthly.Rpt.Prd),]

plot(b[,.(Monthly.Rpt.Prd, act)], typ="b"); points(b[,.(Monthly.Rpt.Prd, pred)], typ="b",col="red")



