##-----------------------------------------------------------------------
##
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------
## clean the cache
##-----------------------------------------------------------------------
rm(list=ls())

##-----------------------------------------------------------------------
## load libraries
##-----------------------------------------------------------------------
source("/Users/alexstephens/Development/fnma/fnma_code/99_Load_Libraries.r")

##-----------------------------------------------------------------------
## set the working directory
##-----------------------------------------------------------------------
setwd("/Users/alexstephens/Development/fnma/data/bac/22_ODAD_NonOverlap_Qtr_Reference_Data")
figDirectory <- "/Users/alexstephens/Development/fnma/data/bac/23_Review_Odad_Qtr_Reference_Data"

##-----------------------------------------------------------------------
## load the reference data
##-----------------------------------------------------------------------

## load the CUR file
file.in     <- dir(path=".", pattern="Q1_Combined_Data_BAC_ODAD_NonOverlap_Qtr_CUR.Rda")
load(file.in)

## load the DLQ file
file.in     <- dir(path=".", pattern="Q1_Combined_Data_BAC_ODAD_NonOverlap_Qtr_DLQ.Rda")
load(file.in)

## define a random U01 to use for filtering
set.seed(54321)
Ref_CUR[, c("U01") := runif(nrow(Ref_CUR))]
Ref_DLQ[, c("U01") := runif(nrow(Ref_DLQ))]


##-----------------------------------------------------------------------
## <function> :: genContTables
##-----------------------------------------------------------------------
## Create clean (CL) and missing (NA) tables for each continuous variable
##-----------------------------------------------------------------------
genContTables   <- function(dat, contvar, nbkt=21)
{
    ## Isolate the NA and non-NA data
    Tmp_CUR         <- dat[,.(DEF1Y, eval(parse(text=contvar)))]
    Tmp_CUR         <- Tmp_CUR[,c(paste0(contvar,"_NA"),paste0(contvar,"_CL")) := list( 1*is.na(eval(parse(text=contvar))), ifelse(is.na(eval(parse(text=contvar))),0,eval(parse(text=contvar)))) ]
    
    ## Isolate the clean (CL) data
    Tmp_CUR.CL      <- Tmp_CUR[!is.na(eval(parse(text=contvar))),]
    Tmp_CUR.CL.qnt  <- quantile(Tmp_CUR.CL[,eval(parse(text=contvar))], seq(0,1,length.out=nbkt))
    Tmp_CUR.CL[ , c(paste0(contvar,"_CL_BKT")) := list(as.character(cut(eval(parse(text=contvar)), breaks = c(-Inf, Tmp_CUR.CL.qnt[2:(length(Tmp_CUR.CL.qnt)-1)], Inf), right = TRUE, ordered = TRUE)))]
   
    ## Tabulate results for the CL and NA data
    Tmp_CUR.CL.tbl <- Tmp_CUR.CL[, list(DefRate=sum(DEF1Y)/length(DEF1Y), bktMean=mean(eval(parse(text=contvar))), bktCnt=length(eval(parse(text=contvar)))), by=c(paste0(contvar,"_CL_BKT"))]
    Tmp_CUR.NA.tbl <- Tmp_CUR[, list(DefRate=sum(DEF1Y)/length(DEF1Y),  bktMean=mean(eval(parse(text=paste0(contvar,"_NA")))), bktCnt=length(eval(parse(text=paste0(contvar,"_NA"))))), by=c(paste0(contvar,"_NA"))]
    
    ## Return CL and NA tabular summaries
    return(list(na=Tmp_CUR.NA.tbl, cl=Tmp_CUR.CL.tbl))
}

##-----------------------------------------------------------------------
## <function> :: genContPlots
##-----------------------------------------------------------------------
## Create clean (CL) and missing (NA) plots for each continuous variable
##-----------------------------------------------------------------------
genContPlots    <- function(dat, contvar, dlqvar, nbkt=21)
{
    tmp.list   <- genContTables(dat, contvar=contvar, nbkt=nbkt)
    tmp.cl     <- tmp.list[["cl"]][order(bktMean),]
    tmp.na     <- tmp.list[["na"]][,]
    tmp.cl.gg  <- ggplot(data=tmp.cl, aes(x=bktMean, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("Bucket Mean") + ylab("Default Rate") + ggtitle(paste0(contvar," :: ", dlqvar))
    tmp.na.gg  <- ggplot(data=tmp.na, aes(x=bktMean, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("Bucket Mean") + ylab("Default Rate") + ggtitle(paste0(contvar," :: ", dlqvar))

    ## Return CL and NA plots
    return(list(cl.gg=tmp.cl.gg, na.gg=tmp.na.gg))
}



##-----------------------------------------------------------------------
## Continuous Variables
##-----------------------------------------------------------------------

## Primary Credit Score @ Origination
CSCORE_B.CUR    <- genContPlots(Ref_CUR, contvar="CSCORE_B", dlq="CUR")
CSCORE_B.DLQ    <- genContPlots(Ref_DLQ, contvar="CSCORE_B", dlq="DLQ")

pdf(file=paste0(figDirectory,"/","CSCORE_B.CL.pdf")); multiplot(CSCORE_B.CUR$cl.gg, CSCORE_B.DLQ$cl.gg); dev.off()
pdf(file=paste0(figDirectory,"/","CSCORE_B.NA.pdf")); multiplot(CSCORE_B.CUR$na.gg, CSCORE_B.DLQ$na.gg); dev.off()

## Coborrower Credit Score @ Origination
CSCORE_C.CUR    <- genContPlots(Ref_CUR, contvar="CSCORE_C", dlq="CUR")
CSCORE_C.DLQ    <- genContPlots(Ref_DLQ, contvar="CSCORE_C", dlq="DLQ")
multiplot(CSCORE_C.CUR$cl.gg, CSCORE_C.DLQ$cl.gg)
multiplot(CSCORE_C.CUR$na.gg, CSCORE_C.DLQ$na.gg)

pdf(file=paste0(figDirectory,"/","CSCORE_C.CL.pdf")); multiplot(CSCORE_C.CUR$cl.gg, CSCORE_C.DLQ$cl.gg); dev.off()
pdf(file=paste0(figDirectory,"/","CSCORE_C.NA.pdf")); multiplot(CSCORE_C.CUR$na.gg, CSCORE_C.DLQ$na.gg); dev.off()


## Loan Age
Loan.Age.CUR    <- genContPlots(Ref_CUR, contvar="Loan.Age", dlq="CUR")
Loan.Age.DLQ    <- genContPlots(Ref_DLQ, contvar="Loan.Age", dlq="DLQ")
multiplot(Loan.Age.CUR$cl.gg, Loan.Age.DLQ$cl.gg)
multiplot(Loan.Age.CUR$na.gg, Loan.Age.DLQ$na.gg)

## Current UPB
FIN_UPB.CUR    <- genContPlots(Ref_CUR, contvar="FIN_UPB", dlq="CUR")
FIN_UPB.DLQ    <- genContPlots(Ref_DLQ, contvar="FIN_UPB", dlq="DLQ")
multiplot(FIN_UPB.CUR$cl.gg, FIN_UPB.DLQ$cl.gg)
multiplot(FIN_UPB.CUR$na.gg, FIN_UPB.DLQ$na.gg)

## Current LTV
CLTV.CUR    <- genContPlots(Ref_CUR, contvar="CLTV", dlq="CUR")
CLTV.DLQ    <- genContPlots(Ref_DLQ, contvar="CLTV", dlq="DLQ")
multiplot(CLTV.CUR$cl.gg, CLTV.DLQ$cl.gg)
multiplot(CLTV.CUR$na.gg, CLTV.DLQ$na.gg)

## Current Unemployment Rate
UNEMP.CUR    <- genContPlots(Ref_CUR, contvar="UNEMP", dlq="CUR")
UNEMP.DLQ    <- genContPlots(Ref_DLQ, contvar="UNEMP", dlq="DLQ")
multiplot(UNEMP.CUR$cl.gg, UNEMP.DLQ$cl.gg)
multiplot(UNEMP.CUR$na.gg, UNEMP.DLQ$na.gg)

## 1 x YoY Relative Change in Unemployment Rate
UE_DEL_1Y.CUR    <- genContPlots(Ref_CUR, contvar="UE_DEL_1Y", dlq="CUR", nbkt=11)
UE_DEL_1Y.DLQ    <- genContPlots(Ref_DLQ, contvar="UE_DEL_1Y", dlq="DLQ", nbkt=11)
multiplot(UE_DEL_1Y.CUR$cl.gg, UE_DEL_1Y.DLQ$cl.gg)

## 2 x YoY Relative Change in Unemployment Rate
UE_DEL_2Y.CUR    <- genContPlots(Ref_CUR, contvar="UE_DEL_2Y", dlq="CUR", nbkt=11)
UE_DEL_2Y.DLQ    <- genContPlots(Ref_DLQ, contvar="UE_DEL_2Y", dlq="DLQ", nbkt=11)
multiplot(UE_DEL_2Y.CUR$cl.gg, UE_DEL_2Y.DLQ$cl.gg)

## 3 x YoY Relative Change in Unemployment Rate
UE_DEL_3Y.CUR    <- genContPlots(Ref_CUR, contvar="UE_DEL_3Y", dlq="CUR", nbkt=11)
UE_DEL_3Y.DLQ    <- genContPlots(Ref_DLQ, contvar="UE_DEL_3Y", dlq="DLQ", nbkt=11)
multiplot(UE_DEL_3Y.CUR$cl.gg, UE_DEL_3Y.DLQ$cl.gg)

## Relative Change in Unemployment Rate Since Origination
UE_DEL_ORIG.CUR    <- genContPlots(Ref_CUR, contvar="UE_DEL_ORIG", dlq="CUR", nbkt=11)
UE_DEL_ORIG.DLQ    <- genContPlots(Ref_DLQ, contvar="UE_DEL_ORIG", dlq="DLQ", nbkt=11)
multiplot(UE_DEL_ORIG.CUR$cl.gg, UE_DEL_ORIG.DLQ$cl.gg)

## 1 x YoY Relative Change in HPI
HPI_DEL_1Y.CUR    <- genContPlots(Ref_CUR, contvar="HPI_DEL_1Y", dlq="CUR")
HPI_DEL_1Y.DLQ    <- genContPlots(Ref_DLQ, contvar="HPI_DEL_1Y", dlq="DLQ")
multiplot(HPI_DEL_1Y.CUR$cl.gg, HPI_DEL_1Y.DLQ$cl.gg)
multiplot(HPI_DEL_1Y.CUR$na.gg, HPI_DEL_1Y.DLQ$na.gg)

## 2 x YoY Relative Change in HPI
HPI_DEL_2Y.CUR    <- genContPlots(Ref_CUR, contvar="HPI_DEL_2Y", dlq="CUR")
HPI_DEL_2Y.DLQ    <- genContPlots(Ref_DLQ, contvar="HPI_DEL_2Y", dlq="DLQ")
multiplot(HPI_DEL_2Y.CUR$cl.gg, HPI_DEL_2Y.DLQ$cl.gg)

## 3 x YoY Relative Change in HPI
HPI_DEL_3Y.CUR    <- genContPlots(Ref_CUR, contvar="HPI_DEL_3Y", dlq="CUR")
HPI_DEL_3Y.DLQ    <- genContPlots(Ref_DLQ, contvar="HPI_DEL_3Y", dlq="DLQ")
multiplot(HPI_DEL_3Y.CUR$cl.gg, HPI_DEL_3Y.DLQ$cl.gg)

## Relative Change in HPI since Origination
HPI_DEL_ORIG.CUR    <- genContPlots(Ref_CUR, contvar="HPI_DEL_ORIG", dlq="CUR")
HPI_DEL_ORIG.DLQ    <- genContPlots(Ref_DLQ, contvar="HPI_DEL_ORIG", dlq="DLQ")
multiplot(HPI_DEL_ORIG.CUR$cl.gg, HPI_DEL_ORIG.DLQ$cl.gg)

## [CONTROL] Uniform Random
U01.CUR    <- genContPlots(Ref_CUR, contvar="U01", dlq="CUR")
U01.DLQ    <- genContPlots(Ref_DLQ, contvar="U01", dlq="DLQ")
jpeg(file=paste0(figDirectory,"/","length-hist.jpg"))
multiplot(U01.CUR$cl.gg, U01.DLQ$cl.gg)
dev.off()

## Coborrower Credit Score @ Origination
DTI.CUR    <- genContPlots(Ref_CUR, contvar="DTI", dlq="CUR")
DTI.DLQ    <- genContPlots(Ref_DLQ, contvar="DTI", dlq="DLQ")
multiplot(DTI.CUR$cl.gg, DTI.DLQ$cl.gg)
multiplot(DTI.CUR$na.gg, DTI.DLQ$na.gg)


##-----------------------------------------------------------------------
## Categorical Variables
##-----------------------------------------------------------------------

## Last Payment Status
#LAST_STAT.CUR       <- Ref_CUR[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="LAST_STAT"]
#LAST_STAT.CUR.gg    <- ggplot(data=LAST_STAT.CUR, aes(x=LAST_STAT.CUR, y=DefRate, fill=DefRate)) + geom_bar(stat="identity")
LAST_STAT.DLQ       <- Ref_DLQ[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="LAST_STAT"]
LAST_STAT.DLQ.gg    <- ggplot(data=LAST_STAT.DLQ, aes(x=LAST_STAT, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("LAST_STAT") + ylab("Default Rate") + ggtitle(paste0("LAST_STAT"," :: ", "DLQ"))
LAST_STAT.DLQ.gg

## Property Type
PROP_TYP.CUR       <- Ref_CUR[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="PROP_TYP"]
PROP_TYP.CUR.gg    <- ggplot(data=PROP_TYP.CUR, aes(x=PROP_TYP, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("PROP_TYP") + ylab("Default Rate") + ggtitle(paste0("PROP_TYP"," :: ", "CUR"))
PROP_TYP.DLQ       <- Ref_DLQ[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="PROP_TYP"]
PROP_TYP.DLQ.gg    <- ggplot(data=PROP_TYP.DLQ, aes(x=PROP_TYP, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("PROP_TYP") + ylab("Default Rate") + ggtitle(paste0("PROP_TYP"," :: ", "DLQ"))
multiplot(PROP_TYP.CUR.gg, PROP_TYP.DLQ.gg)

## Origination Channel
ORIG_CHN.CUR       <- Ref_CUR[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="ORIG_CHN"]
ORIG_CHN.CUR.gg    <- ggplot(data=ORIG_CHN.CUR, aes(x=ORIG_CHN, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("ORIG_CHN") + ylab("Default Rate") + ggtitle(paste0("ORIG_CHN"," :: ", "CUR"))
ORIG_CHN.DLQ       <- Ref_DLQ[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="ORIG_CHN"]
ORIG_CHN.DLQ.gg    <- ggplot(data=ORIG_CHN.DLQ, aes(x=ORIG_CHN, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("ORIG_CHN") + ylab("Default Rate") + ggtitle(paste0("ORIG_CHN"," :: ", "DLQ"))
multiplot(ORIG_CHN.CUR.gg, ORIG_CHN.DLQ.gg)

## Occupancy Status
OCC_STAT.CUR       <- Ref_CUR[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="OCC_STAT"]
OCC_STAT.CUR.gg    <- ggplot(data=OCC_STAT.CUR, aes(x=OCC_STAT, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("OCC_STAT") + ylab("Default Rate") + ggtitle(paste0("OCC_STAT"," :: ", "CUR"))
OCC_STAT.DLQ       <- Ref_DLQ[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="OCC_STAT"]
OCC_STAT.DLQ.gg    <- ggplot(data=OCC_STAT.DLQ, aes(x=OCC_STAT, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("OCC_STAT") + ylab("Default Rate") + ggtitle(paste0("OCC_STAT"," :: ", "DLQ"))
multiplot(OCC_STAT.CUR.gg, OCC_STAT.DLQ.gg)

## Purpose Code
PURPOSE.CUR       <- Ref_CUR[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="PURPOSE"]
PURPOSE.CUR.gg    <- ggplot(data=PURPOSE.CUR, aes(x=PURPOSE, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("PURPOSE") + ylab("Default Rate") + ggtitle(paste0("PURPOSE"," :: ", "CUR"))
PURPOSE.DLQ       <- Ref_DLQ[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="PURPOSE"]
PURPOSE.DLQ.gg    <- ggplot(data=PURPOSE.DLQ, aes(x=PURPOSE, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("PURPOSE") + ylab("Default Rate") + ggtitle(paste0("PURPOSE"," :: ", "DLQ"))
multiplot(PURPOSE.CUR.gg, PURPOSE.DLQ.gg)

## Indicator denoting if has been modified (not "ever modified")
IS_MOD.CUR       <- Ref_CUR[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="IS_MOD"]
IS_MOD.CUR.gg    <- ggplot(data=IS_MOD.CUR, aes(x=IS_MOD, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("IS_MOD") + ylab("Default Rate") + ggtitle(paste0("IS_MOD"," :: ", "CUR"))
IS_MOD.DLQ       <- Ref_DLQ[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="IS_MOD"]
IS_MOD.DLQ.gg    <- ggplot(data=IS_MOD.DLQ, aes(x=IS_MOD, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("IS_MOD") + ylab("Default Rate") + ggtitle(paste0("IS_MOD"," :: ", "DLQ"))
multiplot(IS_MOD.CUR.gg, IS_MOD.DLQ.gg)

## Number of Units (bucketed)
NUM_UNIT_BKT.CUR       <- Ref_CUR[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="NUM_UNIT_BKT"]
NUM_UNIT_BKT.CUR.gg    <- ggplot(data=NUM_UNIT_BKT.CUR, aes(x=NUM_UNIT_BKT, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("NUM_UNIT_BKT") + ylab("Default Rate") + ggtitle(paste0("NUM_UNIT_BKT"," :: ", "CUR"))
NUM_UNIT_BKT.DLQ       <- Ref_DLQ[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="NUM_UNIT_BKT"]
NUM_UNIT_BKT.DLQ.gg    <- ggplot(data=NUM_UNIT_BKT.DLQ, aes(x=NUM_UNIT_BKT, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("NUM_UNIT_BKT") + ylab("Default Rate") + ggtitle(paste0("NUM_UNIT_BKT"," :: ", "DLQ"))
multiplot(NUM_UNIT_BKT.CUR.gg, NUM_UNIT_BKT.DLQ.gg)

## Number of Borrowers (bucketed)
NUM_BO_BKT.CUR       <- Ref_CUR[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="NUM_BO_BKT"]
NUM_BO_BKT.CUR.gg    <- ggplot(data=NUM_BO_BKT.CUR, aes(x=NUM_BO_BKT, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("NUM_BO_BKT") + ylab("Default Rate") + ggtitle(paste0("NUM_BO_BKT"," :: ", "CUR"))
NUM_BO_BKT.DLQ       <- Ref_DLQ[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="NUM_BO_BKT"]
NUM_BO_BKT.DLQ.gg    <- ggplot(data=NUM_BO_BKT.DLQ, aes(x=NUM_BO_BKT, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("NUM_BO_BKT") + ylab("Default Rate") + ggtitle(paste0("NUM_BO_BKT"," :: ", "DLQ"))
multiplot(NUM_BO_BKT.CUR.gg, NUM_BO_BKT.DLQ.gg)

## Number of Borrowers (bucketed)
ORIG_RT_BKT.CUR       <- Ref_CUR[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="ORIG_RT_BKT"]
ORIG_RT_BKT.CUR.gg    <- ggplot(data=ORIG_RT_BKT.CUR, aes(x=ORIG_RT_BKT, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("ORIG_RT_BKT") + ylab("Default Rate") + ggtitle(paste0("ORIG_RT_BKT"," :: ", "CUR"))
ORIG_RT_BKT.DLQ       <- Ref_DLQ[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="ORIG_RT_BKT"]
ORIG_RT_BKT.DLQ.gg    <- ggplot(data=ORIG_RT_BKT.DLQ, aes(x=ORIG_RT_BKT, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("ORIG_RT_BKT") + ylab("Default Rate") + ggtitle(paste0("ORIG_RT_BKT"," :: ", "DLQ"))
multiplot(ORIG_RT_BKT.CUR.gg, ORIG_RT_BKT.DLQ.gg)

## First Time Homebuyer
FTHB_FLG.CUR       <- Ref_CUR[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="FTHB_FLG"]
FTHB_FLG.CUR.gg    <- ggplot(data=FTHB_FLG.CUR, aes(x=FTHB_FLG, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("FTHB_FLG") + ylab("Default Rate") + ggtitle(paste0("FTHB_FLG"," :: ", "CUR"))
FTHB_FLG.DLQ       <- Ref_DLQ[, list(DefRate=sum(DEF1Y)/length(DEF1Y)),by="FTHB_FLG"]
FTHB_FLG.DLQ.gg    <- ggplot(data=FTHB_FLG.DLQ, aes(x=FTHB_FLG, y=DefRate, fill=DefRate)) + geom_bar(stat="identity") + xlab("FTHB_FLG") + ylab("Default Rate") + ggtitle(paste0("FTHB_FLG"," :: ", "DLQ"))
multiplot(FTHB_FLG.CUR.gg, FTHB_FLG.DLQ.gg)





