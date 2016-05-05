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
setwd("/Users/alexstephens/Development/fnma/data/bac")
bacDirectory <- "/Users/alexstephens/Development/fnma/data/bac"

##-----------------------------------------------------------------------
## get the Performance file list
##-----------------------------------------------------------------------
file.list   <- dir(path=".", pattern="Performance_Data_BAC.Rda$")
file.num    <- length(file.list)





## Load the file

## create a default indicator
Tmp_C[,def := ifelse(is.na(FCE_CTR),0,ifelse(FCE_CTR<=0, 1, 0))]

Tmp_C2 = Tmp_C[which(is.na(FCE_CTR) | FCE_CTR >=0),]


a <- Tmp_C2[,table(Loan.Age,def)]
colnames(a) <- c("ND","D")
b <- data.frame(
AGE = as.numeric(rownames(a)),
ND  = as.vector(a[,"ND"]),
D   = as.vector(a[,"D"])
)

b$TOT <- b$ND + b$D
b$defrate <- b$D/b$TOT
b$cumdef <- cumsum(b$D)/b$TOT

q <- Tmp_C2[,table(Loan.Age,LAST_STAT)]


##-----------------------------------------------------------------------
## Tabulate basic stats
##-----------------------------------------------------------------------

## create a numeric origination year
Acquisitions_Data.bac.all$ORIG_YY <- as.integer(Acquisitions_Data.bac.all[,substr(ORIG_DTE,4,8)])

## compute yearly (count, UPB) total to cross-check against FNMA acquisition stats
ORIG_UPB_YY_MM  <- xtabs(ORIG_AMT ~ ORIG_YY , data=Acquisitions_Data.bac.all[,])/1e06
LOAN_CNT_YY     <- Acquisitions_Data.bac.all[,table(ORIG_YY)]

## tabule categorical variables by origination year
Acquisitions_Data.bac.all[,table(ORIG_CHN,ORIG_YY)]
Acquisitions_Data.bac.all[,table(Seller.Name,ORIG_YY)]
Acquisitions_Data.bac.all[,table(ORIG_TRM,ORIG_YY)]
Acquisitions_Data.bac.all[,table(NUM_BO,ORIG_YY)]
Acquisitions_Data.bac.all[,table(FTHB_FLG,ORIG_YY)]
Acquisitions_Data.bac.all[,table(PURPOSE,ORIG_YY)]
Acquisitions_Data.bac.all[,table(PROP_TYP,ORIG_YY)]
Acquisitions_Data.bac.all[,table(NUM_UNIT,ORIG_YY)]
Acquisitions_Data.bac.all[,table(OCC_STAT,ORIG_YY)]
Acquisitions_Data.bac.all[,table(STATE,ORIG_YY)]

## tabulate NA totals for quasi-numeric variables by origination year
Acquisitions_Data.bac.all[,table(is.na(ORIG_RT))]
Acquisitions_Data.bac.all[,table(is.na(ORIG_AMT))]
Acquisitions_Data.bac.all[,table(is.na(OLTV))]
Acquisitions_Data.bac.all[,table(is.na(DTI))]
Acquisitions_Data.bac.all[,table(is.na(CSCORE_B))]
Acquisitions_Data.bac.all[,table(is.na(CSCORE_C))]
Acquisitions_Data.bac.all[,table(NUM_BO, is.na(CSCORE_C))]
Acquisitions_Data.bac.all[,table(is.na(ZIP_3))]



v

###################
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





Tmp_CUR         <- Ref_CUR[,.(DEF1Y, CSCORE_B)]
Tmp_CUR         <- Tmp_CUR[,c("CSCORE_B_NA","CSCORE_B_CL") := list( 1*is.na(CSCORE_B), ifelse(is.na(CSCORE_B),0,CSCORE_B)) ]

Tmp_CUR.CL      <- Tmp_CUR[!is.na(CSCORE_B),]
Tmp_CUR.CL.qnt  <- quantile(Tmp_CUR.CL$CSCORE_B, seq(0,1,length.out=20))

Tmp_CUR.CL[ , c("CSCORE_B_CL_BKT") := list(as.character(cut(CSCORE_B, breaks = c(-Inf, Tmp_CUR.CL.qnt[2:(length(Tmp_CUR.CL.qnt)-1)], Inf), right = TRUE, ordered = TRUE)))]



Tmp_CUR.NA.tbl <- Tmp_CUR[, list(DefRate=sum(DEF1Y)/length(DEF1Y)), by="CSCORE_B_NA"]
Tmp_CUR.CL.tbl <- Tmp_CUR.CL[, list(DefRate=sum(DEF1Y)/length(DEF1Y)), by="CSCORE_B_CL_BKT"]
Tmp_CUR.CL.tbl[order(CSCORE_B_CL_BKT),]







## sample loans to test 909003862264 955327563521 965094576864 428777648980 556466369444 855312777175
## 661507292010 140228957968 451446939570 582784456268 822252678256 854178607413
## 999936260493 999951495094 999956463090 999957116005 999991571801 346458094285 defaults




## insensitive
a <- glm(DEF1Y ~ ns(CSCORE_MN_CL,df=3), data=Ref_DLQ, family="binomial")
Ref_DLQ$pred    <- predict(a,data=Ref_DLQ,type="response")
b <- Ref_DLQ[order(Monthly.Rpt.Prd),list(pred=sum(pred*wt)/sum(wt), act=sum(DEF1Y*wt)/sum(wt)), by="Monthly.Rpt.Prd"]
plot(b[,.(Monthly.Rpt.Prd, act)], typ="b"); points(b[,.(Monthly.Rpt.Prd, pred)], typ="b",col="red")


## weak
a <- glm(DEF1Y ~ CLTV + CLTV*CLTV, data=Ref_DLQ, family="binomial")
Ref_DLQ$pred    <- predict(a,data=Ref_DLQ,type="response")
b <- Ref_DLQ[order(Monthly.Rpt.Prd),list(pred=sum(pred*wt)/sum(wt), act=sum(DEF1Y*wt)/sum(wt)), by="Monthly.Rpt.Prd"]
plot(b[,.(Monthly.Rpt.Prd, act)], typ="b"); points(b[,.(Monthly.Rpt.Prd, pred)], typ="b",col="red")

##good
a <- glm(DEF1Y ~ HPI_DEL_1Y + HPI_DEL_1Y*HPI_DEL_1Y, data=Ref_DLQ, family="binomial")
Ref_DLQ$pred    <- predict(a,data=Ref_DLQ,type="response")
b <- Ref_DLQ[order(Monthly.Rpt.Prd),list(pred=sum(pred*wt)/sum(wt), act=sum(DEF1Y*wt)/sum(wt)), by="Monthly.Rpt.Prd"]
plot(b[,.(Monthly.Rpt.Prd, act)], typ="b"); points(b[,.(Monthly.Rpt.Prd, pred)], typ="b",col="red")

## good
a <- glm(DEF1Y ~ HPI_DEL_2Y + HPI_DEL_2Y*HPI_DEL_2Y, data=Ref_DLQ, family="binomial")
Ref_DLQ$pred    <- predict(a,data=Ref_DLQ,type="response")
b <- Ref_DLQ[order(Monthly.Rpt.Prd),list(pred=sum(pred*wt)/sum(wt), act=sum(DEF1Y*wt)/sum(wt)), by="Monthly.Rpt.Prd"]
plot(b[,.(Monthly.Rpt.Prd, act)], typ="b"); points(b[,.(Monthly.Rpt.Prd, pred)], typ="b",col="red")

#a <- glm(DEF1Y ~ HPI_DEL_ORIG + HPI_DEL_ORIG*HPI_DEL_ORIG, data=Ref_DLQ, family="binomial")
#Ref_DLQ$pred    <- predict(a,data=Ref_DLQ,type="response")
#b <- Ref_DLQ[order(Monthly.Rpt.Prd),list(pred=sum(pred*wt)/sum(wt), act=sum(DEF1Y*wt)/sum(wt)), by="Monthly.Rpt.Prd"]
#plot(b[,.(Monthly.Rpt.Prd, act)], typ="b"); points(b[,.(Monthly.Rpt.Prd, pred)], typ="b",col="red")

## good
a <- glm(DEF1Y ~ UE_DEL_2Y + UE_DEL_1Y*UE_DEL_1Y, data=Ref_DLQ, family="binomial")
Ref_DLQ$pred    <- predict(a,data=Ref_DLQ,type="response")
b <- Ref_DLQ[order(Monthly.Rpt.Prd),list(pred=sum(pred*wt)/sum(wt), act=sum(DEF1Y*wt)/sum(wt)), by="Monthly.Rpt.Prd"]
plot(b[,.(Monthly.Rpt.Prd, act)], typ="b"); points(b[,.(Monthly.Rpt.Prd, pred)], typ="b",col="red")

## good
a <- glm(DEF1Y ~ UE_DEL_2Y + UE_DEL_2Y*UE_DEL_2Y, data=Ref_DLQ, family="binomial")
Ref_DLQ$pred    <- predict(a,data=Ref_DLQ,type="response")
b <- Ref_DLQ[order(Monthly.Rpt.Prd),list(pred=sum(pred*wt)/sum(wt), act=sum(DEF1Y*wt)/sum(wt)), by="Monthly.Rpt.Prd"]
plot(b[,.(Monthly.Rpt.Prd, act)], typ="b"); points(b[,.(Monthly.Rpt.Prd, pred)], typ="b",col="red")


## good
a <- glm(DEF1Y ~ UE_DEL_ORIG + UE_DEL_ORIG*UE_DEL_ORIG, data=Ref_DLQ, family="binomial")
Ref_DLQ$pred    <- predict(a,data=Ref_DLQ,type="response")
b <- Ref_DLQ[order(Monthly.Rpt.Prd),list(pred=sum(pred*wt)/sum(wt), act=sum(DEF1Y*wt)/sum(wt)), by="Monthly.Rpt.Prd"]
plot(b[,.(Monthly.Rpt.Prd, act)], typ="b"); points(b[,.(Monthly.Rpt.Prd, pred)], typ="b",col="red")


## insensitive
a <- glm(DEF1Y ~ DTI_CL + DTI_NA, data=Ref_DLQ, family="binomial")
Ref_DLQ$pred    <- predict(a,data=Ref_DLQ,type="response")
b <- Ref_DLQ[order(Monthly.Rpt.Prd),list(pred=sum(pred*wt)/sum(wt), act=sum(DEF1Y*wt)/sum(wt)), by="Monthly.Rpt.Prd"]
plot(b[,.(Monthly.Rpt.Prd, act)], typ="b"); points(b[,.(Monthly.Rpt.Prd, pred)], typ="b",col="red")


#a <- glm(DEF1Y ~ HPI_DEL_ORIG + HPI_DEL_ORIG*HPI_DEL_ORIG, data=Ref_DLQ, family="binomial")
#Ref_DLQ$pred    <- predict(a,data=Ref_DLQ,type="response")
#b <- Ref_DLQ[order(Monthly.Rpt.Prd),list(pred=sum(pred*wt)/sum(wt), act=sum(DEF1Y*wt)/sum(wt)), by="Monthly.Rpt.Prd"]
#plot(b[,.(Monthly.Rpt.Prd, act)], typ="b"); points(b[,.(Monthly.Rpt.Prd, pred)], typ="b",col="red")

##good HPI_DEL_2Y +
a <- glm(DEF1Y ~ ns(HPI_DEL_2Y,5) + ns(UE_DEL_2Y,df=5) + as.factor(LAST_STAT), data=Ref_DLQ, family="binomial")
Ref_DLQ$pred    <- predict(a,data=Ref_DLQ,type="response")
b <- Ref_DLQ[order(Monthly.Rpt.Prd),list(pred=sum(pred*wt)/sum(wt), act=sum(DEF1Y*wt)/sum(wt)), by="Monthly.Rpt.Prd"]
plot(b[,.(Monthly.Rpt.Prd, act)], typ="b"); points(b[,.(Monthly.Rpt.Prd, pred)], typ="b",col="red")
vif(a)


## good
#a <- glm(DEF1Y ~ ns(HPI_DEL_2Y,3) + ns(UE_DEL_2Y,df=3) + Loan.Age, data=Ref_CUR, family="binomial")
a <- glm(DEF1Y ~ HPI_DEL_2Y + UE_DEL_2Y + bs(Loan.Age,df=5), data=Ref_CUR, family="binomial")
Ref_CUR$pred    <- predict(a,data=Ref_CUR,type="response")
b <- Ref_CUR[order(Monthly.Rpt.Prd),list(pred=sum(pred)/length(pred), act=sum(DEF1Y)/length(DEF1Y)), by="Monthly.Rpt.Prd"]
plot(b[,.(Monthly.Rpt.Prd, act)], typ="b"); points(b[,.(Monthly.Rpt.Prd, pred)], typ="b",col="red")



####---------------------------------

## good
#a <- glm(DEF1Y ~ ns(HPI_DEL_2Y,3) + ns(UE_DEL_2Y,df=3) + Loan.Age, data=Ref_CUR, family="binomial")
#a <- glm(DEF1Y ~ HPI_DEL_1Y + UE_DEL_1Y + bs(Loan.Age,df=5), data=Ref_CUR, family="binomial")
eps = 1e-15;

a               <- glm(DEF1Y ~ bs(HPI_DEL_2Y,df=3) + bs(UE_DEL_2Y,df=3) + bs(Loan.Age,df=4) + bs(CLTV, df=3) + CSCORE_MN_CL, data=Ref_CUR, family="binomial") #
Ref_CUR$pred    <- predict(a,data=Ref_CUR,type="response")
Ref_CUR$pred    <- pmin(1-eps,pmax(eps, Ref_CUR$pred))
Ref_CUR[,c("logLoss") := ((DEF1Y*log(pred))+((1-DEF1Y)*log(1-pred)))]

b               <- Ref_CUR[order(Monthly.Rpt.Prd),list(pred=sum(pred)/length(pred), act=sum(DEF1Y)/length(DEF1Y), ll=sum(logLoss)/length(logLoss)), by="Monthly.Rpt.Prd"]
#plot(b[,.(Monthly.Rpt.Prd, act)], typ="b"); points(b[,.(Monthly.Rpt.Prd, pred)], typ="b",col="red")


#plot(b[,.(Monthly.Rpt.Prd, ll)], typ="b")
points(b[,.(Monthly.Rpt.Prd, ll)], typ="b", col="red")




## Loan.Age
tmp <- Ref_CUR[, list(act=sum(DEF1Y)/length(DEF1Y), pred=sum(pred)/length(pred)), by="Loan.Age"]
plot(tmp[,.(Loan.Age,act)])
lines(tmp[order(Loan.Age),.(Loan.Age,pred)], col="red")

##-8.620690  -4.301075   1.428571  16.000000
Ref_CUR[,c("UE_DEL_2Y_BKT") := cut(UE_DEL_2Y, breaks=c(-Inf,-10.638298,-8.620690,-6.666667,-4.301075,-2.040816,1.428571,6.012121,16.000000,33.333333,Inf), labels=seq(1,10))]
tmp <- Ref_CUR[order(UE_DEL_2Y_BKT), list(ue=mean(UE_DEL_2Y),act=sum(DEF1Y)/length(DEF1Y), pred=sum(pred)/length(pred)), by="UE_DEL_2Y_BKT"]
plot(tmp[,.(ue,act)],type="b")
points(tmp[order(ue),.(ue,pred)], col="red")

##-8.620690  -4.301075   1.428571  16.000000
Ref_CUR[,c("HPI_DEL_2Y_BKT") := cut(HPI_DEL_2Y, breaks=c(-Inf,-17.302785,-9.949897,-5.604887,-2.380038,1.071495,5.484765,9.377975,16.110892,28.346551,Inf), labels=seq(1,10))]
tmp <- Ref_CUR[order(HPI_DEL_2Y_BKT), list(hpi=mean(HPI_DEL_2Y),act=sum(DEF1Y)/length(DEF1Y), pred=sum(pred)/length(pred)), by="HPI_DEL_2Y_BKT"]
plot(tmp[,.(hpi,act)],type="b")
points(tmp[order(hpi),.(hpi,pred)], col="red")


##-8.620690  -4.301075   1.428571  16.000000 | Ref_CUR[,quantile(CLTV,seq(0,1,length.out=15))]
Ref_CUR[,c("CLTV_BKT") := cut(CLTV, breaks=c(-Inf,35.18768080,44.22726375,50.64621522,56.00634419,60.64272303,64.80311532,68.60209510,72.14908837,75.45936744,78.56235617,82.36169540,87.93932836,96.29069757,Inf), labels=seq(1,14))]
tmp <- Ref_CUR[order(CLTV_BKT), list(cltv=mean(CLTV),act=sum(DEF1Y)/length(DEF1Y), pred=sum(pred)/length(pred)), by="CLTV_BKT"]
plot(tmp[,.(cltv,act)],type="b")
points(tmp[order(cltv),.(cltv,pred)], col="red")



##-8.620690  -4.301075   1.428571  16.000000 | Ref_CUR[,quantile(CLTV,seq(0,1,length.out=15))]
Ref_CUR[,c("FICO_BKT") := cut(CSCORE_MN_CL, breaks=c(-Inf,631,651,667,681,694,708,722,735,748,761,772,783,795,Inf), labels=seq(1,14))]
tmp <- Ref_CUR[order(FICO_BKT), list(FICO=mean(CSCORE_MN_CL),act=sum(DEF1Y)/length(DEF1Y), pred=sum(pred)/length(pred)), by="FICO_BKT"]
plot(tmp[,.(FICO,act)],type="b")
points(tmp[order(FICO),.(FICO,pred)], col="red")


Ref_CUR[,c("logLoss") := ((DEF1Y*log(pred))+((1-DEF1Y)*log(1-pred)))]

a       <- Ref_CUR$Loan.Age
sp.a    <- bs(a,df=4)
a.smp   <- seq(0,180,1)
sp.a.p  <- predict(sp.a,a.smp)
plot(sp.a.p[,1]~a.smp, ylim=c(0,max(sp.a.p)), type='l', lwd=2, col=1, xlab="Cubic ns() basis", ylab="")
for (j in 2:ncol(sp.a.p)) lines(sp.a.p[,j]~a.smp, lwd=2, col=j)




MultiLogLoss <- function(act, pred)
{
    eps = 1e-15;
    nr <- nrow(pred)
    pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)
    pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
    ll = sum(act*log(pred) + (1-act)*log(1-pred))
    ll = ll * -1/(nrow(act))
    return(ll);
}


##-----------------------------------------------------------------------
## clean and add variables
##-----------------------------------------------------------------------

## Convert character variables to Date type
Data_A$ORIG_DTE <- as.Date(ifelse(Data_A$ORIG_DTE == "", "", paste0(substr(Data_A$ORIG_DTE,1,2),"/01/",substr(Data_A$ORIG_DTE,4,8))), "%m/%d/%Y")   ## origination date
Data_A$FRST_DTE <- as.Date(ifelse(Data_A$FRST_DTE == "", "", paste0(substr(Data_A$FRST_DTE,1,2),"/01/",substr(Data_A$FRST_DTE,4,8))), "%m/%d/%Y")   ## origination date

## Create buckets for continuous attributes and group number of borrowers
Data_A[,c("OcltvBkt", "OltvBkt", "FicoBkt", "DtiBkt", "OrigAmtBkt", "NumBoBkt") :=list(
as.character(cut(OCLTV, breaks = c(-Inf, 0, 60, 65, 70, 75, 80, 85, 90, 97, Inf),labels = c('NA', '(0-60]', '(60-65]', '(65-70]', '(70-75]', '(75-80]', '(80-85]', '(85-90]', '(90-97]', '(97+)'), right = TRUE, ordered = TRUE)),
as.character(cut(OLTV, breaks = c(-Inf, 0, 60, 65, 70, 75, 80, 85, 90, 97, Inf),labels = c('NA', '(0-60]', '(60-65]', '(65-70]', '(70-75]', '(75-80]', '(80-85]', '(85-90]', '(90-97]', '(97+)'), right = TRUE, ordered = TRUE)),
as.character(cut(CSCORE_MN, breaks = c(-Inf, 0, 620, 660, 700, 740, 780, Inf), labels = c('NA','[0-620)', '[620-660)', '[660-700)', '[700-740)', '[740-780)', '[780+)'), right = FALSE, ordered = TRUE)),
as.character(cut(DTI, breaks = c(-Inf, 0, 20, 30, 40, 45, Inf), labels = c('NA', '[0-20)', '[20-30)', '[30-40)', '[40-45)', '[45+)'), right = FALSE, ordered = TRUE)),
as.character(cut(ORIG_AMT, breaks = c(-Inf, 0, 85000, 110000, 125000, 150000, 175000, 200000, 417000, Inf), labels = c('NA', '[0-85k)', '[85k-110k)', '[110k-125k)', '[125k-1500k)', '[150k-175k)', '[175k-200k)', '[200k-417k)', '[417k+)'), right = FALSE, ordered = TRUE)),
as.character(as.character(ifelse(NUM_BO=="","Missing", ifelse(!(NUM_BO %chin% c("1","2")), "3+", NUM_BO))))
)]


##-----------------------------------------------------------------------
## generate continuous variable IV plots
##-----------------------------------------------------------------------
tmp.fico    <- csiPlot(Data_A, filtvar="CSCORE_MN", bktvar="FicoBkt", datevar="ORIG_DTE")
tmp.ocltv   <- csiPlot(Data_A, filtvar="OCLTV", bktvar="OcltvBkt", datevar="ORIG_DTE")
tmp.dti     <- csiPlot(Data_A, filtvar="DTI", bktvar="DtiBkt", datevar="ORIG_DTE")
tmp.bal     <- csiPlot(Data_A, filtvar="ORIG_AMT", bktvar="OrigAmtBkt", datevar="ORIG_DTE")
tmp.state   <- csiPlot(Data_A[!which(STATE %chin% c("GU","VI")),], filtvar="STATE", bktvar="STATE", datevar="ORIG_DTE")

tmp.purp   <- csiPlot(Data_A, filtvar="PURPOSE",  bktvar="PURPOSE",  datevar="ORIG_DTE")
tmp.prop   <- csiPlot(Data_A, filtvar="PROP_TYP", bktvar="PROP_TYP", datevar="ORIG_DTE")
tmp.fthb   <- csiPlot(Data_A, filtvar="FTHB_FLG", bktvar="FTHB_FLG", datevar="ORIG_DTE")
tmp.occ    <- csiPlot(Data_A, filtvar="OCC_STAT", bktvar="OCC_STAT", datevar="ORIG_DTE")
tmp.chan   <- csiPlot(Data_A, filtvar="ORIG_CHN", bktvar="ORIG_CHN", datevar="ORIG_DTE")
## ORIG_RT


#multiplot(lp.fico, lp.ocltv, lp.dti)



fit<-logistf(DEF1Y ~ CLTV, data=Ref_CUR)

v

###################
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




##-----------------------------------------------------------------------
## <function> :: csiPlot
##-----------------------------------------------------------------------
csiPlot  <- function(dt, filtvar, bktvar, datevar="ORIG_DTE")
{
    miss.pct <- 100*dt[is.na(eval(parse(text=filtvar))),table(eval(parse(text=datevar)))]/nrow(dt)   ## small
    
    ## bucket count/proportions by date
    a           <- dt[!is.na(eval(parse(text=filtvar))),table(eval(parse(text=datevar)), eval(parse(text=bktvar)))]
    b           <- 100*(a/rowSums(a))
    
    ## compute global means
    c           <- 0*b
    tmp.c       <- dt[!is.na(eval(parse(text=filtvar))),table(eval(parse(text=bktvar)))]
    tmp.c       <- tmp.c / sum(tmp.c)
    for (i in 1:nrow(c)) { c[i,] <- 100*tmp.c }
    
    ## compute WoE & IV
    d           <- (b-c)*log(b/c)
    e           <- ifelse(is.infinite(d),NA,d)
    f           <- apply(e, 1, sum, na.rm=TRUE)
    
    ## append additional info
    g           <- as.vector(dt[!is.na(eval(parse(text=filtvar))),table(eval(parse(text=datevar)))])
    h           <- (b-c)
    colnames(h) <- paste0("d",colnames(h))
    
    ## tabulate results
    tbl         <- as.data.table(as.data.frame(cbind(b,h,iv=f,cnt=g)))
    tbl$date    <- as.Date(rownames(b))
    lp          <- ggplot(data=tbl[which(date >= "2000-01-01")], aes(x=date, y=iv)) + geom_line() + geom_point()
    
    return(list(miss=miss.pct, tbl=tbl, lp=lp))
}




ggplot(data=Tmp_CUR, aes(x=DEF1Y, y=CLTV, fill=DEF1Y)) + geom_boxplot() + guides(fill=FALSE) + coord_flip()


ggplot(data=Tmp_CUR, aes(x=DEF1Y, y=log(FIN_UPB), fill=DEF1Y)) + geom_boxplot() + guides(fill=FALSE) + coord_flip()


ggplot(data=Tmp_CUR, aes(x=DEF1Y, y=LAST_STAT, fill=DEF1Y)) + geom_boxplot() + guides(fill=FALSE) + coord_flip()




## extract a sample
smp     <- 10



compareDefrateCon(Ref_CUR,"CLTV")


ggplot(Tmp_CUR, aes(x=DEF1Y, y=ORIG_RT, fill=DEF1Y)) + geom_boxplot() + guides(fill=FALSE) + coord_flip()





