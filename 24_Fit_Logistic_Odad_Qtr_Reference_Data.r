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

##-----------------------------------------------------------------------
## load the reference data
##-----------------------------------------------------------------------

## load the CUR file
file.in     <- dir(path=".", pattern="Q1_Combined_Data_BAC_ODAD_NonOverlap_Qtr_CUR.Rda")
load(file.in)

## load the DLQ file
file.in     <- dir(path=".", pattern="Q1_Combined_Data_BAC_ODAD_NonOverlap_Qtr_DLQ.Rda")
load(file.in)

##-----------------------------------------------------------------------
## encode the 1Y default indicator as categorical
##-----------------------------------------------------------------------
Ref_CUR$DEF1Y   <- as.factor(Ref_CUR$DEF1Y)
Ref_DLQ$DEF1Y   <- as.factor(Ref_DLQ$DEF1Y)


##-----------------------------------------------------------------------
## define missing dummies
##-----------------------------------------------------------------------

## CUR
Ref_CUR$CSCORE_B_CL <- ifelse(is.na(Ref_CUR$CSCORE_B),0,Ref_CUR$CSCORE_B)
Ref_CUR$CSCORE_B_NA <- ifelse(is.na(Ref_CUR$CSCORE_B),1,0)
Ref_CUR$CSCORE_C_CL <- ifelse(is.na(Ref_CUR$CSCORE_C),0,Ref_CUR$CSCORE_C)
Ref_CUR$CSCORE_C_NA <- ifelse(is.na(Ref_CUR$CSCORE_C),1,0)
Ref_CUR$DTI_CL <- ifelse(is.na(Ref_CUR$DTI),0,Ref_CUR$DTI)
Ref_CUR$DTI_NA <- ifelse(is.na(Ref_CUR$DTI),1,0)

## DLQ
Ref_DLQ$CSCORE_B_CL <- ifelse(is.na(Ref_DLQ$CSCORE_B),0,Ref_DLQ$CSCORE_B)
Ref_DLQ$CSCORE_B_NA <- ifelse(is.na(Ref_DLQ$CSCORE_B),1,0)
Ref_DLQ$CSCORE_C_CL <- ifelse(is.na(Ref_DLQ$CSCORE_C),0,Ref_DLQ$CSCORE_C)
Ref_DLQ$CSCORE_C_NA <- ifelse(is.na(Ref_DLQ$CSCORE_C),1,0)
Ref_DLQ$DTI_CL <- ifelse(is.na(Ref_DLQ$DTI),0,Ref_DLQ$DTI)
Ref_DLQ$DTI_NA <- ifelse(is.na(Ref_DLQ$DTI),1,0)


##-----------------------------------------------------------------------
## variable units
##-----------------------------------------------------------------------
cur.vars <- c(
            ##"bs(CSCORE_B_CL, df=3) + as.factor(CSCORE_B_NA)", ## dropped in favor of a linear predictor
            "CSCORE_B_CL + as.factor(CSCORE_B_NA)",
            #"bs(CSCORE_C_CL, df=3) + as.factor(CSCORE_C_NA)", ## removed due to lack of significance
            "bs(DTI_CL, df=3) + as.factor(DTI_NA)",
            "bs(Loan.Age,df=4)",
            "bs(UE_DEL_ORIG, df=3)",
            #"bs(HPI_DEL_1Y, df=3)", ## testing a single HPI factor
            "bs(HPI_DEL_2Y, df=3)",
            #"bs(HPI_DEL_3Y, df=3)", ## testing a single HPI factor
            #"as.factor(LAST_STAT)", ## removed: no variation in CUR
            "as.factor(PROP_TYP)",
            "as.factor(PURPOSE)",
            "as.factor(IS_MOD)",
            "as.factor(NUM_BO_BKT)",
            "as.factor(NUM_UNIT_BKT)",
            "as.factor(ORIG_RT_BKT)"
        )

dlq.vars <- c(
            ##"bs(CSCORE_B_CL, df=3) + as.factor(CSCORE_B_NA)", ## dropped in favor of a linear predictor
            "CSCORE_B_CL + as.factor(CSCORE_B_NA)",
            #"bs(CSCORE_C_CL, df=3) + as.factor(CSCORE_C_NA)", ## removed due to lack of significance
            "bs(DTI_CL, df=3) + as.factor(DTI_NA)",
            "bs(Loan.Age,df=4)",
            "bs(UE_DEL_ORIG, df=3)",
            #"bs(HPI_DEL_1Y, df=3)",  ## testing a single HPI factor
            "bs(HPI_DEL_2Y, df=3)",
            #"bs(HPI_DEL_3Y, df=3)",  ## testing a single HPI factor
            "as.factor(LAST_STAT)",
            "as.factor(PROP_TYP)",
            "as.factor(PURPOSE)",
            "as.factor(IS_MOD)",
            "as.factor(NUM_BO_BKT)",
            "as.factor(NUM_UNIT_BKT)",
            "as.factor(ORIG_RT_BKT)"
            )

##-----------------------------------------------------------------------
## set-up
##-----------------------------------------------------------------------

cur.folds   <- createFolds(Ref_CUR$DEF1Y, k = 5, list = TRUE, returnTrain=FALSE)
num.folds   <- length(cur.folds)

glob.res    <- list()
glob.stat   <- list()

cur.formula <- c("DEF1Y ~ ")
for (i in 1:length(cur.vars))
{
    if (i == 1) {
        cur.formula <- paste0(cur.formula, cur.vars[i])
    } else {
        cur.formula <- paste0(cur.formula, " + ", cur.vars[i])
    }
}


for (i in 1:1)
{
    tmp.fold    <- as.vector(cur.folds[[i]])
    tmp.train   <- Ref_CUR[ -tmp.fold, ]
    tmp.test    <- Ref_CUR[  tmp.fold, ]
    
    tmp.fit     <- glm(cur.formula, data=tmp.train, family="binomial")
    
    ## in-sample results
    tmp.train$act       <- ifelse(tmp.train$DEF1Y==1,1,0)
    tmp.train$pred      <- predict(tmp.fit, newdata=tmp.train, type="response")
    tmp.train[, c("brier","logloss") := list(((pred - act)^2), (log(pred)*act + log(1-pred)*(1-act))) ]
    pred.train          <- prediction(tmp.train$pred, tmp.train$act)
    perf.train          <- performance(pred.train, measure = "tpr", x.measure = "fpr")

    ## out-of-sample results
    tmp.test$act       <- ifelse(tmp.test$DEF1Y==1,1,0)
    tmp.test$pred      <- predict(tmp.fit, newdata=tmp.test, type="response")
    tmp.test[, c("brier","logloss") := list(((pred - act)^2), (log(pred)*act + log(1-pred)*(1-act))) ]
    pred.test          <- prediction(tmp.test$pred, tmp.test$act)
    perf.test          <- performance(pred.test, measure = "tpr", x.measure = "fpr")

    ## compute global results
    glob.res[[i]]    <- list(
                res=data.frame(
                    train.n = nrow(tmp.train),
                    train.auc = performance(pred.train, measure = "auc")@y.values[[1]],
                    train.br = tmp.train[,sum(brier)/length(brier)],
                    train.ll = tmp.train[,sum(logloss)/length(logloss)],
                    train.df = extractAIC(tmp.fit)[1],
                    train.aic = extractAIC(tmp.fit)[2],
                    test.n = nrow(tmp.test),
                    test.auc = performance(pred.test, measure = "auc")@y.values[[1]],
                    test.br = tmp.test[,sum(brier)/length(brier)],
                    test.ll = tmp.test[,sum(logloss)/length(logloss)],
                    test.aic = NA,
                    test.df = NA
                )
            )

    glob.stat[[i]] <- tmp.train[order(Monthly.Rpt.Prd),list(pred=sum(pred)/length(pred), act=sum(act)/length(act), ll=sum(logloss)/length(logloss), br=sum(brier)/length(brier)), by="Monthly.Rpt.Prd"]
    
}


## plot the results as a function of the risk drivers
## loop over individual variables separately and plot results (vintile plots)
## clean-up so you can do this for delinqueny variables
## generate all the plots in here

## optimize the fit using log-loss

