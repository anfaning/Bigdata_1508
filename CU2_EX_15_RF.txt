 
---
title: "WAVE2_CU2_EX15_RF: Exsting customer contract predictive model"
output: word_document
---
```{r, echo = FALSE, warning = FALSE, message = FALSE, results = 'hide'}
## ==================================================================
## 0. Loading packages
## ==================================================================
rm(list=ls())
library(data.table)
library(DMwR)
library(Epi)
library(ggplot2)
library(hliR)
library(logistf)
library(party)
library(randomForest)
library(ROCR)
library(SDMTools)
oraConnect()
## ==================================================================
## 1. Data prep
## ==================================================================
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 1.1. Loading the data
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CU.TRN <- oraSql("SELECT *
                        FROM HAMADMIN.AB_CU2_NCT_EX_TRN15_DS")
CU.TST <- oraSql("SELECT *
                        FROM HAMADMIN.AB_CU2_NCT_EX_TST15_DS")
CU.TRN$TGT <- as.factor(CU.TRN$TGT15)
CU.TST$TGT <- as.factor(CU.TST$TGT15)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 1.2. Data transition
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class              <- sapply(CU.TRN, class)
char.list          <- subset(class, class == "character")
char.name          <- names(char.list)
coldelete          <- c("POLY_NO",  "CTOR_CUST_ID","CLLT_FP_PRNO", "CNTT_DATE", "CNTT_YM")
char.name1         <- char.name[! char.name %in% coldelete]
CU.TRN[, char.name1] <- lapply(char.name1, function(x){
  lev <- unique(c(levels(factor(CU.TRN[, x])),
                    levels(factor(CU.TST[, x])) ))
  lev.ord <- lev[order(lev)]
  factor(CU.TRN[, x], levels = lev.ord)
  })
CU.TST[, char.name1] <- lapply(char.name1, function(x){
  lev <- unique(c(levels(factor(CU.TRN[, x])),
                    levels(factor(CU.TST[, x])) ))
  lev.ord <- lev[order(lev)]
  factor(CU.TST[, x], levels = lev.ord)
  })
out_var   <- c("RM_PAYM_CNT_SUM", "RM_PAYM_CNT_AVG", "RM_PAYM_CNT_MAX", "RM_PAYM_CNT_MIN",
               "AVG_PAYM_CNT", "CLLT_CVMF_CNTT", "ARRE_AVG_AMT_1Y", "PAYM_AMT", "PUBL_SHTS",
               "TRNF_FL_PMCNT_1Y", "AVG_TRNF_FL_AMT_1Y", "SMPD_SLL_EXP", "CY_MINCM",
               "VAL_PLCNT", "VAL_PLCNT_COLL")
out_value <- c(2788, 828, 940, 672, 143, 3530252, 464009, 2359080,
               207, 32, 443008, 281, 14000000, 13, 2157)
out <- data.table(out_var, out_value = as.numeric(out_value))
for (colname in out$out_var) {
  CU.TRN[, colname] <- ifelse(CU.TRN[, colname] >= out[out$out_var == colname, out_value]
                              , out[out$out_var == colname, out_value]
                              , CU.TRN[, colname])
}

for (colname in out$out_var) {
  CU.TST[, colname] <- ifelse(CU.TST[, colname] >= out[out$out_var == colname, out_value]
                              , out[out$out_var == colname, out_value]
                              , CU.TST[, colname])
}
 
# # str(CU.TRN)
# sum(is.na(CU.TRN))
# a <- sapply(CU.TRN, function(x){sum(is.na(x))})
# a[a>0]
```
## Random Forest Model
### Target: Customer chung model
#### 2. Model
```{r, echo = FALSE, warning = FALSE, message = FALSE}
set.seed(100)
options(contrasts = c("contr.treatment", "contr.poly"))
RF_OBJ<- randomForest(TGT ~ CTOR_AGE + MACRM_CLSF_CDNM + SRDO_LDDT + SRDO_RECP_TIME_GR + CNTT_WKSQ + FTPR_RECV_MDCD_82_YN + MINS_CUST_REL + RM_PAYM_CNT_MIN + FST_FP_CNTT_YN + LAPS_CNT_3Y + SRRD_CNT_3Y + AVG_PAYM_CNT + MFPAY_CNT_3Y + CLLT_CVMF_CNTT + CLLT_MMTH_CNTT + MNTN_PREM_MM13_GRAC + SP_YN + LST_CHLD_AGE_GR + ARRE_NPAYD_YN_1Y + PALN_YN_3Y + CRNT_LOAN_AMT + TS_NT_MNTN_CNT + TS_MNTN_CNT + CD_GRAD_GR + PL_MPREM_RATIO + PAYM_AMT + PUBL_DTCNT + PUBL_YN + CLLT_CHNG_YN_3Y + RGST_CNTT_MAFT + CNTT_INTV + SPCT_PCT_RNK + DS_MIN_MPREM_RATIO + MAX_MFRT_AFFRD_2Y + SMPD_SLL_EXP_GR + PREM_PCT_GR + VAL_PLCNT + VAL_PLCNT_COLL + CNTT_CLLT_AGE_DIFF_GR + CNTT_CLLT_SEX_DIFF_YN + CALL_CNT + ERC_0001 + XY_VAL_DIFF, 
                              data = CU.TRN, mtry = 6, ntree = 500, nodesize = 500, 
                              importance = TRUE, do.trace = 100
                             )
```
```{r, echo = FALSE, warning = FALSE, message = FALSE}
print(RF_OBJ$call)
```
##### 2.1 Variable Importance
```{r, echo = FALSE, warning = FALSE, message = FALSE, fig.height = 5, fig.width = 7}
a <- varImpPlot(RF_OBJ, cex = 0.7, main = "Variable Importance")
kable(a[order(-a[,"MeanDecreaseAccuracy"]),])

```
#### 3. Vaildation
##### 3.1. confusion matrix 
```{r, echo = FALSE, warning = FALSE, message = FALSE}
##train
print("Train data")
set.seed(100)
prd_trn  <- predict(RF_OBJ, newdata = CU.TRN)
cfml     <- table(prd_trn, CU.TRN$TGT)
col      <- cfml[, 1] + cfml[, 2]
cfml1    <- data.frame(cbind(cfml, col))
cfml2    <- sapply(cfml1[, 1:3], sum)
cfml3    <- as.matrix(rbind(cfml1, cfml2))
colnames(cfml3) <- c("0:False", "1:True", "Total")
rownames(cfml3) <- c("0:Predicted False", "1:Predicted True", "Total")
kable(cfml3)
accuracy  <- ((cfml[1, 1] + cfml[2, 2]) / nrow(CU.TRN)) * 100 
precision <- cfml[2, 2] / (cfml[2, 1] + cfml[2, 2]) * 100
recall    <- cfml[2, 2] / (cfml[1, 2] + cfml[2, 2]) * 100
f1        <- round((precision * recall) / (precision + recall), 4) * 2
kable(cbind(accuracy, precision, recall, f1))

##CU.TST$MINS_CUST_REL <- factor(CU.TST$MINS_CUST_REL, levels = RF_OBJ$forest$xlevels$MINS_CUST_REL)
##test
print("Test data")
set.seed(100)
prd_tst  <- predict(RF_OBJ, newdata = CU.TST)
cfml     <- table(prd_tst, CU.TST$TGT)
col      <- cfml[, 1] + cfml[, 2]
cfml1    <- data.frame(cbind(cfml, col))
cfml2    <- sapply(cfml1[, 1:3], sum)
cfml3    <- as.matrix(rbind(cfml1 ,cfml2))
colnames(cfml3) <- c("0: False", "1: True", "Total")
rownames(cfml3) <- c("0:Predicted False", "1:Predicted True", "Total")
kable(cfml3)
accuracy  <- ((cfml[1, 1] + cfml[2, 2]) / nrow(CU.TST)) * 100 
precision <- cfml[2, 2] / (cfml[2, 1] + cfml[2, 2]) * 100
recall    <- cfml[2, 2] / (cfml[1, 2] + cfml[2, 2]) * 100
f1        <- round((precision * recall) / (precision + recall), 4) * 2
kable(cbind(accuracy, precision, recall, f1))

```
##### 3.2. Density Plot
```{r, echo = FALSE, warning = FALSE, message = FALSE}
set.seed(100)
actually <- factor(CU.TST$TGT)
pred     <- predict(RF_OBJ, CU.TST, type = "prob")
dframe   <- data.frame(actually, predicted = pred)
CU.TST <- cbind(CU.TST, pred)
ggplot(dframe, aes(x=predicted.1, colour = actually)) + geom_density()
```

##### 3.3. Gain Chart
```{r, echo = FALSE, warning = FALSE, message = FALSE}
set.seed(100)
CU.TST1 <- CU.TST
CU.TST$pred <- predict(RF_OBJ, CU.TST, type = "prob")[, 2]
CU.TST      <- CU.TST[with(CU.TST, order(-CU.TST$pred)), ]
CU.TST$ID   <- seq.int(nrow(CU.TST))
CU.TST$Seg  <- with(CU.TST, cut(ID, breaks = quantile(ID, probs = seq(0, 1, by = 0.1)), 
                                            labels = FALSE, include.lowest = TRUE))
cu.tst.dt <- data.table(CU.TST)
gain.set <- cu.tst.dt[, list(min_score = round(min(pred), 4), 
                                 max_score = round(max(pred), 4), 
                                 cnt       = length(pred), 
                                 Resp      = sum(ifelse(TGT == '1', 1, 0))), 
                             by = Seg]
gain.set$Resp_rate <- round((gain.set$Resp / gain.set$cnt) * 100, 1)
gain.set <- within(gain.set, cum_Resp <- cumsum(Resp))
gain.set$cum_Resp_rate <- round((gain.set$cum_Resp / sum(gain.set$Resp)) * 100, 1)
gain.set$lift <- round((gain.set$Resp_rate / 100) / (sum(gain.set$Resp) / sum(gain.set$cnt)), 1)
print(gain.set, row.names = F)
ggplot(data = gain.set, aes(x = Seg, y = cum_Resp_rate, group = 1)) + 
  geom_line(colour = "blue", linetype = "solid", size = 1) +
  geom_point(colour = "blue", size = 5, shape = 21, fill = "white") + 
  labs(title = "Gain Chart", x = "% customer", y = "% responser", color = "Cylinders") +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  theme(axis.text = element_text(size = 14))
```
 
