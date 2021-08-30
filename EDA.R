getwd()
setwd()

install.packages("car")
install.packages("RcmdrMisc")
install.packages("sandwich")
install.packages("data.table")
install.packages("gtools")


library(car)
library(sandwich)
library(RcmdrMisc)
library(data.table)
library(gtools)

options(scipen = 999)

df <- cars


Numerical_var <-
  defmacro(df, var1, 'var2', expr = {
    
    ## Total Percentile
    smry1 <- numSummary(df[, c(var2)],
                        statistics = c("mean", "sd", "IQR", "quantiles", "skewness", "kurtosis"),
                        quantiles = c(0, .01, .05, .1, .2, .25, .3, .4, .5, .6, .7, .75, .8, .9, .95, .99, 1))
    smry2 <- round(as.data.frame(as.matrix(smry1$table)), digits = 1)
    smry3 <- as.data.frame(as.character(smry2[, 1:22]))
    smry4 <- as.data.frame(n <- c('mean', 'sd', 'IQR', 'skewness', 'kurtosis',
                                  '0%', '1%', '5%', '10%', '20%', '25%', '30%', '40%', '50%', '60%',
                                  '70%', '75%', '80%', '90%', '95%', '99%', '100%'))
    smry5 <- cbind(smry4, smry3)
    
    colnames(smry5) <- c("Item", "Total")
    
    n <- as.character(round(as.data.frame(as.matrix(smry1$n)), digits = 0))
    nmiss <- as.character(round(sapply(df[var2], function(x)(sum(is.na(x)))),
                                digits = 0)) # NA counts
    n_smry <- cbind(c('n', 'nmiss'), rbind(n, nmiss))
    
    colnames(n_smry) <- c("Item", "Total")
    rownames(n_smry) <- c(1, 2)
    num.smry <- rbind(n_smry, smry5)
  })
    
Numerical_var(df, speed, 'speed')   
num.smry


df_c <- Prestige

Categorical_var <- 
  def(dt, id, var1, 'var2', expr = {
    
    C.smry1 <- dt[order(var1), list(Total = length)]
    
  })

rbind(as.data.frame(summary(df_c$type)), Total = length(df_c$type))

table(df_c$type)
prop.table(table(df_c$type))

data.frame(round(rbind(table(df_c$type), prop.table(table(df_c$type))*100)))
    
    
```{r, echo = FALSE, message = FALSE, warning = FALSE, fig.height = 6, fig.width = 12}
knitr::kable(smry, format = 'markdown')
```

  