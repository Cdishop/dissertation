library(tidyverse)
library(tseries)
library(plm)
df <- read.csv("../data/cleaned/df.csv")
degrees <- unique(df$degree)


## use only degrees with > 20 time points
table(df$degree, df$time)


df1 <- df %>%
  filter(degree == degrees[18])

dfdickey <- plm.data(df1, index = c("file", "time"))

adf.test(dfdickey[, "total"])["p.value"][[1]]

kpss.test(dfdickey[, "total"])["p.value"][[1]]


runs <- length(degrees)
store_results <- data.frame(
  "dickey" = numeric(runs),
  "kpss" = numeric(runs)
)

for(i in 1:runs){
  
  dt <- df %>%
    filter(degree == degrees[i])
  
  dtdickey = plm.data(dt, index = c("file", "time"))
  
  dy <- adf.test(dtdickey[, "total"])["p.value"][[1]]
  kp <- kpss.test(dtdickey[, "total"])["p.value"][[1]]
  
  store_results[[i, "dickey"]] <- dy
  store_results[[i, "kpss"]] <- kp
  
  
}
