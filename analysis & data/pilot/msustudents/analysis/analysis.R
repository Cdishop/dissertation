library(tidyverse)
library(tseries)
library(plm)
df <- read.csv("../data/cleaned/df.csv")
degrees <- unique(df$degree)




df1 <- df %>%
  filter(degree == degrees[18])
df1 <- df1 %>%
  mutate(time = 1:nrow(df1))


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
  dt <- dt %>%
    mutate(time = 1:nrow(dt))
  
  dtdickey = plm.data(dt, index = c("file", "time"))
  
  dy <- adf.test(dtdickey[, "total"])["p.value"][[1]]
  kp <- kpss.test(dtdickey[, "total"])["p.value"][[1]]
  
  store_results[[i, "dickey"]] <- dy
  store_results[[i, "kpss"]] <- kp
  
  
}


(store_results %>%
  filter(dickey > 0.05, kpss < 0.05) %>%
  count()) / nrow(store_results)


(store_results %>%
  filter(dickey > 0.05) %>%
  count()) / nrow(store_results)
