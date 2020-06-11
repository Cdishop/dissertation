library(tidyverse)
library(tseries)
library(plm)
df <- read.csv("../data/cleaned/df.csv")

# remove a few series after visual inspection (e.g., they canceled the program)

nouse <- c("College of Medicine-Anatomy",
           "College of Social Science-Psychology-Urban Studies",
           "College of Education-Educational Psychology",
           "College of Natural Science-Entomology")

df <- df %>%
  filter(!degree %in% nouse)

# practice once

degrees <- unique(df$degree)
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


(store_results %>%
  filter(dickey > 0.05, kpss < 0.05) %>%
  count()) / nrow(store_results)


(store_results %>%
  filter(dickey > 0.05) %>%
  count()) / nrow(store_results)

(store_results %>%
    filter(kpss < 0.05) %>%
    count()) / nrow(store_results)
