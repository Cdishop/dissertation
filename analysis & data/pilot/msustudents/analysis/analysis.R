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
  "kpss" = numeric(runs),
  "degree_id" = numeric(runs),
  "timepoints" = numeric(runs)
)

for(i in 1:runs){
  
  dt <- df %>%
    filter(degree == degrees[i])
  
  dtdickey = plm.data(dt, index = c("file", "time"))
  
  dy <- adf.test(dtdickey[, "total"])["p.value"][[1]]
  kp <- kpss.test(dtdickey[, "total"])["p.value"][[1]]
  
  store_results[[i, "dickey"]] <- dy
  store_results[[i, "kpss"]] <- kp
  store_results[[i, "degree_id"]] <- dt$degree_id[1]
  store_results[[i, "timepoints"]] <- tail(dt$time, 1)
  
  
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


store_results <- store_results %>% 
  mutate(DP = ifelse(dickey > 0.05, "Yes", "No")) %>% 
  mutate(KP = ifelse(kpss < 0.05, "Yes", "No"))

df <- left_join(df, store_results)




# describe it in table

library(kableExtra)

df %>% 
  filter(time == 1) %>% 
  mutate(degreecode = c(1:length(degrees))) %>% 
  unite("Start", term, year, sep = "-") %>% 
  select(degreecode, Start, timepoints, DP, KP) %>% 
  rename("Degree ID" = degreecode,
         "Start Date" = Start,
         "Length of Series (Semesters)" = timepoints,
         "DF Unit Root" = DP,
         "KPSS Unit Root" = KP) %>% 
  kable() %>% 
  kable_styling() %>% 
  footnote("Dickey-Fuller Results: 77% of series contain unit root\nKPSS Results: 65% of series contain unit root")

