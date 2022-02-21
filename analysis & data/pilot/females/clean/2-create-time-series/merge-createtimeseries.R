# strict inclusion 2 means I don't include the inclusion criteria:
# 'the issue has at least one action by the owner'
# 'the issue is not a pull request'


## ----library ---------------------------------------------------------
# library(tidyverse)
library(ggplot2)
library(tidyr)
library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(lubridate)
library(glue)

## ---- githubIssueCollection -------------------------------------------
library(githubIssueCollection)


## within org merging function
source("series-function.R")


################## facebook

f1issues <- readRDS("../1-grab-repos/issues.rds")



f1 <- create_series(f1issues)

uniquef1 <- f1 %>% 
    distinct(repo)

uniquef1 <- uniquef1 %>% 
  mutate(repoid = 1:nrow(uniquef1)) %>% 
  mutate(nameit = "GitHub Repo:") %>% 
  arrange(repoid) %>% 
  unite("repoidname", nameit, repoid, sep = " ", remove = F) %>% 
  select(repoidname, repo, repoid)

alldf <- left_join(f1, uniquef1)


######################## save

save(alldf, file = "../3-output/daily_issues.RData")
write.csv(alldf, file = "../3-output/daily_issues.csv")

