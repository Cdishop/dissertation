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

f1issues <- readRDS("../orgs/facebook/1/issues.rds")
f2issues <- readRDS("../orgs/facebook/2/issues.rds")
f3issues <- readRDS("../orgs/facebook/3/issues.rds")


f1 <- create_series(f1issues)
f2 <- create_series(f2issues)
f3 <- create_series(f3issues)

facebook <- bind_rows(f1, f2, f3)

uniquefacebook <- facebook %>% 
    distinct(repo)

uniquefacebook <- uniquefacebook %>% 
  mutate(repoid = 1:nrow(uniquefacebook)) %>% 
  mutate(nameit = "GitHub Repo:") %>% 
  arrange(repoid) %>% 
  unite("repoidname", nameit, repoid, sep = " ", remove = F) %>% 
  select(repoidname, repo, repoid)

facebook <- left_join(facebook, uniquefacebook) %>% 
  mutate(org = "facebook")





################## github

git1issues <- readRDS("../orgs/github/1/issues.rds")
git2issues <- readRDS("../orgs/github/2/issues.rds")
git3issues <- readRDS("../orgs/github/3/issues.rds")
git4issues <- readRDS("../orgs/github/4/issues.rds")



git1 <- create_series(git1issues)
git2 <- create_series(git2issues)
git3 <- create_series(git3issues)
git4 <- create_series(git4issues)

github <- bind_rows(git1, git2, git3, git4)

uniquegithub <- github %>% 
  distinct(repo)

uniquegithub <- uniquegithub %>% 
  mutate(repoid = 1:nrow(uniquegithub)) %>% 
  mutate(nameit = "GitHub Repo:") %>% 
  arrange(repoid) %>% 
  unite("repoidname", nameit, repoid, sep = " ", remove = F) %>% 
  select(repoidname, repo, repoid)

github <- left_join(github, uniquegithub) %>% 
  mutate(org = "github")




################## google

all_series <- function(number, organization){
  
  final <- list()
  for(i in 1:number){
    
    iss <- readRDS(paste0("../orgs/", organization, "/", i, "/", "issues.rds"))
    orgiss <- create_series(iss)
    
    final[[i]] <- orgiss
  }
  
  return(final)
  
}

googseries <- all_series(7, "google")
google <- bind_rows(googseries)


labelit <- function(df, title){
  
  uniq <- df %>% 
    distinct(repo)
  
  uniq <- uniq %>% 
    mutate(repoid = 1:nrow(uniq)) %>% 
    mutate(nameit = "GitHub Repo:") %>% 
    arrange(repoid) %>% 
    unite("repoidname", nameit, repoid, sep = " ", remove = F) %>% 
    select(repoidname, repo, repoid)
  
  out <- left_join(df, uniq) %>% 
    mutate(org = title)
  
  return(out)
}

google <- labelit(google, "google")



################## Microsoft

microseries <- all_series(9, "microsoft")
microsoft <- bind_rows(microseries)
microsoft <- labelit(microsoft, "microsoft")



################## final df

finaldf <- bind_rows(facebook, github)
finaldf <- bind_rows(finaldf, google)
finaldf <- bind_rows(finaldf, microsoft)

######################## save

save(finaldf, file = "../output/daily_issues.RData")
write.csv(finaldf, file = "../output/daily_issues.csv")

