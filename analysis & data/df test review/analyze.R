library(tidyverse)
df <- read.csv("article-counts.csv")


df %>% filter(journal == 'jap') %>% count(journal)
df %>% count(journal)

df %>% filter(longitudinal == "yes") %>% 
  filter(steps > 2) %>% 
  count()

df %>% filter(longitudinal == "yes") %>% 
  filter(steps > 2) %>% 
  count(journal)

df %>% filter(longitudinal == "yes") %>% 
  filter(steps > 2) %>% 
  select(steps) %>% 
  arrange(steps)
