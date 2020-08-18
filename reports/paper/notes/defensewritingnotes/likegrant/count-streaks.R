library(tidyverse)
runs <- 10000


df_store <- data.frame(
  'lowvariance' = character(runs),
  'highvariance' = character(runs)
)


for(i in 1:runs){
  

lowv <- round(rnorm(10, 8.5, 0.5))
highv <- round(rnorm(10, 8.5, 2.7))

evensplit_lowv <- FALSE

# if the vector is evenly split, then make evensplit TRUE
# evenly split, meaning half the numbers are below 8.5 and half are above
# evenly split, meaning the vector isn't a streak of 10's
if(sum(lowv < 8.5) < 5 | sum(lowv < 8.5) > 5){
  evensplit_lowv <- TRUE
}

evensplit_highv <- FALSE
if(sum(lowv < 8.5) < 5 | sum(lowv < 8.5) > 5){
  evensplit_highv <- TRUE
}

df_store[[i, "lowvariance"]] <- evensplit_lowv
df_store[[i, "highvariance"]] <- evensplit_highv


}

df_store %>% 
  select(highvariance) %>% 
  filter(highvariance == "TRUE") %>% 
  count() / runs

# 25% of the time, you would get "streaky" performance, with numbers greater or lower than 8.5 dominating the sequence

df_store %>% 
  select(lowvariance) %>% 
  filter(lowvariance == "TRUE") %>% 
  count() / runs
