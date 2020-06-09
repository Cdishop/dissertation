
# steps
# 1) get files
# 2) for each file, load and clean, then combine
# 3) select only those with time-series > 20 periods
# 4) save

library(tidyverse)
library(readxl)
library(stringr)

# 1) get files

setwd("../data/working")
files <- Sys.glob("*.xlsx")

# 2) for each file, load and clean

## do it once first

file_name <- files[1]
df <- read_excel(file_name)

## cleaning

### select the columns I need and rename them
### begin on row 6

df <- df %>%
  select(starts_with("Michi"),
         "...2",
         "...3",
         "...12") %>%
  rename(degree = starts_with("Michi"),
         degree_id = "...2",
         degree_level = "...3",
         total = "...12") %>%
  slice(-c(1:5))

### select only phd students

df <- df %>%
  filter(degree_level == "PHD")

### the prior step also gets rid of total rows across colleges, cool

## new column to represent data set and time

df$file <- file_name

## to get the time I have to do a bit of string work

df$dirtytime <- df$file
df$dirtytime <- gsub(".xlsx", "", df$file)
df <- df %>%
  separate(dirtytime, into = c("term", "year"), 
           sep ="(?=[[:digit:]])",
           extra = "merge",
           fill = "right")


## keep only degrees with unique names 
## e.g., physio is in natural science and biology and osteo

df <- df %>%
  distinct(degree, .keep_all = T)




## repeat for every other file

for(i in 2:length(files)){
  
  file_name <- files[i]
  dfl <- read_excel(file_name)
  dfl <- dfl %>%
    select(starts_with("Michi"),
           "...2",
           "...3",
           "...12") %>%
    rename(degree = starts_with("Michi"),
           degree_id = "...2",
           degree_level = "...3",
           total = "...12") %>%
    slice(-c(1:5))
  
  dfl <- dfl %>%
    filter(degree_level == "PHD")
  dfl$file <- file_name
  dfl$dirtytime <- dfl$file
  dfl$dirtytime <- gsub(".xlsx", "", dfl$file)
  dfl <- dfl %>%
    separate(dirtytime, into = c("term", "year"), 
             sep ="(?=[[:digit:]])",
             extra = "merge",
             fill = "right")
  dfl <- dfl %>%
    distinct(degree, .keep_all = T)
  
  df <- bind_rows(df, dfl)
  
}

# 4) select only time-series with > 20 periods

### and a sequence (1,2,3,4,5; not 1,5,13)

df <- df %>%
  mutate(term_numeric = case_when(
    term == "fall" ~ 2,
    term == "summer" ~ 1,
    term == "spring" ~ 0
  )) %>%
  arrange(degree, year, term_numeric)

df$year <- as.numeric(df$year)

### select only those with multiple years

degrees_with_enough_years <- df %>%
  group_by(degree) %>%
  count() %>%
  filter(n > 20) %>%
  pull(degree)

df <- df %>%
  filter(degree %in% degrees_with_enough_years)


### select only those with sequence (1,2,3,4...)


degrees <- unique(df$degree)

save_degrees <- c()
for(deg in degrees){
  
  major <- df %>%
    filter(degree == deg)
  
  first_year <- major[1, "year"][[1]] 
  start_year <- first_year + 1
  last_year <- major[nrow(major), "year"][[1]]
  end_year <- last_year - 1
  
  use_years <- seq(from = start_year,
                   to = end_year, 
                   by = 1)
  
  save_seq <- c()
  for(k in 1:length(use_years)){
    
    yeardf <- major %>%
      filter(year == use_years[k])
    
    yearmatch <- c("spring", "summer", "fall")
    
    is_sequential <- FALSE
    if(
      (sum(yearmatch == yeardf$term) == 3)){
      is_sequential <- TRUE
    }
    
    save_seq <- c(save_seq, is_sequential)
    
  }
  
  save_degree <- FALSE
  if(
    (sum(save_seq == FALSE)) == 0){
      save_degree <- TRUE
    }
  
  
  if(save_degree == TRUE){
    
    save_degrees <- c(save_degrees, deg)
    
  }
  
}


df <- df %>%
  filter(degree %in% save_degrees)





  




# 4) save

getwd()
setwd("../cleaned")
write_csv(df, "df.csv")
