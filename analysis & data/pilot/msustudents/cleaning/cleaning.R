
# steps
# 1) get files
# 2) for each file, load and clean, then combine
# 3) save

library(tidyverse)
library(readxl)

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
df$time <- 1


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
  dfl$time <- i
  dfl <- dfl %>%
    distinct(degree, .keep_all = T)
  
  df <- bind_rows(df, dfl)
  
}


# 3) save

getwd()
setwd("../cleaned")
write_csv(df, "df.csv")
