
# steps
# 1) get files
# 2) for each file, load and clean, then combine
# 3) select only those with time-series > 20 periods
# 4) add time column
# 5) save

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
### begin on row 5

df <- df %>%
  select(starts_with("Michi"),
         "...2",
         "...3",
         "...12") %>%
  rename(degree = starts_with("Michi"),
         degree_id = "...2",
         degree_level = "...3",
         total = "...12") %>%
  slice(-c(1:4))

### impute college name within degree names because some degrees have the same title

ag_start <- which(df$degree == "Agriculture and Natural Resources")
ag_end <- which(df$degree == "Agriculture and Natural Resources Total")
ag_rows <- seq(ag_start:(ag_end - 1))
ag <- df[ag_rows, ]
ag <- ag %>%
  slice(-1)
ag$degree <- paste("College of Agriculture", ag$degree, sep = "-")


create_college <- function(title1,
                           title2,
                           renamelabel){
  
  start <- which(df$degree == title1)
  end <- which(df$degree == title2)
  rows <- seq(from = start, to = (end-1), by = 1)
  collegedf <- df[rows, ]
  collegedf <- collegedf %>%
    slice(-1)
  collegedf$degree <- paste(renamelabel, collegedf$degree, sep = "-")
  return(collegedf)
  
  
}

artsletters <- create_college("Arts and Letters",
                             "Arts and Letters Total",
                             "College of Arts")

business <- create_college("Business",
                           "Business Total",
                           "College of Business")

communication <- create_college("Communication Arts and Sciences",
                                "Communication Arts and Sciences Total",
                                "College of Communication")

education <- create_college("Education",
                            "Education Total",
                            "College of Education")

engineering <- create_college("Engineering",
                              "Engineering Total",
                              "College of Engineering")

medicine <- create_college("Human Medicine",
                          "Human Medicine Total",
                          "College of Medicine")

natscience <- create_college("Natural Science",
                             "Natural Science Total",
                             "College of Natural Science")


## social science is a weird one because there is a social science degree itself


ss_start <- which(df$degree == "Osteopathic Medicine Total")
ss_end <- which(df$degree == "Social Science Total")
ss_rows <- seq(from = (ss_start + 1), to = (ss_end - 1), by = 1)
ss <- df[ss_rows, ]
ss <- ss %>%
  slice(-1)
ss$degree <- paste("College of Social Science", ss$degree, sep = "-")


vet <- create_college("Veterinary Medicine",
                      "Veterinary Medicine Total",
                      "College of Vet Medicine")



df <- bind_rows(ag,
                artsletters,
                business,
                communication,
                education,
                engineering,
                medicine,
                natscience,
                ss,
                vet)

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



## repeat for every other file


create_collegel <- function(title1,
                           title2,
                           renamelabel){
  
  start <- which(dfl$degree == title1)
  end <- which(dfl$degree == title2)
  rows <- seq(from = (start + 1), to = (end-1), by = 1)
  collegedf <- dfl[rows, ]
  collegedf <- collegedf %>%
    slice(-1)
  collegedf$degree <- paste(renamelabel, collegedf$degree, sep = "-")
  return(collegedf)
  
  
}



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
    slice(-c(1:4))
  
  
  ag_start <- which(dfl$degree == "Agriculture and Natural Resources")
  ag_end <- which(dfl$degree == "Agriculture and Natural Resources Total")
  ag_rows <- seq(from = ag_start, to = (ag_end - 1), by = 1)
  ag <- dfl[ag_rows, ]
  ag <- ag %>%
    slice(-1)
  ag$degree <- paste("College of Agriculture", ag$degree, sep = "-")
  
  artsletters <- create_collegel("Agriculture and Natural Resources Total",
                                "Arts and Letters Total",
                                "College of Arts")
  
  business <- create_collegel("Arts and Letters Total",
                             "Business Total",
                             "College of Business")
  
  communication <- create_collegel("Business Total",
                                  "Communication Arts and Sciences Total",
                                  "College of Communication")
  
  education <- create_collegel("Communication Arts and Sciences Total",
                               "Education Total",
                               "College of Education")
  
  
  engineering <- create_collegel("Education Total",
                                "Engineering Total",
                                "College of Engineering")
  
  medicine <- create_collegel("Engineering Total",
                            "Human Medicine Total",
                            "College of Medicine")
  
  natscience <- create_collegel("Human Medicine Total",
                               "Natural Science Total",
                               "College of Natural Science")
  
  ss <- create_collegel("Natural Science Total",
                        "Social Science Total",
                        "College of Social Science")
  
  
  vet <- create_collegel("Social Science Total",
                        "Veterinary Medicine Total",
                        "College of Vet Medicine")
  
  dfl <- bind_rows(ag,
                  artsletters,
                  business,
                  communication,
                  education,
                  engineering,
                  medicine,
                  natscience,
                  ss,
                  vet)
  
  
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
  
  df <- bind_rows(df, dfl)
  
}

# 3) select only time-series with > 20 periods

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



# 4) add time column


degrees <- unique(df$degree)
dftime <- df %>%
  filter(degree == degrees[1])
dftime <- dftime %>%
  mutate(time = 1:nrow(dftime))

for(p in 2:length(degrees)){
  
  
  dd <- df %>%
    filter(degree == degrees[p])
  dd <- dd %>%
    mutate(time = 1:nrow(dd))
  
  dftime <- bind_rows(dftime, dd)
}



# 5) save

getwd()
setwd("../cleaned")
write_csv(dftime, "df.csv")
