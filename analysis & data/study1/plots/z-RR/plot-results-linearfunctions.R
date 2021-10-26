library(tidyverse)
library(ggplot2)
library(gridExtra)
library(data.table)


# load and merge all simulation results

setwd("../../simulations/z-RR/linear-functions/sim-results")
file_names <- Sys.glob("*.csv")
file_labels <- gsub(".csv", "", file_names)


df <- read.csv(file_names[1])
df <- df %>%
    mutate(simulation = file_labels[1])

count <- 1
for(file in file_names[-1]){
  count <- count + 1
  
  new_df <- read.csv(file)
  new_df <- new_df %>%
    mutate(simulation = file_labels[count])
  
  df <- bind_rows(df, new_df)
  
}

# sim 3 = respond to influx; sim 4 = respond to outflow
# a = random & inertia
# b = random & no inertia

df <- df %>%
  mutate(response = case_when(
    simulation == "sim3a-linear" | simulation == "sim3b-linear" ~ "Respond To Influx",
    simulation == "sim4a-linear" | simulation == "sim4b-linear" ~ "Respond To Outflow"
  )) %>%
  mutate(situation = case_when(
    simulation == "sim3a-linear" | simulation == "sim4a-linear" ~ "Requests Accumulate",
    simulation == "sim3b-linear" | simulation == "sim4b-linear" ~ "Requests Do Not Accumulate"
  )) 


ggplot(df, aes(x = k, y = probability)) + 
  geom_bar(stat = "identity") + 
  facet_grid(situation ~ response) + 
  theme_classic(base_size = 15) + 
  labs(x = "Time Spent", y = "Probability") 








