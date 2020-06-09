library(tidyverse)
library(ggplot2)
library(gridExtra)
library(data.table)


# load and merge all simulation results

setwd("../simulations/sim-results")
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

# sim 1 = respond to many; sim 2 = respond to few
# a = random & inertia
# b = random & no inertia

keep1 <- c("sim1a", "sim1b", "sim2a", "sim2b")
p1df <- df %>%
  filter(simulation %in% keep1) %>%
  mutate(response = case_when(
    simulation == "sim1a" | simulation == "sim1b" ~ "Respond To Many",
    simulation == "sim2a" | simulation == "sim2b" ~ "Respond To Few"
  )) %>%
  mutate(situation = case_when(
    simulation == "sim1a" | simulation == "sim2a" ~ "Random & Inertia",
    simulation == "sim1b" | simulation == "sim2b" ~ "Random & No Inertia"
  )) 


ggplot(p1df, aes(x = k, y = probability)) + 
  geom_bar(stat = "identity") + 
  facet_grid(situation ~ response) + 
  theme_classic(base_size = 15) + 
  labs(x = "Time Spent", y = "Probability") 








# sim 3 = respond to influx; sim 4 = respond to outflow
# a = random & inertia
# b = random & no inertia

keep2 <- c("sim3a", "sim3b", "sim4a", "sim4b")
p2df <- df %>%
  filter(simulation %in% keep2) %>%
  mutate(response = case_when(
    simulation == "sim3a" | simulation == "sim3b" ~ "Respond To Influx",
    simulation == "sim4a" | simulation == "sim4b" ~ "Respond To Outflow"
  )) %>%
  mutate(situation = case_when(
    simulation == "sim3a" | simulation == "sim4a" ~ "Random & Inertia",
    simulation == "sim3b" | simulation == "sim4b" ~ "Random & No Inertia"
  )) 


ggplot(p2df, aes(x = k, y = probability)) + 
  geom_bar(stat = "identity") + 
  facet_grid(situation ~ response) + 
  theme_classic(base_size = 15) + 
  labs(x = "Time Spent", y = "Probability")




# conformity
# 5 = high; 6 = moderate; 7 = low

keepers <- c("sim5a", "sim5b", "sim6a", "sim6b", "sim7a", "sim7b")
conform <- df %>%
  filter(simulation %in% keepers) %>%
  mutate(situation = case_when(
    simulation == "sim5a" | simulation == "sim6a" | simulation == "sim7a" ~ "Random & Inertia",
    simulation == "sim5b" | simulation == "sim6b" | simulation == "sim7b" ~ "Random & No Inertia"
  )) %>%
  mutate(Conformity = case_when(
    simulation == "sim5a" | simulation == "sim5b" ~ "0.8 High",
    simulation == "sim6a" | simulation == "sim6b" ~ "0.5 Moderate",
    simulation == "sim7a" | simulation == "sim7b" ~ "0.2 Low"
  )) 



ggplot(conform, aes(x = k, y = probability)) + 
  geom_bar(stat = "identity") +  
  facet_grid(situation ~ Conformity) + 
  theme_classic(base_size = 15) + 
  labs(x = "Time Spent", y = "Probability")

