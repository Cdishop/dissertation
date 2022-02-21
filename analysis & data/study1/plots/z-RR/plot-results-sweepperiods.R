library(tidyverse)
library(ggplot2)
library(gridExtra)
library(data.table)


# load and merge all simulation results

setwd("../../simulations/z-RR/sweep-time/sim-results")
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
    simulation == "sim3a" | simulation == "sim3b" ~ "Respond To Influx",
    simulation == "sim4a" | simulation == "sim4b" ~ "Respond To Outflow",
    simulation == "sim1a" | simulation == "sim1b" ~ "Respond To Many",
    simulation == "sim2a" | simulation == "sim2b" ~ "Respond To Few",
  )) %>%
  mutate(situation = case_when(
    simulation == "sim3a" | simulation == "sim4a" | simulation == "sim1a" | simulation == "sim2a" ~ "Sustained Lead",
    simulation == "sim3b" | simulation == "sim4b" | simulation == "sim1b" | simulation == "sim2b" ~ "No Sustained Lead"
  )) 


df <- df %>% 
  mutate(situation = factor(situation,
                            levels = c("Sustained Lead", 
                                       "No Sustained Lead")))

ggplot(df %>% filter(
                     timeamount == 3), aes(x = k, y = probability)) + 
  #geom_bar(stat = "identity", position = "dodge", alpha = 0.3) + 
  geom_line() + 
  facet_grid(situation ~ response) + 
  theme_classic(base_size = 15) + 
  labs(x = "Time Spent", y = "Probability") + 
  ggtitle("Periods = 3")



ggplot(df %>% filter(
  timeamount == 40), aes(x = k, y = probability)) + 
  #geom_bar(stat = "identity", position = "dodge", alpha = 0.3) + 
  geom_line() + 
  facet_grid(situation ~ response) + 
  theme_classic(base_size = 15) + 
  labs(x = "Time Spent", y = "Probability") + 
  ggtitle("Periods = 40")




ggplot(df %>% filter(
  timeamount == 200), aes(x = k, y = probability)) + 
  #geom_bar(stat = "identity", position = "dodge", alpha = 0.3) + 
  geom_line() + 
  facet_grid(situation ~ response) + 
  theme_classic(base_size = 15) + 
  labs(x = "Time Spent", y = "Probability") + 
  ggtitle("Periods = 200")





g1 <- ggplot(df %>% filter(
  timeamount == 3), aes(x = k, y = probability)) + 
  #geom_bar(stat = "identity", position = "dodge", alpha = 0.3) + 
  geom_line() + 
  facet_grid(situation ~ response) + 
  theme_classic(base_size = 15) + 
  labs(x = "Time Spent", y = "Probability") + 
  ggtitle("Periods = 3")


g2 <- ggplot(df %>% filter(
  timeamount == 40), aes(x = k, y = probability)) + 
  #geom_bar(stat = "identity", position = "dodge", alpha = 0.3) + 
  geom_line() + 
  facet_grid(situation ~ response) + 
  theme_classic(base_size = 15) + 
  labs(x = "Time Spent", y = "Probability") + 
  ggtitle("Periods = 40")




g3 <- ggplot(df %>% filter(
  timeamount == 200), aes(x = k, y = probability)) + 
  #geom_bar(stat = "identity", position = "dodge", alpha = 0.3) + 
  geom_line() + 
  facet_grid(situation ~ response) + 
  theme_classic(base_size = 15) + 
  labs(x = "Time Spent", y = "Probability") + 
  ggtitle("Periods = 200")


ggsave('j.pdf', g1, width = 7, height = 5)
ggsave('j2.pdf', g2, width = 7, height = 5)
ggsave('j3.pdf', g3, width = 7, height = 5)


