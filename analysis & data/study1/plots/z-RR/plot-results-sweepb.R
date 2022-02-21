library(tidyverse)
library(ggplot2)
library(gridExtra)
library(data.table)


# load and merge all simulation results

a_file_names <- Sys.glob("../../simulations/z-RR/sweep-b/sim-results/1a-sweepb/*.csv")


df <- read.csv(a_file_names[1])
df <- df %>%
    mutate(simulation = "Sustained Lead")

count <- 1
for(file in a_file_names[-1]){
  count <- count + 1
  
  new_df <- read.csv(file)
  new_df <- new_df %>%
    mutate(simulation = "Sustained Lead")
  
  df <- bind_rows(df, new_df)
  
}


# now do the same thing for the requests do not accumulate condition

b_file_names <- Sys.glob("../../simulations/z-RR/sweep-b/sim-results/1b-sweepb/*.csv")


blah <- read.csv(b_file_names[1])
blah <- blah %>%
  mutate(simulation = "No Sustained Lead")

df <- bind_rows(df, blah)

count <- 1
for(file in b_file_names[-1]){
  count <- count + 1
  
  new_df <- read.csv(file)
  new_df <- new_df %>%
    mutate(simulation = "No Sustained Lead")
  
  df <- bind_rows(df, new_df)
  
}


df <- df %>% 
  mutate(simulation = factor(simulation,
                             levels = c("Sustained Lead",
                                        "No Sustained Lead")))

ggplot(df %>% filter(!b_val == 0), aes(x = k, y = probability, color = factor(b_val))) + 
  geom_point(alpha = 0.8) +
  geom_line(alpha = 0.8) + 
  facet_wrap(~simulation, ncol = 1) + 
  theme_classic(base_size = 15) + 
  labs(x = "Time Spent", y = "Probability", color = "b") 


g <-ggplot(df %>% filter(!b_val == 0), aes(x = k, y = probability, color = factor(b_val))) + 
  geom_point(alpha = 0.8) +
  geom_line(alpha = 0.8) + 
  facet_wrap(~simulation, ncol = 1) + 
  theme_classic(base_size = 15) + 
  labs(x = "Time Spent", y = "Probability", color = "b") 

ggsave('sweepb.pdf', g, width = 6, height = 5.5)


