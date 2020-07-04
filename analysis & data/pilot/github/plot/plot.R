library(tidyverse)
library(ggplot2)

pt <- read.csv("../data/for-analysis-ts/daily_plot.csv")
repoids <- pt %>% 
  distinct(repo)
repoids <- repoids %>% 
  mutate(repoid = 1:nrow(repoids)) %>% 
  mutate(nameit = "GitHub Repo:") %>% 
  select(nameit, repoid, repo)

pt <- left_join(pt, repoids)
pt <- pt %>% 
  arrange(repoid) %>% 
  unite("repoidname", nameit, repoid, sep = " ")
  


pt %>% 
  ggplot() +
  aes(event_at, value, color = repoidname, group = repoidname) + 
  geom_step(show.legend = F, color = "black") + 
  facet_wrap(vars(repoidname), scales = "free") + 
  ylim(0, NA) + 
  theme_classic() + 
  labs(
    x = "Day",
    y = NULL,
    title = "Number of Open Issues"
  )










ts <- read.csv("../data/for-analysis-ts/daily_issues.csv")

ts <- left_join(ts, repoids)

small <- ts %>% 
  filter(repoid == 12)

small %>% 
  ggplot() + 
  aes(x = event_at, y = value, group = repoid) + 
  geom_point(size = 0.5, alpha = 0.8) + 
  geom_line()


small2 <- pt %>% 
  filter(repoidname == "GitHub Repo: 12")

small2 %>% 
  ggplot() + 
  aes(x = event_at, y = value, group = repoidname) + 
  geom_point(size = .5, alpha = 0.8) + 
  geom_line()
