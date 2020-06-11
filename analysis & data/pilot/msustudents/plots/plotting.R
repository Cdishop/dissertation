library(ggplot2)
library(tidyverse)
library(data.table)
df <- read.csv("../data/cleaned/df.csv")
dt <- data.table(df)



ggplot(dt[degree == "College of Natural Science-Statistics"], aes(x = time, y = total, group = degree_id)) +
         geom_point(alpha = 0.8) + 
         geom_line(color = "darkblue", alpha = 0.7) + 
         labs(x = "Time", y = "Number Graduate Students", title = "Statistics") + 
         theme_bw()
  



ggplot(dt, aes(x = time, y = total, group = degree_id)) +
  geom_point(alpha = 0.8) + 
  geom_line(color = "darkblue", alpha = 0.7) + 
  labs(x = "Time", y = "Number Graduate Students", title = "Statistics") + 
  theme_bw() + 
  facet_wrap(~degree)
  
# series that look weird:
# Economics
# Microbiol & Molecular Genetics
# Zoology
# Biochemistry & Molecular Biol

unique(df$degree)

ggplot(dt[degree == "College of Business-Economics"], aes(x = time, y = total, group = degree_id)) +
  geom_point(alpha = 0.8) + 
  geom_line(color = "darkblue", alpha = 0.7) + 
  labs(x = "Time", y = "Number Graduate Students") + 
  theme_bw()

df %>%
  filter(degree == "Economics")

ggplot(dt[degree == "College of Natural Science-Microbiol & Molecular Genetics"], aes(x = time, y = total, group = degree_id)) +
  geom_point(alpha = 0.8) + 
  geom_line(color = "darkblue", alpha = 0.7) + 
  labs(x = "Time", y = "Number Graduate Students") + 
  theme_bw()

df %>%
  filter(degree == "Microbiol & Molecular Genetics")

ggplot(dt[degree == "College of Natural Science-Zoology"], aes(x = time, y = total, group = degree_id)) +
  geom_point(alpha = 0.8) + 
  geom_line(color = "darkblue", alpha = 0.7) + 
  labs(x = "Time", y = "Number Graduate Students") + 
  theme_bw()

df %>%
  filter(degree == "Zoology")



