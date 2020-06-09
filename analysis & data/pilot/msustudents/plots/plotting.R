library(ggplot2)
library(tidyverse)
library(data.table)
df <- read.csv("../data/cleaned/df.csv")
table(df$degree)

dt <- data.table(df)



ggplot(dt[degree == "Statistics"], aes(x = time, y = total, group = degree_id)) +
         geom_point() + 
         geom_line()




