df <- data.frame(
  'id' = c(rep(c('Focal Employee', 'Colleagues'), 9)),
  'OCBs' = c(7, 3, 
             0, 0, 
             5, 6,
             0, 0,
             9, 4,
             0, 0,
             3, 1,
             0, 0,
             6, 3),
  'Time' = c('1', '1',
             '2', '2',
             '3', '3',
             '4', '4', 
             '5', '5',
             '6', '6', 
             '7', '7',
             '8', '8',
             '9', '9')
)

library(tidyverse)
library(ggplot2)


ggplot(df, aes(x = Time, y = OCBs, fill = id)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_classic() + 
  scale_fill_grey() + 
  scale_x_discrete(labels = NULL) + 
  scale_y_continuous(labels = NULL) + 
  theme(axis.ticks = element_blank()) + 
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 20))

        