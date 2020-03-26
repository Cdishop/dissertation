# Figure 2

df_fig2 <- data.frame(
  'k' = c(0:20),
  'Probability' = c(0.23, 0.19, 0.16, 0.13, 0.11, 0.09, 0.08, 0.07, 0.07, 0.07, 0.07,
                    0.07, 0.07, 0.07, 0.08, 0.09, 0.11, 0.13, 0.16, 0.19, 0.23)
  )

library(tidyverse)
library(ggplot2)

ggplot(df_fig2, aes(x = k, y = Probability)) +
  geom_bar(stat = 'identity') + 
  theme_classic() + 
  theme(axis.title.x = element_text(face = 'italic'))




# Figure 3

df_fig3 <- data.frame(
  'k' = c(rep(0:20, 5)),
  'Probability' = c(rep(c(0.23, 0.19, 0.16, 0.13, 0.11, 0.09, 0.08, 0.07, 0.07, 0.07, 0.07,
                    0.07, 0.07, 0.07, 0.08, 0.09, 0.11, 0.13, 0.16, 0.19, 0.23),5)),
  'Drift' = c(rep('0.2', 21),
              rep('0.4', 21),
              rep('0.6', 21),
              rep('0.8', 21),
              rep('1.0', 21)
))

library(tidyverse)
library(ggplot2)

ggplot(df_fig3, aes(x = k, y = Probability, shape = Drift)) +
  theme_classic() + 
  geom_point(alpha = 0) +
  theme(axis.title.x = element_text(face = 'italic')) + 
  guides(shape = guide_legend(override.aes = list(alpha=1))) 



# Figure 4

df_fig4 <- data.frame(
  'k' = c(rep(0:20, 5)),
  'Probability' = c(rep(c(0.23, 0.19, 0.16, 0.13, 0.11, 0.09, 0.08, 0.07, 0.07, 0.07, 0.07,
                          0.07, 0.07, 0.07, 0.08, 0.09, 0.11, 0.13, 0.16, 0.19, 0.23),5)),
  'Autoregressive' = c(rep('0.0', 21),
              rep('0.2', 21),
              rep('0.4', 21),
              rep('0.6', 21),
              rep('0.8', 21)
  ))

library(tidyverse)
library(ggplot2)

ggplot(df_fig4, aes(x = k, y = Probability, shape = Autoregressive)) +
  theme_classic() + 
  geom_point(alpha = 0) +
  theme(axis.title.x = element_text(face = 'italic')) + 
  guides(shape = guide_legend(override.aes = list(alpha=1))) 



# Figure 5

df_fig5 <- data.frame(
  'k' = c(rep(0:20, 5)),
  'Probability' = c(rep(c(0.23, 0.19, 0.16, 0.13, 0.11, 0.09, 0.08, 0.07, 0.07, 0.07, 0.07,
                          0.07, 0.07, 0.07, 0.08, 0.09, 0.11, 0.13, 0.16, 0.19, 0.23),5)),
  'Employees' = c(rep('2', 21),
                       rep('200', 21),
                       rep('400', 21),
                       rep('600', 21),
                       rep('800', 21)
  ))

library(tidyverse)
library(ggplot2)

ggplot(df_fig5, aes(x = k, y = Probability, shape = Employees)) +
  theme_classic() + 
  geom_point(alpha = 0) +
  theme(axis.title.x = element_text(face = 'italic')) + 
  guides(shape = guide_legend(override.aes = list(alpha=1))) 
