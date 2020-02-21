time <- 200
y <- numeric(time)
y[1] <- 14
yes <- numeric(time)
yes[1] <- 14

for(i in 2:time){
  
  peeps_in <- round(rnorm(1, 4, 0.5))
  peeps_out <- round(rnorm(1, 4, 0.5))
  
  y[i] <- y[i-1] + peeps_in - peeps_out
  yes[i] <- yes[i-1] + rnorm(1, 0, 1)
  
}

library(ggplot2)

df <- data.frame(
  'time' = c(1:time),
  'y' = c(y),
  'yes' = c(yes)
)

ggplot(df, aes(x = time, y = y)) + 
  geom_point() + 
  geom_line() + 
  theme_classic()


library(tseries)

# if p < 0.05, then stationary
adf.test(df$y)
adf.test(df$yes)
