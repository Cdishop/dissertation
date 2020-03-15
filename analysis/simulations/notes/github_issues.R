# more issues coming in than you can resolve

time <- 50
y <- numeric(time)
y[1] <- 0

for(i in 2:time){
  
  issues_in <- round(rnorm(1, 2, 0.5))
  issues_out <- round(rnorm(1, 1, 0.5))
  
  y[i] <- y[i-1] + (issues_in - issues_out)

}

library(ggplot2)

df <- data.frame(
  'time' = c(1:time),
  'y' = c(y)
)

ggplot(df, aes(x = time, y = y)) + 
  geom_point() + 
  geom_line() + 
  theme_classic()


library(tseries)

## if p < 0.05, then stationary
adf.test(df$y)






# completing issues to maintain a steady level


time <- 500
y <- numeric(time)
y[1] <- 4

for(i in 2:time){
  
  issues_in <- round(rnorm(1, 2, 0.5))
  issues_out <- round(rnorm(1, 2, 0.5))
  
  y[i] <- y[i-1] + (issues_in - issues_out)
  
}

library(ggplot2)

df <- data.frame(
  'time' = c(1:time),
  'y' = c(y)
)

ggplot(df, aes(x = time, y = y)) + 
  geom_point() + 
  geom_line() + 
  theme_classic()


library(tseries)

## if p < 0.05, then stationary
adf.test(df$y)








# force it to stay at an equilibrium of 4

time <- 50
y <- numeric(time)
y[1] <- 4

for(i in 2:time){
  
  issues_in <- round(rnorm(1, 4, 1))
  issues_out <- issues_in
  
  y[i] <- (issues_in - issues_out)
  
}

library(ggplot2)

df <- data.frame(
  'time' = c(1:time),
  'y' = c(y)
)

ggplot(df, aes(x = time, y = y)) + 
  geom_point() + 
  geom_line() + 
  theme_classic()


library(tseries)

## if p < 0.05, then stationary
adf.test(df$y)


