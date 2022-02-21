# graph to create:

# inflow > outflow = random walk with trend. sustained lead still happens

# outflow > inflow = random walk with trend. sustained lead still happens

# outflow > inflow but there is a zero, then sustained lead is less likely. 
## so, outflow > inflow with the assumption that there is a zero limi, or inflow > outflow with the assumption that there is a max limit, then sustained lead goes away


library(tidyverse)

# when inflow exceeds outflow, its a random walk with trend

time <- 200
y <- numeric(time)
inflow <- 0.5
outflow <- 0.4
y[1] <- 2
for(i in 2:time){
  
  y[i] <- y[i-1] + inflow - outflow + rnorm(1, 0, 0.3)
  
}

df1 <- 
plot(y)

# when outflow exceeds inflow, its a random walk with trend

time <- 200
y <- numeric(time)
inflow <- 0.4
outflow <- 0.5
y[1] <- 2
for(i in 2:time){
  
  y[i] <- y[i-1] + inflow - outflow + rnorm(1, 0, 0.3)
  
}

plot(y)


# when a goes to near zero, its...

time <- 200
y <- numeric(time)
inflow <- 0.4
outflow <- 0.45
y[1] <- 2
for(i in 2:time){
  
  y[i] <- 1*y[i-1] + inflow - outflow + rnorm(1, 0, 0.3)
  if(y[i] < 0){y[i] <- 0}
}

plot(y)
