library(tidyverse)

sims <- 5000
saveit <- numeric(sims)
for(sim in 1:sims){
  

time <- 200
y <- numeric(time)
x <- numeric(time)
inflow <- 0.4
outflow <- 0.41
y[1] <- 2
x[1] <- 2
for(i in 2:time){
  
  y[i] <- 0*y[i-1] + inflow - outflow + rnorm(1, 0, 0.3)
  if(y[i] < 0){y[i] <- 0}
  
  x[i] <- 0*x[i-1] + inflow - outflow + rnorm(1, 0, 0.3)
  if(x[i] < 0){x[i] <- 0}
  
}

df <- data.frame(
  'x' = c(x),
  'y' = c(y),
  'time' = c(1:time)
)

df <- df %>% 
  mutate(xlead = ifelse(x > y, 1, 0))

totalleads <- sum(df$xlead)
proportion <- totalleads / time

sims[sim] <- proportion

}

finaldf <- data.frame(table(sims))
str(finaldf)
finaldf <- finaldf %>% 
  mutate(freq = Freq,
         prop = sims) %>% 
  mutate(prop = as.numeric(as.character(prop)))


ggplot(finaldf, aes(x = prop, y = Freq)) + 
  geom_bar(stat = "identity") + 
  xlim(0, 1)
