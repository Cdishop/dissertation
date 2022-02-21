# graph to create:

# inflow > outflow = random walk with trend. sustained lead still happens

# outflow > inflow = random walk with trend. sustained lead still happens

# outflow > inflow but there is a zero, then sustained lead is less likely. 
## so, outflow > inflow with the assumption that there is a zero limi, or inflow > outflow with the assumption that there is a max limit, then sustained lead goes away


library(tidyverse)


# sustained lead, flows equal


# when inflow exceeds outflow, its a random walk with trend

set.seed(196)
time <- 200
y <- numeric(time)
inflow <- 0.45
outflow <- 0.45
runs <- matrix(, nrow = time, ncol = 4)
runs[1, ] <- 2
runs[1, 4] <- 1

for(i in 2:time){
  
  for(k in 1:3){
    
    runs[i, k] <- runs[i-1, k] + inflow - outflow + rnorm(1, 0, 0.3)
    runs[i, 4] <- i
    
  }
  
}

df0 <- data.frame(runs)
names(df0) <- c('p1', 'p2', 'p3', 'Day')
df0 <- df0 %>% 
  pivot_longer(cols = c(p1, p2, p3),
               names_to = 'people',
               values_to = 'Notifications') %>% 
  mutate(Person = case_when(
    people == 'p1' ~ 1, 
    people == 'p2' ~ 2,
    people == 'p3' ~ 3
  )) %>% 
  mutate(Person = as.character(Person))

ggplot(df0, aes(x = Day, y = Notifications, color = Person)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

g0 <- ggplot(df0, aes(x = Day, y = Notifications, color = Person)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave('rw.pdf', g0, width = 6, height = 5)



# when inflow exceeds outflow, its a random walk with trend

set.seed(196)
time <- 200
y <- numeric(time)
inflow <- 0.45
outflow <- 0.4
runs <- matrix(, nrow = time, ncol = 4)
runs[1, ] <- 2
runs[1, 4] <- 1

for(i in 2:time){
  
  for(k in 1:3){
    
    runs[i, k] <- runs[i-1, k] + inflow - outflow + rnorm(1, 0, 0.3)
    runs[i, 4] <- i
    
  }
  
}

df1 <- data.frame(runs)
names(df1) <- c('p1', 'p2', 'p3', 'Day')
df1 <- df1 %>% 
  pivot_longer(cols = c(p1, p2, p3),
               names_to = 'people',
               values_to = 'Notifications') %>% 
  mutate(Person = case_when(
    people == 'p1' ~ 1, 
    people == 'p2' ~ 2,
    people == 'p3' ~ 3
  )) %>% 
  mutate(Person = as.character(Person))

ggplot(df1, aes(x = Day, y = Notifications, color = Person)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

g1 <- ggplot(df1, aes(x = Day, y = Notifications, color = Person)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave('trendup.pdf', g1, width = 6, height = 5)







# when outflow exceeds inflow, its a random walk with trend

set.seed(7325)
time <- 200
y <- numeric(time)
inflow <- 0.4
outflow <- 0.45
runs <- matrix(, nrow = time, ncol = 4)
runs[1, ] <- 2
runs[1, 4] <- 1

for(i in 2:time){
  
  for(k in 1:3){
    
    runs[i, k] <- runs[i-1, k] + inflow - outflow + rnorm(1, 0, 0.3)
    runs[i, 4] <- i
    
  }
  
}

df2 <- data.frame(runs)
names(df2) <- c('p1', 'p2', 'p3', 'Day')
df2 <- df2 %>% 
  pivot_longer(cols = c(p1, p2, p3),
               names_to = 'people',
               values_to = 'Notifications') %>% 
  mutate(Person = case_when(
    people == 'p1' ~ 1, 
    people == 'p2' ~ 2,
    people == 'p3' ~ 3
  )) %>% 
  mutate(Person = as.character(Person))

ggplot(df2, aes(x = Day, y = Notifications, color = Person)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

g2 <- ggplot(df2, aes(x = Day, y = Notifications, color = Person)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave('trenddown.pdf', g2, width = 6, height = 5)






# when a goes to near zero, its... [there isn't a good way to represent a going to zero in a stock-flow diagram]


set.seed(295)
time <- 200
y <- numeric(time)
inflow <- 0.45
outflow <- 0.45
runs <- matrix(, nrow = time, ncol = 4)
runs[1, ] <- .2
runs[1, 4] <- 1

for(i in 2:time){
  
  for(k in 1:3){
    
    runs[i, k] <- 0*runs[i-1, k] + inflow - outflow + rnorm(1, 0, 0.3)
    runs[i, 4] <- i
    
  }
  
}

df3 <- data.frame(runs)
names(df3) <- c('p1', 'p2', 'p3', 'Day')
df3 <- df3 %>% 
  pivot_longer(cols = c(p1, p2, p3),
               names_to = 'people',
               values_to = 'Notifications') %>% 
  mutate(Person = case_when(
    people == 'p1' ~ 1, 
    people == 'p2' ~ 2,
    people == 'p3' ~ 3
  )) %>% 
  mutate(Person = as.character(Person))

ggplot(df3, aes(x = Day, y = Notifications, color = Person)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  ylim(-2, 5)

g3 <- ggplot(df3, aes(x = Day, y = Notifications, color = Person)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  ylim(-2, 5)

ggsave('azero.pdf', g3, width = 6, height = 5)




# when outflow exceeds infow, sustained lead does go away assuming there is a bottom limit


set.seed(7325)
time <- 200
y <- numeric(time)
inflow <- 0.4
outflow <- 0.45
runs <- matrix(, nrow = time, ncol = 4)
runs[1, ] <- 2
runs[1, 4] <- 1

for(i in 2:time){
  
  for(k in 1:3){
    
    runs[i, k] <- runs[i-1, k] + inflow - outflow + rnorm(1, 0, 0.3)
    runs[i, 4] <- i
    
    if(runs[i, k] < 0){runs[i,k] <- 0}
  }
  
}

df4 <- data.frame(runs)
names(df4) <- c('p1', 'p2', 'p3', 'Day')
df4 <- df4 %>% 
  pivot_longer(cols = c(p1, p2, p3),
               names_to = 'people',
               values_to = 'Notifications') %>% 
  mutate(Person = case_when(
    people == 'p1' ~ 1, 
    people == 'p2' ~ 2,
    people == 'p3' ~ 3
  )) %>% 
  mutate(Person = as.character(Person))

ggplot(df4, aes(x = Day, y = Notifications, color = Person)) + 
  geom_point(alpha = 0.8) + 
  geom_line(alpha = 0.8) + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  ylim(-0.23, 7) + 
  geom_hline(yintercept = 0, size = 2, color = "plum") + 
  annotate('text', x = 100, y = -0.23, label = "Notifications = 0", color = "plum", size = 4.5)

g4 <- ggplot(df4, aes(x = Day, y = Notifications, color = Person)) + 
  geom_point(alpha = 0.8) + 
  geom_line(alpha = 0.8) + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  ylim(-0.23, 7) + 
  geom_hline(yintercept = 0, size = 2, color = "plum") + 
  annotate('text', x = 100, y = -0.23, label = "Notifications = 0", color = "plum", size = 4.5)

ggsave('trenddown-limit.pdf', g4, width = 6, height = 5)



# if the outflow is super large, then the trajectories cannot get back above 0



set.seed(7325)
time <- 200
y <- numeric(time)
inflow <- 0.4
outflow <- 2
runs <- matrix(, nrow = time, ncol = 4)
runs[1, ] <- 2
runs[1, 4] <- 1

for(i in 2:time){
  
  for(k in 1:3){
    
    runs[i, k] <- runs[i-1, k] + inflow - outflow + rnorm(1, 0, 0.3)
    runs[i, 4] <- i
    
    if(runs[i, k] < 0){runs[i,k] <- 0}
  }
  
}

df5 <- data.frame(runs)
names(df5) <- c('p1', 'p2', 'p3', 'Day')
df5 <- df5 %>% 
  pivot_longer(cols = c(p1, p2, p3),
               names_to = 'people',
               values_to = 'Notifications') %>% 
  mutate(Person = case_when(
    people == 'p1' ~ 1, 
    people == 'p2' ~ 2,
    people == 'p3' ~ 3
  )) %>% 
  mutate(Person = as.character(Person))

ggplot(df5, aes(x = Day, y = Notifications, color = Person)) + 
  geom_point(alpha = 0.4) + 
  geom_line(alpha = 0.4) + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  ylim(-0.23, 7) + 
  geom_hline(yintercept = 0, size = 2, color = "plum") + 
  annotate('text', x = 100, y = -0.23, label = "Notifications = 0", color = "plum", size = 4.5)

g5 <- ggplot(df5, aes(x = Day, y = Notifications, color = Person)) + 
  geom_point(alpha = 0.4) + 
  geom_line(alpha = 0.4) + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  ylim(-0.23, 7) + 
  geom_hline(yintercept = 0, size = 2, color = "plum") + 
  annotate('text', x = 100, y = -0.23, label = "Notifications = 0", color = "plum", size = 4.5)

ggsave('trenddown-limit-superoutflow.pdf', g5, width = 6, height = 5)
