library(tidyverse)
library(viridis)

# Functions for calculation probabilty and counts

day_pdf <- function(m, k, d=365){
  # m = number of people with birthdays on that day,
  # k = total number of people
  # d = number of days

  dbinom(m, k, prob = 1/d)
}


# exact formula
day_atleast2 <- function(k, d=365){
  p1 <- (d+k-1) / (d-1)
  p2 <- ((d-1)/d)^k
  return(d * (1-p1*p2))
}

# binomial formula
day_atleast2_alt <- function(k, d=365){
  d*(1 - dbinom(0, k, 1/d) - dbinom(1,k, 1/d))
}

# # Testing
# day_atleast2(1:29)
# day_atleast2_alt(1:29)
# day_atleast2(1:29) - day_atleast2_alt(1:29)
#


# Plot conditions
ks <- 10:1000
ms <- c(0, 1)
dat <- expand.grid(ks, ms)
colnames(dat) <- c("k", "m")



dat %>%
  rowwise() %>%
  mutate(prob = day_pdf(m =m, k=k, d=365)) %>%
  mutate(exp = 365 * prob)


#######################################
#### Probability  (and count) that no one has that Birthday
#### P(D_r = 0)
#######################################
x <- 10:1000
y <- 10:1000
z0<- outer(x, y, function(x, y) {dbinom(0, x, prob = 1/y) })
data <- expand.grid(x = x, y = y)
data$z0 <- as.vector(z0)
data$e0 <- data$z0 * data$y


lp <- length(unique(data$z0))
prob_colors <-viridis(lp, option = "rocket")[(lp/6):lp]  # use only part the color palette

col365 <- "#FFD700"
colxy <- "#000000"



p1 <- ggplot(data, aes(x = x, y = y, fill = z0)) +
  geom_raster() +
  scale_fill_gradientn(
    limits = c(0,1),
    colors = prob_colors,
    oob = scales::squish,
    name = "Probability")+
  theme_minimal() +
  theme(
        # legend.position = "none",
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"))+
  labs(title =expression("P(" * D[r] *"=0)")) +
  xlab("Total number of people (k)") +
  ylab("Days in a year (d)") +
  geom_hline(yintercept = 365, color = colxy, linewidth = 2)+
  geom_abline(slope = 1, color = col365, linewidth = 2)
p1



lu <- length(unique(data$e0))
viridis_colors <- viridis(lu, option = "mako")[(lu/3):lu]  # use only part the color palette

p2 <- ggplot(data, aes(x = x, y = y, fill = e0)) +
  geom_raster() +
  scale_fill_gradientn(
    limits = c(0,500),
    colors = viridis_colors,
    oob = scales::squish,
    breaks = c(0, 100, 200, 300, 400, 500),
    labels = c(0, 100, 200, 300, 400, expression("\u2265" ~ 500)),
    name = "Expected \n Value")+
  theme_minimal() +
  theme(
        # legend.position = "none",
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"))+
  labs(title = expression(E * bgroup("(", sum(bold(1)(D[r] == 0), r == 1, d), ")"))) +
  xlab("Total number of people (k)") +
  ylab("Days in a year (d)") +
  geom_hline(yintercept = 365, color = colxy, linewidth = 2)+
  geom_abline(slope = 1, color = col365, linewidth = 2)
p2



#######################################
#### Probability  (and count) that only one person has that Birthday
#### P(D_r = 1)
#######################################

z1 <- outer(x, y, function(x, y) {dbinom(1, x, prob = 1/y) })
data$z1 <- as.vector(z1)
data$e1 <- data$z1 * data$y


p3 <- ggplot(data, aes(x = x, y = y, fill = z1)) +
  geom_raster() +
  scale_fill_gradientn(
    limits = c(0,1),
    colors = prob_colors,
    oob = scales::squish,
    name = "Probability")+
  theme_minimal() +
  theme(#legend.position = "none",
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"))+
  labs(title =expression("P(" * D[r] *"=1)")) +
  xlab("Total number of people (k)") +
  ylab("Days in a year (d)") +
  geom_hline(yintercept = 365, color = colxy, linewidth = 2)+
  geom_abline(slope = 1, color = col365, linewidth = 2)
p3


p4 <- ggplot(data, aes(x = x, y = y, fill = e1)) +
  geom_raster() +
  scale_fill_gradientn(
    limits = c(0,500),
    colors = viridis_colors,
    name = "Expected Value")+
  theme_minimal() +
  theme(#legend.position = "none",
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"))+
  labs(title = expression(E * bgroup("(", sum(bold(1)(D[r] == 1), r == 1, d), ")"))) +
  xlab("Total number of people (k)") +
  ylab("Days in a year (d)") +
  geom_hline(yintercept = 365, color = colxy, linewidth = 2)+
  geom_abline(slope = 1, color = col365, linewidth = 2)
p4


#######################################
#### Probability  (and count) that at least two people have that Birthday
#### P(D_r \geq 2)
#######################################

z2 <- outer(x, y, function(x, y) {1 - dbinom(0, x, prob = 1/y) - dbinom(1, x, prob = 1/y) } )
data$z2 <- as.vector(z2)
data$e2 <- data$z2 * data$y


p5 <- ggplot(data, aes(x = x, y = y, fill = z2)) +
  geom_raster() +
  scale_fill_gradientn(
    limits = c(0,1),
    colors = prob_colors,
    oob = scales::squish,
    name = "Probability")+
  theme_minimal() +
  theme(#legend.position = "none",
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"))+
  labs(title =expression("P(" * D[r] >=  "2)")) +
  xlab("Total number of people (k)") +
  ylab("Days in a year (d)") +
  geom_hline(yintercept = 365, color = colxy, linewidth = 2)+
  geom_abline(slope = 1, color = col365, linewidth = 2)
p5


p6 <- ggplot(data, aes(x = x, y = y, fill = e2)) +
  geom_raster() +
  scale_fill_gradientn(
    limits = c(0, 500),
    colors = viridis_colors,
    name = "Expected Value")+
  theme_minimal() +
  theme(#legend.position = "none",
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"))+
  labs(title = expression(E * bgroup("(", sum(italic(1)(D[r] >= 2), r == 1, d), ")"))) +
  xlab("Total number of people (k)") +
  ylab("Days in a year (d)") +
  geom_hline(yintercept = 365, color = colxy, linewidth = 2)+
  geom_abline(slope = 1, color = col365, linewidth = 2)
p6



#########################################
### Color blindness check
#########################################
# remotes::install_github("clauswilke/colorblindr")
library(colorblindr)

cvd_grid(p1)
cvd_grid(p2)
cvd_grid(p3)
cvd_grid(p4)
cvd_grid(p5)
cvd_grid(p6)



#######################################
### Variations on the birthday problem
#######################################

# Minimum number of people such that the expected value of the number of days that
# are a shared birthday is at least 1 for d=365: k_min = 29
data %>%
  select(x, y, e2) %>%
  filter(y == 365) %>%
  filter(e2 >=1) %>%
  slice(1)


# Minimum number of people such that the expected value of the number of days that
# are a shared birthday is at least 1/2 for d=365: k_min = 29
data %>%
  select(x, y, e2) %>%
  filter(y == 365) %>%
  filter(e2 >=0.5) %>%
  slice(1)


# Plot of expected number of shared birthdays for first 100 people in the room
# for 365 days in a year
data %>%
  filter(y == 365) %>%
  slice(1:100) %>%
  ggplot() +
  geom_line(aes(x = x, y = e2)) +
  geom_hline(yintercept = 1, color = "red")

# Plot of probability of shared birthdays for 365 days in a year
data %>%
  filter(y == 365) %>%
  ggplot() +
  geom_line(aes(x = x, y = z2))


################################
### Plots for pairs of people
################################
x <- 10:1000
y <- 10:1000
z3<- outer(x, y, function(x, y) {x * (x-1) / y/2 })
z4<- outer(x, y, function(x, y) {1/ y })
data <- expand.grid(x = x, y = y)
data$z3 <- as.vector(z3)


## Numer of people needed to be in a room for the expect count of pairs of people
## with the same birthday to be at leaset 1 for 365 days in a year
data %>%
  filter(y == 365) %>%
  filter(z3 > 1) %>%
  slice(1)

data$z3 <- ifelse(data$z3 < 1, 1, data$z3)
data$z3 <- log(data$z3,base = 10)

data$z4 <- as.vector(z4)

range(data$z4)


p8 <- ggplot(data, aes(x = x, y = y, fill = z3)) +
  geom_raster() +
  scale_fill_viridis_c(
    limits = c(0.000001, 4.2),
    #values = c(10, 100, 1000),
    oob = scales::squish,
    #trans = "log",
    breaks = c(1, 2, 3, 4),
    labels = c(10, 100, 1000, 10000),
    option="mako",
    name = "Expected \nCount")  +
  theme_minimal() +
  theme(
        # legend.position = "none",
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"))+
  labs(title =expression("P(" * D[r] *"=0)")) +
  xlab("Total number of people (k)") +
  ylab("Days in a year (d)") +
  geom_hline(yintercept = 365, color = colxy, linewidth = 2)+
  geom_abline(slope = 1, color = col365, linewidth = 2)
p8




p7 <- ggplot(data, aes(x = x, y = y, fill = z4)) +
  geom_raster() +
  scale_fill_viridis_c(
    limits = c(0.001, 0.1),
    #values = c(10, 100, 1000),
    oob = scales::squish,
    trans = "log10",
    option="rocket",
    breaks = c(0.001, 0.01, 0.1),
    labels = c(expression("\u2264" ~ 0.001), "0.01", expression("\u2265" ~ 0.1)),
    name = "Probability")  +
  theme_minimal() +
  theme(
    # legend.position = "none",
    axis.text.x = element_text(colour="black"),
    axis.text.y = element_text(colour="black"))+
  labs(title =expression("P(" * A[r] *"=0)")) +
  xlab("Total number of people (k)") +
  ylab("Days in a year (d)") +
  geom_hline(yintercept = 365, color = colxy, linewidth = 2)+
  geom_abline(slope = 1, color = col365, linewidth = 2)
p7




#############################
### Additional Plots not included in paper
### Bootstrapping Theoretical limits
#############################

x <- 2:1000
y <- 2:1000
data <- expand.grid(x = x, y = y)

z0<- outer(x, y, function(x, y) {dbinom(0, x, prob = 1/y) })
data$z0 <- as.vector(z0)
data$e0 <- data$z0 * data$y

z1 <- outer(x, y, function(x, y) {dbinom(1, x, prob = 1/y) })
data$z1 <- as.vector(z1)
data$e1 <- data$z1 * data$y


z2 <- outer(x, y, function(x, y) {1 - dbinom(0, x, prob = 1/y) - dbinom(1, x, prob = 1/y) } )
data$z2 <- as.vector(z2)
data$e2 <- data$z2 * data$y

boot.dat <-
  data %>%
  filter(x ==y )

## Question 7
boot.dat %>%
  ggplot() +
  geom_line(aes(x =x , y = z0)) +
  geom_hline(yintercept = exp(-1), col = "blue") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(colour="black"),
    axis.text.y = element_text(colour="black"))+
  ylim(0.0, 1)+
  xlab("n") +
  ylab(expression("P(" * D[i]^"*"==  0 *")"))

boot.dat %>%
  ggplot() +
  geom_line(aes(x =x , y = e0)) +
  geom_abline( slope = exp(-1), col = "blue") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(colour="black"),
    axis.text.y = element_text(colour="black"))+
  #ylim(0.0, 1)+
  xlab("n") +
  ylab(expression("E(" *Sigma[i==1]^n * "(" * 1 * "(" * D[i]^"*"==  0 *"))"))


#Question  8
boot.dat %>%
  ggplot() +
  geom_line(aes(x =x , y = z1)) +
  geom_hline(yintercept = exp(-1), col = "blue") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(colour="black"),
    axis.text.y = element_text(colour="black"))+
  ylim(0.0, 1)+
  xlab("n") +
  ylab(expression("P(" * D[i]^"*"==  1 *")"))

boot.dat %>%
  ggplot() +
  geom_line(aes(x =x , y = e1)) +
  geom_abline( slope = exp(-1), col = "blue") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(colour="black"),
    axis.text.y = element_text(colour="black"))+
  #ylim(0.0, 1)+
  xlab("n") +
  ylab(expression("E(" *Sigma[i==1]^n * "(" * 1 * "(" * D[i]^"*"==  1 *"))"))



#Question  9
boot.dat %>%
  ggplot() +
  geom_line(aes(x =x , y = z2)) +
  geom_hline(yintercept = 1-2*exp(-1), col = "blue") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(colour="black"),
    axis.text.y = element_text(colour="black"))+
  ylim(0.0, 1)+
  xlab("n") +
  ylab(expression("P(" * D[i]^"*" >= 2 *")"))

boot.dat %>%
  ggplot() +
  geom_line(aes(x =x , y = e2)) +
  #geom_abline( slope = 1-2*exp(-1), col = "blue") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(colour="black"),
    axis.text.y = element_text(colour="black"))+
  #ylim(0.0, 1)+
  xlab("n") +
  ylab(expression("E(" *Sigma[i==1]^n * "(" * 1 * "(" * D[i]^"*" >=  2 *"))"))

