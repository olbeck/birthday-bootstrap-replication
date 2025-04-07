library(purrr)
library(dplyr)
library(ggplot2)
library(furrr)
library(tidyr)
library(tictoc)

## Function to record events of interest for a given
##  number of samples and bootstrap replications

get_results <- function(x){

  n <- x$n[1]
  B <- x$B[1]

  boot.samps <- matrix(sample(1:n, n*B, replace = TRUE),
                       ncol = n, nrow = B)


  tab <- data.frame(
    number.unique = rep(NA, B),
    number.repeated = 0,
    number.nonrepeated = 0,
    no.show=0,
    match.pair = 0,
    diff.pair=0
  )
  for(b in 1:B){
    #bootstrap sample
    temp.samp <- boot.samps[b, ]

    # things I want to konw about individual items
    tab.temp <- table(factor(temp.samp, levels = 1:n))
    tab$number.unique[b] <- length(tab.temp[tab.temp > 0])
    tab$number.nonrepeated[b] <- length(tab.temp[tab.temp==1])
    tab$number.repeated[b] <- length(tab.temp[tab.temp>1])
    tab$no.show[b] <- length(tab.temp[tab.temp==0])

    #things I want to know about paird items
    temp.combos <- combn(temp.samp, 2)
    tab$match.pair[b] <- sum(temp.combos[1, ] == temp.combos[2, ])
    tab$diff.pair[b] <- sum(temp.combos[1, ] != temp.combos[2, ])

  }
  tab$n <- n
  tab$B <- B
  return(as.list(tab))
}

## Set up parallelization
num.cores <- parallel::detectCores() - 1
plan(multisession, workers = num.cores)


# Set up experiment conditions
ns <- seq(25, 1000, by = 25)
Bs <- c(5000)
opts <- expand.grid(n=ns, B=Bs)


# Run simulation study
tic()

results <-
  opts %>%
  split(~n+B) %>%
  future_map( ~ get_results(.x), .options = furrr_options(seed = 36987)) %>%
  future_map(~ as.data.frame(.x)) %>%
  bind_rows()

toc()
# save results

save(results,opts, file = "sim-study-results.Rdata")

##############################
### Plots
###################################
load("sim-study-results.Rdata")

plot.dat <- results %>%
  mutate(is.perm = number.unique==n)

# no simulations were permutations - across all n
plot.dat %>%
  filter(is.perm)


###########################
### Proportions
###########################

## A. Proportion of original sample elements that do not show up in the bootstrap sample
plot.dat %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(n), y = no.show/n), coef = 10) +
  geom_hline(yintercept = exp(-1), col = "#357BA2FF", size = 2) +
  theme_minimal() +
  ylim(0,0.75) +
  scale_x_discrete(breaks = seq(100,1000, by = 200))+
  ggtitle("Proportion of original sample elements that are not present in the bootstrap sample",
          subtitle = "Limit =  1/e") +
  xlab("Sample Size (n)") +
  ylab("Proportion")



## B. Proportion of original sample elements that show up exactly once in the bootstrap sample
plot.dat %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(n), y = number.nonrepeated/n), coef = 10) +
  geom_hline(yintercept = exp(-1), col = "#357BA2FF", size = 2) +
  theme_minimal() +
  ylim(0,0.75) +
  scale_x_discrete(breaks = seq(100,1000, by = 200))+
  ggtitle("Proportion of original sample elements that are present in the bootstrap sample exactly once",
          subtitle = "Limit =  1/e") +
  xlab("Sample Size (n)") +
  ylab("Proportion")



## C. Proportion of bootstrap samples elemets that match atleast one other element from that bootstrap sample

plot.dat %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(n), y = number.repeated/n), coef = 10) +
  geom_hline(yintercept = 1-2*exp(-1), col = "#357BA2FF", size = 2) +
  theme_minimal() +
  ylim(0,0.75) +
  scale_x_discrete(breaks = seq(100,1000, by = 200))+
  ggtitle("Proportion of elements in the bootstrap sample that match at least one other element",
          subtitle = "Limit = 1 - 2/e") +
  xlab("Sample Size (n)") +
  ylab("Proportion")


####################################
### Counts
###################################

ecount0  <-
  data.frame(n = unique(opts$n)) %>%
  mutate(e.prop = ((n-1)/n)^n) %>%
  mutate(e.count = e.prop * n)

ecount1  <-
  data.frame(n = unique(opts$n)) %>%
  mutate(e.prop = ((n-1)/n)^n) %>%
  mutate(e.count = e.prop * n)


ecount2  <-
  data.frame(n = unique(opts$n)) %>%
  mutate(e.prop = 1-2*((n-1)/n)^n) %>%
  mutate(e.count = e.prop * n)

## A. Count of original sample elements that do not show up in the bootstrap sample
plot.dat %>%
  mutate(n = as.factor(n)) %>%
  ggplot() +
  geom_boxplot(data = plot.dat, aes(x = n, group = n, y = no.show), coef = 10) +
  geom_line(data = ecount0, aes(group =1, x=n, y=e.count), color = "#357BA2FF", size = 1.5, alpha = 0.75)+
  theme_minimal() +
  ylim(0,430) +
  scale_x_continuous(breaks = seq(100,1000, by = 200))+
  ggtitle("Count of original sample elements that are not present in the bootstrap sample",
          subtitle = "Expected Count =  n* (((n-1)/n)^n)") +
  xlab("Sample Size (n)") +
  ylab("Count")



## B. Proportion of original sample elements that show up exactly once in the bootstrap sample
plot.dat %>%
  mutate(n = as.factor(n)) %>%
  ggplot() +
  geom_boxplot(data = plot.dat, aes(x = n, group = n, y = number.nonrepeated), coef = 10) +
  geom_line(data = ecount1, aes(group =1, x=n, y=e.count), color = "#357BA2FF", size = 1.5, alpha = 0.75)+
  theme_minimal() +
  ylim(0,430) +
  scale_x_continuous(breaks = seq(100,1000, by = 200))+
  ggtitle("Proportion of original sample elements that are present in the bootstrap sample exactly once",
          subtitle = "Expected Count =  n* (((n-1)/n)^n)") +
  xlab("Sample Size (n)") +
  ylab("Count")



## C. Proportion of bootstrap samples elemets that match atleast one other element from that bootstrap sample
plot.dat %>%
  mutate(n = as.factor(n)) %>%
  ggplot() +
  geom_boxplot(data = plot.dat, aes(x = n, group = n, y = number.repeated), coef = 10) +
  geom_line(data = ecount2, aes(group =1, x=n, y=e.count), color = "#357BA2FF", size = 1.5, alpha = 0.75)+
  theme_minimal() +
  ylim(0,430) +
  scale_x_continuous(breaks = seq(100,1000, by = 200))+
  ggtitle("Proportion of elements in the bootstrap sample that match at least one other element",
          subtitle = "Expected Count = n* (1-2*((n-1)/n)^n)") +
  xlab("Sample Size (n)") +
  ylab("Count")



#################################
## Pairs
#################################
ecount6  <-
  data.frame(n = unique(opts$n)) %>%
  mutate(e.prop = 1/n) %>%
  mutate(e.count = (n-1)/2)


plot.dat2 <- plot.dat %>%
  mutate(n2 = as.factor(n),
         pair = match.pair/choose(n,2))

ggplot() +
  geom_boxplot(data = plot.dat2, aes(x = n, group = n, y = pair), coef = 10) +
  geom_line(data = ecount6, aes( x = n, y= e.prop), color = "#357BA2FF", size = 1, alpha = 0.75) +
  theme_minimal() +
  ylim(0,0.11) +
  scale_x_continuous(breaks = seq(100,1000, by = 200))+
  ggtitle("Proportion of pairs of elements in the bootstrap sample that correspond to the same sample element",
          subtitle = "Limit = 1 /n") +
  xlab("Sample Size (n)") +
  ylab("Proportion")


plot.dat %>%
  ggplot() +
  geom_boxplot(aes(group = n, x = (n), y = match.pair), coef = 10) +
  geom_line(data = ecount6, aes( x = n, y= e.count), color = "#357BA2FF", size = 1, alpha = 0.75) +
  scale_x_continuous(breaks = seq(100,1000, by = 200))+
  theme_minimal()+
  ggtitle("Count of pairs of elements in the bootstrap sample that correspond to the same sample element",
          subtitle = "Expected Count = (n-1)/2" )+
  xlab("Sample Size (n)") +
  ylab("Count")






###### Extra ------------------------------------------


plot.dat %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(n), y = number.repeated), coef = 10) +
  geom_point(data = ecount2, aes(x = as.factor(n), y= e.count), color = "blue") +
  facet_wrap(~B)+
  ggtitle("Number of elements in the bootstrap sample that match at least one other element",
          subtitle = "Expected Count = n* (1-2*((n-1)/n)^n)") +
  xlab("Sample Size (n)") +
  ylab("count")



## 3. Proportion of elements in the bootstrap sample that show up exactly once
plot.dat %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(n), y = number.nonrepeated/n), coef = 10) +
  geom_hline(yintercept = exp(-1), col = "blue") +
  facet_wrap(~B)+
  ggtitle("Proportion of original sample elements that are present in the bootstrap sample exactly once",
          subtitle = "Limit =  1/e") +
  xlab("Sample Size (n)") +
  ylab("proportion")



plot.dat %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(n), y = number.nonrepeated), coef = 10) +
  geom_point(data = ecount3, aes(x = as.factor(n), y= e.count), color = "blue") +
  facet_wrap(~B)+
  ggtitle("Number of original sample elements that are present in the bootstrap sample exactly once",
          subtitle = "Expected Count = n* (((n-1)/n)^n)") +
  xlab("Sample Size (n)") +
  ylab("count")


# 4. Proportion of the original sample elements that do not appear in the bootstrap sample (e^-1)
plot.dat %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(n), y = no.show/n), coef = 10) +
  geom_hline(yintercept = exp(-1), col = "blue") +
  facet_wrap(~B)+
  ggtitle("Proportion of original sample elements that are not present in the bootstrap sample",
          subtitle = "Limit =  1/e") +
  xlab("Sample Size (n)") +
  ylab("proportion")



plot.dat %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(n), y = no.show), coef = 10) +
  geom_point(data = ecount4, aes(x = as.factor(n), y= e.count), color = "blue") +
  facet_wrap(~B)+
  ggtitle("Number of original sample elements that are not present in the bootstrap sample",
          subtitle = "Expected Count = n* (((n-1)/n)^n)") +
  xlab("Sample Size (n)") +
  ylab("count")


## 5. Proportion of elements in the bootstrap sample that are unique (i.e. proportion of the original sample that show up in the bootstrap sample)
plot.dat %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(n), y = number.unique/n), coef = 10) +
  geom_hline(yintercept = 1-exp(-1), col = "blue") +
  facet_wrap(~B)+
  ggtitle("Proportion of original sample elements that are present in the bootstrap sample",
          subtitle = "Limit = 1 - 1/e") +
  xlab("Sample Size (n)") +
  ylab("proportion")


ecount5  <-
  data.frame(n = unique(opts$n)) %>%
  mutate(e.prop = 1-((n-1)/n)^n) %>%
  mutate(e.count = e.prop * n)

plot.dat %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(n), y = number.unique), coef = 10) +
  geom_point(data = ecount5, aes(x = as.factor(n), y= e.count), color = "blue") +
  facet_wrap(~B)+
  ggtitle("Number of original sample elements that are present in the bootstrap sample",
          subtitle = "Expected Count = n* (1-((n-1)/n)^n)") +
  xlab("Sample Size (n)") +
  ylab("count")



## 6. Proporiton of (X_i*, X_j*) s.t. X_i* = X_j* (1/n)

ecount6  <-
  data.frame(n = unique(opts$n)) %>%
  mutate(e.prop = 1/n) %>%
  mutate(e.count = (n-1)/2)

plot.dat %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(n), y = match.pair/choose(n,2)), coef = 10) +
  facet_wrap(~B)+
  geom_point(data = ecount6, aes(x = as.factor(n), y= e.prop), color = "blue") +
  ggtitle("Proportion of pairs of elements in the bootstrap sample that correspond to the same sample element",
          subtitle = "Limit = 1 /n") +
  xlab("Sample Size (n)") +
  ylab("proportion")


plot.dat %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(n), y = match.pair), coef = 10) +
  facet_wrap(~B)+
  geom_point(data = ecount6, aes(x = as.factor(n), y= e.count), color = "blue") +
  ggtitle("Count of pairs of elements in the bootstrap sample that correspond to the same sample element",
          subtitle = "Limit = (n-1)/2" )+
  xlab("Sample Size (n)") +
  ylab("count")




## 1. Proportion of bootstrap samples that are a permutation of the original sample

plot.dat %>%
  group_by(n,B) %>%
  summarise(p.perm = mean(is.perm)) %>%
  ggplot() +
  geom_point(aes(x = n, y = p.perm)) +
  theme_minimal() +
  ggtitle("Average number of bootstrap samples that were a permutation of the original sample") +
  xlab("Sample Size (n)") +
  ylab("proportion")

