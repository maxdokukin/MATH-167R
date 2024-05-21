run_one_sim <- function(seed, n) {
  
  set.seed(seed)
  flips <- sample(x = c("H", "T"), size = 1000, replace = T)
  HH_score <- 0
  HT_score <- 0
  for (i in 2:n) {
    if (flips[i-1] == "H" & flips[i] == "H") {
      HH_score <- HH_score + 1
    } 
    if (flips[i-1] == "H" & flips[i] == "T") {
      HT_score <- HT_score + 1
    }
  }
  return(list(HH = HH_score, HT = HT_score))
}
# run 100000 simulations with n = 100
results <- lapply(1:10000, run_one_sim, n = 1000)

HH_scores <- sapply(results, function(x) x$HH)
HT_scores <- sapply(results, function(x) x$HT)

mean(HH_scores > HT_scores)
mean(HH_scores < HT_scores)
mean(HH_scores == HT_scores)






samples <- rnorm(n = 1000000, mean = 10, sd = 20)

library(ggplot2)

ggplot() +
  geom_histogram(aes(x = samples), 
                 alpha = .5, binwidth = 1) +
  ggtitle("Histogram of scores for Alice and Bob") +
  xlab("Score")




2 * (1 - pnorm(q = 1.24, mean = 0, sd = 1))#p value two tails
#q is a z score, pnorm() returns p value
2 * pnorm(q = 2.12, mean = 0, sd = 1)


#If I flip a coin with probability of heads = 0.6 ten times, what is the probability of observing exactly 6 heads?
dbinom(x=6, size=10, prob=.6)

#If I flip a coin with probability of heads = 0.6 ten times, what is the probability of observing 6 or fewer heads?
pbinom(6, size=10, prob=.6, lower.tail=TRUE)

#Consider an experiment where you flip a coin with a probability of heads = .6 ten times. 
#Simulate this experiment 1000 times and create a histogram of your results.

flips <- rbinom(n = 1000, size = 10, prob = .6)

ggplot() +
  geom_histogram(aes(x = flips), 
                 alpha = .5, binwidth = 1) +
  ggtitle("Histogram of scores for Alice and Bob") +
  xlab("Score")

mean(flips)

