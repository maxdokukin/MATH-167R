

mean(sample(c(1:6), 1000, replace = T))



set.seed(1028)
dice_rolls <- sample(1:6, 10000, replace = TRUE)
sample_avg <- sapply(1:length(dice_rolls), function(i) mean(dice_rolls[1:i]))

plot(1:10000, sample_avg, type = "l", col = "red",
     xlab = "Rolls", ylab = "Average")
abline(h = 3.5, col = "blue")


library(ggplot2)
Xbars <- data.frame(Xbar = replicate(1000, mean(rnorm(10, mean = 2, sd = 2))))
ggplot(Xbars, aes(x = Xbar)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_function(fun = dnorm, args = list(mean = 2, sd = 2 / sqrt(10)), color = "red")


n <- 40
Xbars <- data.frame(Xbar = replicate(1000, mean(rexp(n, rate = 1))))
ggplot(Xbars, aes(x = Xbar)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_function(fun = dnorm, args = list(mean = 1, sd = 1 / sqrt(n)), color = "red")





n <- 30 # sample size
mu <- 10 # unknown true mean
sigma <- 2 # known standard deviation
Xbars <- replicate(100, mean(rnorm(n, mean = mu, sd = sigma)))
coverage_rate <- mean(abs(Xbars - mu) < 1.96 * sigma / sqrt(n))
coverage_rate

library(tidyverse)
plot_dat <- data.frame(id = 1:100,
                       Xbar = Xbars) |>
  mutate(upper = Xbar + 1.96 * sigma / sqrt(n),
         lower = Xbar - 1.96 * sigma / sqrt(n))
ggplot(plot_dat, 
       aes(x = id, y = Xbar, 
           color = abs(Xbars - mu) < 1.96 * sigma / sqrt(n))) + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_abline(intercept = mu, slope = 0) + 
  theme(legend.position = "none")

