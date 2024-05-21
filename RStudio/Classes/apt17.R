carrier <- rep(c(0,1), c(100,200))

# an example where y is independent of the gene
null_y <- rnorm(300) 
# an example where y is dependent on the gene
alt_y <- rnorm(300, mean = carrier * 5) 


t.test(null_y[carrier == 0], null_y[carrier == 1], var.equal=TRUE)
t.test(alt_y[carrier == 0], alt_y[carrier == 1], var.equal=TRUE)

set.seed(1)
null_diff <- mean(null_y[carrier==1]) - mean(null_y[carrier==0])
single_test <- function(label, y) {
  resample <- sample(label)
  # resample test statistic
  mean(y[resample == 1]) - mean(y[resample == 0])
}
test_stats_null <- replicate(1000, single_test(carrier, null_y))



hist(test_stats_null)
abline(v = null_diff, lwd=2, col="purple")

mean(abs(test_stats_null) > abs(null_diff)) # P-value




set.seed(1)
alt_diff <- mean(alt_y[carrier==1]) - mean(alt_y[carrier==0])
test_stats_alt <- replicate(1000, single_test(carrier, alt_y))
hist(test_stats_alt, xlim = c(-0.5, 6))
abline(v = alt_diff, lwd=2, col="purple")
mean(abs(test_stats_alt) > abs(alt_diff)) # P-value






head(sleep)
test_stat <- mean(sleep$extra[sleep$group == 1]) - 
  mean(sleep$extra[sleep$group == 2])

single_test <- function(label, y) {
  resample <- sample(label)
  # resample test statistic
  mean(y[resample == 1]) - mean(y[resample == 2])
}

test_stats_alt <- replicate(1000, single_test(sleep$group, sleep$extra))
hist(test_stats_alt)
abline(v = test_stat, lwd=2, col="purple")

mean(abs(test_stats_alt) > abs(test_stat)) # P-value
#p value is the probobility of seing data that is more extreme what we got. 
#in this case everything left of -1.5 and right of 1.5


run_one_sim <- function(seed){
  
  set.seed(seed)
  x <- rnorm(10, mean=1, sd=1)
  res <- t.test(x)
  p_val <- res$p.value
  return(p_val < 0.05)
}


mean(sapply(1:10000, run_one_sim))
#percent of time we reject H0
#if mean is set lower, than more data needed to establish that effect 
#is actually present





