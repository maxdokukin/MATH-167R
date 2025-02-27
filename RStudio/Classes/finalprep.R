cal_var <- function(){
  x <- rnorm(n = 10, mean = 1, sd = 10)
  return(var(x))
}

cal_var <- function(){
  x <- rnorm(n = 10, mean = 1, sd = 10)
  return(var(x))
}

results <- replicate(100, cal_var())
mean(results)
f


# get random sample
x <- rnorm(100)

#calculate probability of each observation
probs <- pnorm(x)
probs

# get quitile for 75 percent
qnorm(0.75)

#probability of each observation 
p <- dnorm(x)
p
