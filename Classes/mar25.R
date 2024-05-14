set.seed(12)

take_step <- function() {
  return(sample(c(1, -1), 1))
}
walk_randomly <- function(n_steps, start = 0) {
  if (n_steps <= 1) {
    return(start)
  }
  x <- c(start, 
         walk_randomly(n_steps - 1, start + take_step()))
  return(x)
}

n_steps <- 1000
x <- walk_randomly(n_steps)

plot(1:n_steps, x, type = "l",
     xlab = "Step", ylab = "x",
     main = "A one-dimensional random walk")




take_step_2d <- function() {
  steps <- rbind(c(0, 1),
                 c(0, -1), 
                 c(1, 0),
                 c(-1, 0))
  return(steps[sample(1:4, 1), ])
}
walk_randomly_2d <- function(n_steps, start = c(0, 0)) {
  if (n_steps <= 1) {
    return(start)
  }
  x <- rbind(start, 
             walk_randomly_2d(n_steps - 1, start + take_step_2d()))
  rownames(x) <- NULL
  return(x)
}

coords <-walk_randomly_2d(200)

library(ggplot2)
coords_dat <- data.frame(x = coords[, 1],
                         y = coords[, 2],
                         step = 1:n_steps)
ggplot(coords_dat, aes(x = x, y = y, color = step)) +
  geom_path()


library(devtools)
library(RCurl)
library(httr)
set_config( config( ssl_verifypeer = 0L ) )
devtools::install_github("dgrtwo/gganimate")



library(gganimate)
anim_walk <- ggplot(coords_dat, aes(x = x, y = y, color = step)) +
  geom_path() +
  transition_reveal(along = step)
anim_walk





set.seed(123)
ns <- c(10, 20, 50, 100, 200, 500) 
expected_returns <- 
  vapply(ns, 
         function(n_steps) 
           mean(replicate(1000, sum(walk_randomly(n_steps) == 0) - 1)),
         numeric(1))
plot(ns, expected_returns, type = "l", 
     xlab = "Number of steps", 
     ylab = "Mean returns to x = 0")






steps <- c(10, 100, 1000, 2000, 3000, 4000, 5000)



expected_returns <- 
  vapply(ns, 
         function(steps) 
           mean(replicate(1000, max(abs(walk_randomly(steps))))),
         numeric(1))

plot(ns, expected_returns, type = "l", 
     xlab = "Number of steps", 
     ylab = "Mean dist from x = 0")
