t.test(extra ~ group, data = sleep)

real_data <- data.frame('Flip' = sample(c("H", "T"), 200, replace = T))
head(real_data)

library(tidyverse)
flips <- read_csv("https://math167r-s24.github.io/static/flips.csv")
head(flips)

flips_long <- flips |>
  mutate(id = 1:200) |>
  pivot_longer(-id,  names_to = "Sequence", values_to = "Flip")

head(flips_long)

flips_long |> 
  ggplot(aes(x = id, y = Flip, group = Sequence)) +
  geom_line() +
  geom_point(aes(color = Flip)) +
  facet_grid(rows = vars(Sequence)) + 
  theme(
    legend.position = "none",
    axis.title = element_blank()
  )







real_data <- data.frame(replicate(1000,  sample(c("H", "T"), 200, replace = T)))
head(real_data)
dim(real_data)

######## Longest Streak

long_flip_streaks <- flips_long |>
  group_by(Sequence) |>
  summarize(longest_streak = max(rle(Flip)$length))

long_sim_streaks <- 
  data.frame(
    x = replicate(100000, 
                  max(rle(sample(c("H", "T"), size = 200, replace = T))$length)
    )
  )

ggplot() +
  geom_histogram(data = long_sim_streaks, aes(x = x), binwidth = 1) +
  geom_vline(data = long_flip_streaks,
             aes(color = Sequence, xintercept = longest_streak)) +
  xlab("Longest Streak")

######## Average Streak

avg_flip_streaks <- flips_long |>
  group_by(Sequence) |>
  summarize(avg_streak = mean(rle(Flip)$length))

avg_sim_streaks <- 
  data.frame(
    x = replicate(100000, 
                  mean(rle(sample(c("H", "T"), size = 200, replace = T))$length)
    )
  )

ggplot() +
  geom_histogram(data = avg_sim_streaks, aes(x = x)) +
  geom_vline(data = avg_flip_streaks,
             aes(color = Sequence, xintercept = avg_streak)) +
  xlab("Average Streak")

######## Transition Count

trans_counts <- flips_long |>
  group_by(Sequence) |>
  summarize(transitions = length(rle(Flip)$length))

trans_counts_sim <- 
  data.frame(
    x = replicate(100000, 
                  length(rle(sample(c("H", "T"), size = 200, replace = T))$length)
    )
  )

ggplot() +
  geom_histogram(data = trans_counts_sim, aes(x = x)) +
  geom_vline(data = trans_counts,
             aes(color = Sequence, xintercept = transitions)) +
  xlab("H/T Transitions")

########

p_vals <- sapply(flip_streaks$longest_streak,
                 function (x) mean(sim_streaks$x > x))
p_vals



rle_res <- rle(c("H", "T", "T", "H", "H", "H", "H", "H", "T", "H"))
length(rle_res$lengths)