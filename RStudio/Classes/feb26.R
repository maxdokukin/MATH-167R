library(tidyverse)
band_members
band_instruments


band_members |> inner_join(band_instruments)
band_members |> full_join(band_instruments, by = join_by(name))


library(nycflights13)
data(flights)
data(airlines)
data(weather)
head(flights)

flights |>
  left_join(airlines, by = "carrier")

library(ggplot2)

flights_weather <- flights |> left_join(weather, by = "time_hour")

#ggplot(flights_weather, aes(dep_delay, wind_speed) + geom_point()


library(tidycensus)
Sys.getenv("CENSUS_API_KEY")

# 2021 median income by county
income_2021 <- get_acs(geography = "county", 
                       variables = c(medincome = "B19013_001"), 
                       state = "CA", 
                       year = 2021)

# 2010 median income by county
income_2010 <- get_acs(geography = "county", 
                       variables = c(medincome = "B19013_001"), 
                       state = "CA", 
                       year = 2010)

head(income_2010)
head(income_2021)


income_2021 <- income_2021 |>
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) |>
  select(-moe_medincome) |>
  rename(medincome_2021 = estimate_medincome)
head(income_2021)

# repeat for 2010 data
income_2010 <- income_2010 |>
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) |>
  select(-moe_medincome) |>
  rename(medincome_2010 = estimate_medincome)       

income_2010

income_combined <- income_2010 |>
  left_join(income_2021, by = c("GEOID", "NAME"))
ggplot(income_combined, aes(x = medincome_2010, y = medincome_2021)) +
  geom_point() + 
  geom_abline(slope = 1) + 
  labs(title = "Median income in 2021 vs. 2010, CA Counties",
       subtitle = "American Community Survey",
       y = "2021 Median Income",
       x = "2010 Median Income")




state_population <- readr::read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/national/totals/nst-est2020.csv")
US_pop_tidy <- state_population |>
  filter(SUMLEV == "010") |> 
  select(-POPESTIMATE042020) |>
  pivot_longer(contains("POPESTIMATE"), 
               names_to = "Year",
               values_to = "Population") |>
  mutate(Year = as.numeric(stringr::str_sub(Year, start = 12)))
saveRDS(US_pop_tidy, "/Users/xewe/Documents/Education/BS SJSU/Classes/MATH 167R - R Programming/RStudio/Data/US_pop_tidy.rds")

US_pop_tidy <- readRDS("/Users/xewe/Documents/Education/BS SJSU/Classes/MATH 167R - R Programming/RStudio/Data/US_pop_tidy.rds")


US_pop_plot <- US_pop_tidy |>
  ggplot(aes(x = Year, y = Population)) + 
  geom_line()
saveRDS(US_pop_plot, "/Users/xewe/Documents/Education/BS SJSU/Classes/MATH 167R - R Programming/RStudio/Data/US_pop_plot.rds")

US_pop_plot <- readRDS("/Users/xewe/Documents/Education/BS SJSU/Classes/MATH 167R - R Programming/RStudio/Data/US_pop_plot.rds")
print(US_pop_plot)

pdf(file = "/Users/xewe/Documents/Education/BS SJSU/Classes/MATH 167R - R Programming/RStudio/Data/US_pop_plot.rds",
    width = 8, height = 6) # dimensions in inches
US_pop_plot <- US_pop_tidy |>
  ggplot(aes(x = Year, y = Population)) + 
  geom_line() + 
  ggtitle("US Population, 2010-2020")
print(US_pop_plot)
dev.off()
