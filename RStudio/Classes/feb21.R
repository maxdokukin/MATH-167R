library(tidyverse)
table1

relig_income |> pivot_longer(cols = !religion, names_to = 'income', values_to = 'count')

billboard

billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  )

fish_encounters

fish_encounters |> pivot_wider(names_from = station, values_from = seen)


us_rent_income 

us_rent_income |>
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  )



table1
table2 |>   pivot_wider(
  names_from = type,
  values_from = count
)


table4a |> pivot_longer(cols = !country, names_to = 'year', values_to = 'cases')
table1




state_population <- readr::read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/national/totals/nst-est2020.csv")

us_pop <- state_population |> filter(SUMLEV == "010")
head(us_pop)

plot_data <- us_pop |>
  pivot_longer(
    cols = starts_with("POPESTIMATE"),
    names_to = "Year",
    names_prefix = "POPESTIMATE",
    values_to = "Population",
    values_drop_na = TRUE
  )

head(plot_data)

us_pop |> 
  select(-POPESTIMATE042020) |>
  pivot_longer(contains("POPESTIMATE"), 
               names_to = "Year",
               values_to = "Population") |>
  mutate(Year = as.numeric(stringr::str_sub(Year, start = 12))) |>
  ggplot(aes(x = Year, y = Population)) + 
  geom_line()


state_population |> 
  filter(SUMLEV == "020") |>
  select(-POPESTIMATE042020) |>
  pivot_longer(contains("POPESTIMATE"), 
               names_to = "Year",
               values_to = "Population") |>
  mutate(Year = as.numeric(stringr::str_sub(Year, start = 12))) |>
  ggplot(aes(x = Year, y = Population, color = NAME)) + 
  geom_line()




library(openxlsx)
cpi <- read.xlsx("https://thedocs.worldbank.org/en/doc/1ad246272dbbc437c74323719506aa0c-0350012021/original/Inflation-data.xlsx",
                 sheet = 4)
cpi <- cpi[1:203,]
cpi

cpi |>
  pivot_longer(cols = `19701`:`20224`,
               names_to = "Year", 
               values_to = "CPI") |>
  mutate(Year = as.numeric(Year)) |>
  select(Country, Year, CPI)
