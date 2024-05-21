library(tidyverse)

pm_2_5 <- readr::read_csv("https://math167r-s24.github.io/static/CA_PM2_5_2023.csv")
head(pm_2_5)
summary(pm_2_5)

#pm_2_5$Date <- as.Date(pm_2_5$Date)


#1.
pm_2_5[which.max(pm_2_5$DAILY_AQI_VALUE),]

#2.???
pm_2_5 |> 
  group_by(`Site Name`) |> 
  summarize(AQI_median = median(DAILY_AQI_VALUE, na.rm = T)) |>
  filter(AQI_median == max(AQI_median))


#3.???
sc_pm <- pm_2_5 |> 
  filter(COUNTY == 'Santa Clara') |>
  summarize(n = length(unique('Site Name')))


typeof(pm_2_5$Date)

pm_2_5 <- pm_2_5 |>
  mutate(Date = mdy(Date))

typeof(pm_2_5$Date)#???????





library(ggplot2)
oct19_pm_2_5 <- pm_2_5 |> filter(Date == "2023-02-19")

ggplot(data = oct19_pm_2_5, 
       mapping = aes(x = SITE_LONGITUDE, 
                     y = SITE_LATITUDE, 
                     color = DAILY_AQI_VALUE)) +
  geom_point() +
  scale_color_viridis_c()



library(sf)
oct19_pm_2_5 <- oct19_pm_2_5 |>
  st_as_sf(
    # what columns hold lon/lat?
    coords = c("SITE_LONGITUDE", "SITE_LATITUDE"), 
    # what projection are we using?
    crs = st_crs(4326)
  ) 

head(oct19_pm_2_5)

ggplot(oct19_pm_2_5) + geom_sf()



library(tigris)
CA_sf <- states() |> filter(NAME == "California")
ggplot() +
  geom_sf(data = CA_sf) +
  geom_sf(data = oct19_pm_2_5, 
          mapping = aes(color = DAILY_AQI_VALUE)) +
  scale_color_viridis_c()




oct19_county_means <- oct19_pm_2_5 |>
  # remove the geometry; we don't need it here
  st_set_geometry(NULL) |> 
  group_by(COUNTY) |>
  summarize(AQI = mean(DAILY_AQI_VALUE))

CA_counties_sf <- counties(state = "CA")

oct19_county_means <- CA_counties_sf |>
  right_join(oct19_county_means, by = c("NAME" = "COUNTY")) 

ggplot() +
  geom_sf(data = CA_sf) +
  geom_sf(data = oct19_county_means, aes(fill = AQI)) +
  scale_fill_viridis_c() 
























