library(readr)
library(dplyr)
library(tidyr)
library(countrycode)
library(sf)
library(tmap)

gii_raw <- read_csv("homework4_data/gii_data.csv")

gii_clean <- gii_raw %>%
  filter(indicatorCode == "gii") %>%  
  select(country, year, value)

gii_clean <- gii_clean %>%
  mutate(
    iso3 = countrycode(country, "country.name", "iso3c")
  )

gii_wide <- gii_clean %>%
  filter(year %in% c(2010, 2019)) %>%
  pivot_wider(
    names_from = year,
    values_from = value,
    names_prefix = "gii_"
  ) %>%
  select(country, iso3, gii_2010, gii_2019)


gii_wide <- gii_wide %>%
  mutate(GII_diff = gii_2019 - gii_2010)



world <- st_read("homework4_data/World_Countries_(Generalized)_9029012925078512962.geojson")

world_clean <- world %>%
  mutate(iso3 = countrycode(COUNTRY, "country.name", "iso3c"))


world_gii <- world_clean %>%
  left_join(gii_wide, by = "iso3")


summary(world_gii$gii_2010)
summary(world_gii$gii_2019)
summary(world_gii$GII_diff)


tmap_mode("plot")

tm_shape(world_gii) +
  tm_polygons(
    col = "gii_2010",
    palette = "RdBu",
    style = "quantile",
    title = "GII 2010"
  ) +
  tm_layout(legend.outside = TRUE)


tm_shape(world_gii) +
  tm_polygons(
    col = "gii_2019",
    palette = "RdBu",
    style = "quantile",
    title = "GII 2019"
  ) +
  tm_layout(legend.outside = TRUE)


tm_shape(world_gii) +
  tm_polygons(
    col = "GII_diff",
    palette = "-RdBu",
    style = "quantile",
    title = "GII Change (2010–2019)"
  ) +
  tm_layout(legend.outside = TRUE)

