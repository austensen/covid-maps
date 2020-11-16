library(tidyverse)
library(tidycensus)
library(lubridate)
library(sf)

url_base <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/master"

census_api_key("c32dfbf7d25fe9558fd11bf021780457970f96ff")


# Reshape Test Rate Data by ModZCTA ---------------------------------------

tests_raw <- str_c(url_base, "/trends/testrate-by-modzcta.csv") %>% 
  read_csv(col_types = cols(week_ending = col_date("%m/%d/%Y")))

tests_long <- tests_raw %>% 
  pivot_longer(-week_ending, names_to = "modzcta", values_to = "test_rate") %>% 
  mutate(modzcta = str_extract(modzcta, "\\d+")) %>% 
  filter(!is.na(modzcta))


# Get ACS Data by ModZCTA -------------------------------------------------

zcta_modzcta_xwalk <- str_c(url_base, "/Geography-resources/ZCTA-to-MODZCTA.csv") %>% 
  read_csv(col_types = "cc") %>% 
  rename_with(str_to_lower)

acs_raw <- get_acs(
  "zcta",
  variables = c(
    "gross_rent_med" = "B25064_001", # median gross rent
    "hh_inc_med" = "B19013_001", # median household income
    "rent_burden_med" = "B25071_001", # median rent burden
    "pov_pct" = "C17002_001", # poverty rate
    "hh_size_avg" = "B25010_001", # average hosehold size
    "pop_ge65_" = str_c("B01001_", str_pad(c(20:25, 44:49), 3, "left", "0"))
  )
)

acs_clean <- acs_raw %>% 
  rename(zcta = GEOID) %>% 
  inner_join(zcta_modzcta_xwalk, by = "zcta") %>% 
  # make all the age 65+ rows have the name value for the "variable" column
  mutate(variable = str_remove(variable, "_\\d+$")) %>% 
  group_by(modzcta, variable) %>% 
  summarise(
    e = sum(estimate, na.rm = TRUE),
    moe = moe_sum(moe, estimate, na.rm = TRUE)
  ) %>% 
  pivot_longer(c(e, moe), names_to = "var_type") %>% 
  unite(name, variable, var_type) %>% 
  pivot_wider(id_cols = modzcta, names_from = name, values_from = value)
  
# Join All Data with Geometries -------------------------------------------


modzcta_geos <- str_c(url_base, "/Geography-resources/MODZCTA_2010_WGS1984.geo.json") %>% 
  read_sf() %>% 
  rename_with(str_to_lower)

covid_nyc_modzcta <- modzcta_geos %>% 
  left_join(tests_long, by = "modzcta") %>% 
  left_join(acs_clean, by = "modzcta")
  
write_sf(covid_nyc_modzcta, str_glue("data/covid_nyc_modzcta_{Sys.Date()}.geojson"))


# Mapping with ggplot2 ----------------------------------------------------

library(gganimate)
library(gifski)
library(transformr)

covid_nyc_modzcta %>%
  filter(week_ending == "2020-11-07 ") %>% 
  st_transform(2263) %>%
  ggplot() +
  geom_sf(aes(fill = test_rate), color = "white", size = 0.05) +
  scale_fill_viridis_c(labels = scales::comma) +
  theme_void() +
  theme(legend.position = c(0.2, 0.8)) +
  labs(
    title = "Rate of PCR testing per 100,000 people, by week",
    subtitle = "Week ending: November 7, 2020",
    fill = NULL,
    caption = "Source: NYC Health Department via github.com/nychealth/coronavirus-data (trends/testrate-by-modzcta.csv)
    Note: The dates reflect the date of specimen collection"
  )

ggsave("img/covid-testrate_map-single.png", width = 9, height = 6)

covid_nyc_modzcta %>%
  filter(week_ending > "2020-10-01") %>% 
  st_transform(2263) %>%
  ggplot() +
  geom_sf(aes(fill = test_rate), color = "white", size = 0.05) +
  scale_fill_viridis_c(labels = scales::comma) +
  theme_void() +
  facet_wrap(~week_ending, labeller = as_labeller(function(x) stamp_date('November 1, 2020')(ymd(x)))) +
  labs(
    title = "Rate of PCR testing per 100,000 people, by week",
    subtitle = "",
    fill = NULL,
    caption = "Source: NYC Health Department via github.com/nychealth/coronavirus-data (trends/testrate-by-modzcta.csv)
    Note: The dates reflect the date of specimen collection"
  )

ggsave("img/covid-testrate_map-facet.png", width = 9, height = 6)

covid_testrate_map <- covid_nyc_modzcta %>%
  st_transform(2263) %>%
  ggplot() +
  geom_sf(aes(fill = test_rate), color = "white", size = 0.05) +
  scale_fill_viridis_c(labels = scales::comma) +
  transition_states(week_ending) +
  theme_void() +
  theme(legend.position = c(0.2, 0.8)) +
  labs(
    title = "Rate of PCR testing per 100,000 people, by week",
    subtitle = "Week ending: {stamp_date('November 1, 2020')(ymd(closest_state))}",
    fill = NULL,
    caption = "Source: NYC Health Department via github.com/nychealth/coronavirus-data (trends/testrate-by-modzcta.csv)
    Note: The dates reflect the date of specimen collection"
  )

animate(covid_testrate_map, width = 900, height = 600)
anim_save("img/covid-testrate_map-anim.gif")
  