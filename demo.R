library(tidyverse)
library(tidycensus)
library(lubridate)
library(sf)

# We'll be downloading multiple files from the NYC Health dept GitHub repo, and
# so we can save the beginning part of the URL that all the files have in common
# so our links are more readable
url_base <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/master"

# We'll be getting Census ACS data again, so need to input your API key
census_api_key("c32dfbf7d25fe9558fd11bf021780457970f96ff")


# Reshape Test Rate Data by ModZCTA ---------------------------------------

# Download covid testing data, specifying the formatting of the date column, and
# letting read_csv() guess the rest (numeric, in this case)
tests_raw <- str_c(url_base, "/trends/testrate-by-modzcta.csv") %>% 
  read_csv(col_types = cols(week_ending = col_date("%m/%d/%Y")))

# The data is not in "tidy" format. Every row should be an observation and every
# column a variable, but here we have the "modzcta" variable mixed into the
# column names. So we need to pivot_longer to get that info into its own column
# and extract the modzcta code from the rest of the column name
tests_long <- tests_raw %>% 
  pivot_longer(-week_ending, names_to = "modzcta", values_to = "test_rate") %>% 
  mutate(modzcta = str_extract(modzcta, "\\d+")) %>% 
  filter(!is.na(modzcta))


# Get ACS Data by ModZCTA -------------------------------------------------

# The geography used in the covid test data is a modified version of ZCTAs that
# groups some together, so we'll need this "crosswalk" file to tell us how to go
# from regular ZCTA to these MODZCTAs (there is no splitting, so we don't have
# to worry about allocation splitting like in Lucy's example today).
zcta_modzcta_xwalk <- str_c(url_base, "/Geography-resources/ZCTA-to-MODZCTA.csv") %>% 
  read_csv(col_types = "cc") %>% 
  rename_with(str_to_lower)

# Download some basic ACS data like we've done before. To create a variable for
# population aged 65+ we need to get many variables (rows) from a table, later
# we'll sum these up). Because of the need to sum these up we'll download in "tidy" rather than "wide" format, and later we'll aggregate and Because ZCTAs don't nest nicely within other geographies,
# we need to download them for all US then filter down some other way.
acs_raw <- get_acs(
  "zcta",
  variables = c(
    "gross_rent_med" = "B25064_001", # median gross rent
    "hh_inc_med" = "B19013_001", # median household income
    "rent_burden_med" = "B25071_001", # median rent burden
    "pov_pct" = "C17002_001", # poverty rate
    "hh_size_avg" = "B25010_001", # average household size
    "pop_ge65_" = str_c("B01001_", str_pad(c(20:25, 44:49), 3, "left", "0"))
  ),
  output = "tidy"
)

# Here is a bunch of cleaning and aggregating code. We inner join with our
# zcta/modzcta crosswalk to both filter down to just NYC and bring in the
# modzcta that we'll use for group/summarize later. We
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

# Use read_sf() to downloadand import spatial data file (geojson) for the
# boundaries of the modzctas as a spatial data frame
modzcta_geos <- str_c(url_base, "/Geography-resources/MODZCTA_2010_WGS1984.geo.json") %>% 
  read_sf() %>% 
  rename_with(str_to_lower)

# Start with these boundaries and joinon our covid testing and acs data by
# modzcta IDs
covid_nyc_modzcta <- modzcta_geos %>% 
  left_join(tests_long, by = "modzcta") %>% 
  left_join(acs_clean, by = "modzcta")

# Since we will now have repeated rows for every week of testing, whe we export
# fr QGIS it'll be best to filter to just one week, so that we have only one row
# per modzcta. 
covid_nyc_modzcta %>% 
  filter(week_ending == "2020-11-07") %>% 
  write_sf(str_glue("data/covid_nyc_modzcta_{Sys.Date()}.geojson"))


# Mapping with ggplot2 ----------------------------------------------------

# Now we'll see how you can do some mapping within R using the same ggplot2
# package you've used for graphs.

# We'll also make a fun GIF and so need some extra packages for that
library(gganimate)
library(gifski)
library(transformr)

# Simple static map of one week of data
covid_nyc_modzcta %>%
  filter(week_ending == "2020-11-07") %>% 
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

# All the same code as above, but we'll keep a few weeks wen we filter and we'll
# add facet_wrap(~week_ending) to make a "small multiples" plot
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

# Now we'll again use all thesame code but keep all the weeks in the dataset and
# add transition_states(week_ending) to make this an animation for all the weeks
# of data
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

# Once we've created the annimation info we need to annimate it. Here oyu can
# control frame rate, size, etc.
animate(covid_testrate_map, width = 900, height = 600)

# And we cna now export to a GIF file
anim_save("img/covid-testrate_map-anim.gif")
  