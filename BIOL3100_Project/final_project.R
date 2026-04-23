# load necessary packages
library(tidyverse)
library(skimr)
library(janitor)

# Load the data
air_q_dat <- read.csv('data/annual_aqi_by_county_2025.csv')

health_dat <- read.csv('data/health_ineq_final_project.csv')

# Take a look at our data
head(air_q_dat)
head(health_dat)


health_clean <- health_dat %>%
  distinct() %>%
  rename(state_name = statename) %>%
  select(-c(cty, cz, cz_name, cz_pop2000, state_id, stateabbrv))

air_q_clean <- air_q_dat %>%
  distinct() %>%
  clean_names() %>%
  rename(county_name = county, state_name = state) %>%
  group_by(state_name, county_name) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(c(good_days, moderate_days, unhealthy_for_sensitive_groups_days,
                  unhealthy_days, very_unhealthy_days, hazardous_days),
                ~ . / days_with_aqi))

air_q_clean <- air_q_dat %>%
  distinct() %>%
  clean_names() %>%
  rename(county_name = county, state_name = state) %>%
  group_by(state_name, county_name) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(c(good_days, moderate_days, unhealthy_for_sensitive_groups_days,
                  unhealthy_days, very_unhealthy_days, hazardous_days),
                ~ . / days_with_aqi))

join_dat <- left_join(air_q_clean,
                      health_clean,
                      by = c("county_name" = "county_name", "state_name" = "state_name"))
skim(join_dat)
          
join_dat %>%
  ggplot(aes(x = cty_pop2000,
             y = good_days)) +
  geom_point()


join_dat %>%
  filter(state_name %in% c("California", "Texas", "New York", "Utah")) %>%
  ggplot(aes(x = good_days,
             y = le_agg_q1_F)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~state_name)

test <- join_dat %>%
  pivot_longer(cols = starts_with("le_agg"),
               names_to = "group", values_to = "life_expectancy")

join_dat %>%
  mutate(
    le_agg_q1 = (le_agg_q1_F + le_agg_q1_M) / 2,
    le_agg_q2 = (le_agg_q2_F + le_agg_q2_M) / 2,
    le_agg_q3 = (le_agg_q3_F + le_agg_q3_M) / 2,
    le_agg_q4 = (le_agg_q4_F + le_agg_q4_M) / 2
  ) %>%
  select(good_days, le_agg_q1, le_agg_q2, le_agg_q3, le_agg_q4) %>%
  pivot_longer(cols = starts_with("le_agg"),
               names_to = "income_quartile",
               values_to = "life_expectancy") %>%
  mutate(income_quartile = recode(income_quartile,
                                  "le_agg_q1" = "Q1 (Lowest Income)",
                                  "le_agg_q2" = "Q2",
                                  "le_agg_q3" = "Q3",
                                  "le_agg_q4" = "Q4 (Highest Income)"
  )) %>%
  ggplot(aes(x = good_days, y = life_expectancy, color = income_quartile)) +
  geom_smooth(method = "lm") +
  labs(x = "% Good Air Quality Days",
       y = "Avg Life Expectancy",
       color = "Income Quartile",
       title = "Air Quality vs Life Expectancy by Income Quartile")

join_dat %>%
  group_by(state_name) %>%
  summarise(across(c(days_ozone, days_pm2_5, days_co, days_no2), mean, na.rm = TRUE)) %>%
  pivot_longer(-state_name, names_to = "pollutant", values_to = "days") %>%
  ggplot(aes(x = reorder(state_name, days), y = days, fill = pollutant)) +
  geom_col() +
  coord_flip()

# GIS mapping####
library(sf)
library(tigris)
library(janitor)

options(tigris_use_cache = TRUE)

# Get US states shapefile (simpler than counties for a base map)
states_sf <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "MP", "AS"))  # contiguous US only

# Clean monitor locations
locations <- read.csv('data/annual_conc_by_monitor_2025.csv') %>%
  clean_names() %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  distinct(latitude, longitude)  # removes duplicate monitor locations

locations_sf <- st_as_sf(locations, coords = c("longitude", "latitude"), crs = 4326)

# Plot
ggplot() +
  geom_sf(data = states_sf, fill = "gray95", color = "gray60", linewidth = 0.3) +
  geom_sf(data = locations_sf, size = 0.8, alpha = 0.6, color = "purple3") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  labs(title = "EPA Air Quality Monitor Locations (2025)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

# Add good_days back to locations before converting to sf
locations <- read.csv('data/annual_conc_by_monitor_2025.csv') %>%
  clean_names() %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  left_join(air_q_clean %>% select(county_name, state_name, good_days),
            by = c("county_name", "state_name"))

locations_sf <- st_as_sf(locations, coords = c("longitude", "latitude"), crs = 4326)

ggplot() +
  geom_sf(data = states_sf, fill = "gray95", color = "gray60", linewidth = 0.3) +
  geom_sf(data = locations_sf, aes(color = good_days), size = 0.8, alpha = 0.6) +
  scale_color_viridis_c(option = "magma", name = "% Good Days") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  theme_void()


#statistical modeling

mod1 <- glm(data = join_dat, le_agg_q1_F~good_days)
mod2 <- glm(data = join_dat, le_agg_q1_F~good_days+state_name)
summary(mod1)
summary(mod2)

#Does the effect of air quality on life expectancy differ by income quartile
model_dat <- join_dat %>%
  mutate(
    le_q1 = (le_agg_q1_F + le_agg_q1_M) / 2,
    le_q4 = (le_agg_q4_F + le_agg_q4_M) /2
  ) %>%
  pivot_longer(c(le_q1, le_q4), names_to = 'quartile', values_to = 'life_expectancy')

mod1 <- lm(data = model_dat, life_expectancy~quartile)

mod2 <- lm(data = model_dat, life_expectancy~good_days*quartile)

summary(mod1)

summary(mod2)

