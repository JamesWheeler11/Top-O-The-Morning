z# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2023-06-20')
tuesdata <- tidytuesdayR::tt_load(2023, week = 25)

ufo_sightings <- tuesdata$`ufo_sightings`
places <- tuesdata$`places`
day_parts_map <- tuesdata$`day_parts_map`

# Or read in the data manually

ufo_sightings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')
places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv')
day_parts_map <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/day_parts_map.csv')



day_parts_map |>
  ggplot(aes(rounded_long, rounded_lat)) +
  geom_jitter(alpha = 0.6)

places |>
  ggplot(aes(longitude, latitude)) +
  geom_jitter(alpha = 0.6)

US_ufo_sightings <- ufo_sightings |>
  filter(country_code == "US")

places_in_US <- places |>
  filter(country_code == "US")

US_ufo_sightings <- US_ufo_sightings |>
  group_by(state) |>
  mutate(count = n()) |>
  arrange(desc(count))

US_ufo_sightings |>
  group_by(state, count) |>
  view()

ufo_sightings <- ufo_sightings |>
  separate(col = posted_date, into = c("year", "month", "day"), sep = "-")

US_ufo_sightings <- US_ufo_sightings |>
  separate(col = posted_date, into = c("year", "month", "day"), sep = "-")

US_ufo_sightings |>
  ggplot(aes(state, year, size = duration_seconds)) +
  geom_point(alpha = 0.5)

ufo_sightings |>
  filter(has_images == "TRUE")

US_ufo_sightings <- US_ufo_sightings |>
  mutate(date(reported_date_time))

US_ufo_sightings <- US_ufo_sightings |>
  rename("ymd" = `date(reported_date_time)`)
  
US_ufo_sightings <- US_ufo_sightings |>
  separate(col = ymd, into = c("year2", "month2", "day2"), sep = "-")

US_ufo_sightings |>
  select(city, state, duration_seconds, year2)

US_ufo_sightings |>
  ggplot(aes(year2, duration_seconds)) +
  geom_jitter(alpha = 0.5) +
  scale_y_log10()

per_state_count <- US_ufo_sightings |>
  group_by(state) |>
  summarize(count = n()) 

US_ufo_sightings <- left_join(US_ufo_sightings, places, by = c("city"), relationship = "many-to-many")
  
test <- unique(test)

plot_usmap(data = per_state_count, regions = "state", values = "count") +
  scale_fill_gradient(low = "aliceblue", high = "purple") +
  labs(title = "U.S. Map based on # of UFO Sightings") +
  geom_point(places, mapping = aes(longitude, latitude))

US_ufo_sightings |>
  select(month2, count, state) |>
  group_by(month2)


US_ufo_sightings |>
  ggplot(aes(month2)) +
  geom_bar(color = "black", fill = "green") +
  labs(x = "Month", y = "Number of UFPO Sightings", title = "Apparently it's Hot Alien Summer")

US_ufo_sightings <- US_ufo_sightings |>
  mutate(weekday = wday(reported_date_time_utc, label = TRUE))
        
US_ufo_sightings |>
  group_by(state) |>
  ggplot(aes(weekday)) +
  geom_bar(color = "black", fill = "green")

US_ufo_sightings |>
  group_by(year2, weekday, state, count) |>
  head(n=20000) |>
  ggplot(US_ufo_sightings, mapping = aes(state, count, size = duration_seconds, color = weekday)) +
  geom_point() +
  labs(title = "Alien Prone States and Clarical Variables", x = "State", y = "Number of Sightings")
