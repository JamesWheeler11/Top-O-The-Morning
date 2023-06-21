# All packages used in this script:
library(tidyverse)
library(here)
library(withr)
library(sp)
library(sf)
url <- "https://github.com/jonthegeek/apis/raw/main/data/data_ufo_reports_with_day_part.rds"
ufo_path <- withr::local_tempfile(fileext = ".rds")
download.file(url, ufo_path)

ufo_data_original <- readRDS(ufo_path)

US_UFO <- ufo_data_original |>
  filter(country_code == "US")

US_UFO <- US_UFO |>
  mutate(date(reported_date_time))

US_UFO <- US_UFO |>
  rename("ymd" = `date(reported_date_time)`)

US_UFO <- US_UFO |>
  separate(col = ymd, into = c("year2", "month2", "day2"), sep = "-")

plot_usmap(data = per_state_count, regions = "state", values = "count") +
  scale_fill_gradient(low = "aliceblue", high = "purple") +
  geom_point(US_UFO, mapping = aes(x, y)) +
  labs(title = "U.S. Map based on # of UFO Sightings")
  #dean
US_UFO |> 
  group_by(state, city) |>
  summarize(longitude, latitude, count = n())
  
US_UFO <- usmap_transform(US_UFO, c("longitude", "latitude"))
