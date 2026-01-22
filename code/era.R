# Create Era Variable for Billboard Data

library(tidyverse)

# Load data
df <- read_csv("billboard_24years_lyrics_spotify.csv") %>%
  mutate(year = as.numeric(year))

# Define music industry eras
df <- df %>%
  mutate(
    Era = case_when(
      year >= 2000 & year <= 2010 ~ "Download Era",
      year >= 2018 & year <= 2023 ~ "Streaming Era",
      TRUE ~ "Transition Period"
    )
  )

# Check era distribution
table(df$Era)
