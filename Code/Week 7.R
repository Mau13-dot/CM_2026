library(tidyverse)

df_all <- all_top_songs_2018_2025 |>
  rename_with(~ tolower(gsub("[^A-Za-z0-9]+", "_", .x)))

# Tempo distribution across all years
ggplot(df_all, aes(x = tempo)) +
  geom_histogram(binwidth = 10, fill = "steelblue", colour = "white") +
  theme_minimal() +
  labs(
    title = "Tempo distribution across my top songs, 2018–2025",
    x = "Tempo (BPM)",
    y = "Number of songs"
  )

# Tempo by year
ggplot(df_all, aes(x = factor(year), y = tempo)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal() +
  labs(
    title = "Tempo by year",
    x = "Year",
    y = "Tempo (BPM)"
  )

# Valence by year
ggplot(df_all, aes(x = factor(year), y = valence)) +
  geom_boxplot(fill = "tomato") +
  theme_minimal() +
  labs(
    title = "Valence by year",
    x = "Year",
    y = "Valence"
  )

# Danceability vs energy
ggplot(df_all, aes(x = danceability, y = energy, colour = factor(year))) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Danceability and energy across my top songs",
    x = "Danceability",
    y = "Energy",
    colour = "Year"
  )

# Richer overview plot
ggplot(df_all, aes(x = valence, y = energy, colour = danceability, size = tempo)) +
  geom_point(alpha = 0.7) +
  scale_colour_viridis_c() +
  theme_minimal() +
  labs(
    title = "Audio-feature overview of my top songs, 2018–2025",
    x = "Valence",
    y = "Energy",
    colour = "Danceability",
    size = "Tempo"
  )
