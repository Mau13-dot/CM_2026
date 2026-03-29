library(tidyverse)
library(compmus)

my_city <- read_csv("/Users/mauritsvanderdoesdewillebois/CM_2026/Data/My city/Week 8.csv", show_col_types = FALSE)
new_light <- read_csv("/Users/mauritsvanderdoesdewillebois/CM_2026/Data/New light/Week 8.csv", show_col_types = FALSE)
blinding_lights <- read_csv("/Users/mauritsvanderdoesdewillebois/CM_2026/Data/Blinding lights/Week 8.csv", show_col_types = FALSE)
dolla_dolla_bill <- read_csv("/Users/mauritsvanderdoesdewillebois/CM_2026/Data/Dolla Dolla Bill/Week 8.csv", show_col_types = FALSE)
ojitos_lindos <- read_csv("/Users/mauritsvanderdoesdewillebois/CM_2026/Data/Ojitos lindos/Week 8.csv", show_col_types = FALSE)
daylight <- read_csv("/Users/mauritsvanderdoesdewillebois/CM_2026/Data/Daylight/Week 8.csv", show_col_types = FALSE)
save_your_tears <- read_csv("/Users/mauritsvanderdoesdewillebois/CM_2026/Data/Save your tears/Week 8.csv", show_col_types = FALSE)
nuevayol <- read_csv("/Users/mauritsvanderdoesdewillebois/CM_2026/Data/Nuevayol/Week 8.csv", show_col_types = FALSE)

my_city |>
  compmus_wrangle_chroma() |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(title = "2018 — My City", x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

new_light |>
  compmus_wrangle_chroma() |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(title = "2019 — New Light", x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

blinding_lights |>
  compmus_wrangle_chroma() |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(title = "2020 — Blinding Lights", x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

dolla_dolla_bill |>
  compmus_wrangle_chroma() |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(title = "2021 — Dolla Dolla Bill", x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

ojitos_lindos |>
  compmus_wrangle_chroma() |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(title = "2022 — Ojitos Lindos", x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

daylight |>
  compmus_wrangle_chroma() |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(title = "2023 — Daylight", x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

save_your_tears |>
  compmus_wrangle_chroma() |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(title = "2024 — Save Your Tears", x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

nuevayol |>
  compmus_wrangle_chroma() |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(title = "2025 — NUEVAYoL", x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()