library(tidyverse)

df_all <- all_top_songs_2018_2025 |>
  rename_with(~ tolower(gsub("[^A-Za-z0-9]+", "_", .x)))

selected_tracks <- tribble(
  ~year, ~track_name_unique,
  2018, "My City",
  2019, "New Light_2019",
  2020, "Blinding Lights_2020",
  2021, "Dolla Dolla Bill",
  2022, "Ojitos Lindos_2022",
  2023, "Daylight",
  2024, "Save Your Tears",
  2025, "NUEVAYoL"
)

plot_df <- df_all |>
  left_join(
    selected_tracks |> mutate(selected = TRUE),
    by = c("year", "track_name_unique")
  ) |>
  mutate(
    selected = if_else(is.na(selected), FALSE, selected),
    label = if_else(selected, paste0(year, " — ", track_name), NA_character_)
  )

ggplot(plot_df, aes(x = factor(year), y = energy)) +
  geom_jitter(
    width = 0.18,
    height = 0,
    colour = "grey75",
    alpha = 0.45,
    size = 2
  ) +
  geom_point(
    data = filter(plot_df, selected),
    aes(size = popularity, colour = factor(year)),
    alpha = 0.95
  ) +
  geom_text(
    data = filter(plot_df, selected),
    aes(label = track_name),
    nudge_x = 0.18,
    hjust = 0,
    size = 3,
    check_overlap = TRUE,
    colour = "black"
  ) +
  scale_colour_viridis_d() +
  scale_size(range = c(3, 6)) +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    plot.margin = margin(5.5, 80, 5.5, 5.5)
  ) +
  labs(
    title = "Selected tracks for timbre analysis within the full corpus",
    subtitle = "Grey points show all submitted tracks; coloured points show the yearly #1 songs used for cepstrograms",
    x = "Year",
    y = "Energy",
    colour = "Year",
    size = "Popularity"
  )