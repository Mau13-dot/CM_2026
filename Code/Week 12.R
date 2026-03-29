library(tidyverse)
library(tidymodels)
library(ggdendro)

df_all <- all_top_songs_2018_2025 |>
  rename_with(~ tolower(gsub("[^A-Za-z0-9]+", "_", .x)))

if ("track_name_unique" %in% names(df_all)) {
  df_all <- df_all |>
    mutate(track_label = stringr::str_trunc(track_name_unique, 36))
} else {
  df_all <- df_all |>
    mutate(track_label = stringr::str_trunc(paste0(year, " — ", track_name), 36))
}

df_all <- df_all |>
  mutate(track_label = make.unique(track_label))

output_dir <- "/Users/mauritsvanderdoesdewillebois/CM_2026/docs/Images/All top songs/Week 12"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

make_cluster_visuals <- function(data, label_col, feature_cols, k = 4, linkage = "average") {
  dat <- data |>
    select(all_of(c(label_col, "year", feature_cols))) |>
    drop_na()
  
  rec <- recipe(~ ., data = dat |>
                  select(-all_of(c(label_col, "year")))) |>
    step_center(all_predictors()) |>
    step_scale(all_predictors()) |>
    prep()
  
  xmat <- bake(rec, new_data = dat |>
                 select(-all_of(c(label_col, "year"))))
  
  xmat <- as.data.frame(xmat)
  rownames(xmat) <- dat[[label_col]]
  
  hc <- dist(xmat, method = "euclidean") |>
    hclust(method = linkage)
  
  assignments <- dat |>
    select(all_of(c(label_col, "year"))) |>
    mutate(cluster = factor(cutree(hc, k = k)))
  
  dendro_obj <- ggdendro::dendro_data(hc)
  
  p_dendro <- ggdendrogram(dendro_obj, rotate = FALSE, size = 0.4) +
    theme_classic() +
    labs(
      title = paste("Hierarchical clustering (", linkage, " linkage)", sep = ""),
      x = NULL,
      y = "Height"
    ) +
    theme(
      axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust = 1)
    )
  
  p_cluster_year <- assignments |>
    count(year, cluster) |>
    group_by(year) |>
    mutate(prop = n / sum(n)) |>
    ungroup() |>
    ggplot(aes(x = factor(year), y = prop, fill = cluster)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_viridis_d() +
    theme_classic() +
    labs(
      title = "Cluster membership by year",
      x = "Year",
      y = "Share of songs",
      fill = "Cluster"
    )
  
  p_profile <- dat |>
    left_join(assignments, by = c(label_col, "year")) |>
    group_by(cluster) |>
    summarise(across(all_of(feature_cols), mean, na.rm = TRUE), .groups = "drop") |>
    pivot_longer(-cluster, names_to = "feature", values_to = "value") |>
    ggplot(aes(x = feature, y = cluster, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c() +
    theme_classic() +
    labs(
      title = "Average feature profile of each cluster",
      x = NULL,
      y = "Cluster",
      fill = "Mean value"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  list(
    hc = hc,
    assignments = assignments,
    p_dendro = p_dendro,
    p_cluster_year = p_cluster_year,
    p_profile = p_profile
  )
}

track_features <- c(
  "danceability",
  "energy",
  "loudness",
  "speechiness",
  "acousticness",
  "instrumentalness",
  "liveness",
  "valence",
  "tempo",
  "duration_ms_"
)

cluster_track <- make_cluster_visuals(
  data = df_all,
  label_col = "track_label",
  feature_cols = track_features,
  k = 4,
  linkage = "average"
)

cluster_track$p_dendro
cluster_track$p_cluster_year
cluster_track$p_profile

ggsave(
  filename = file.path(output_dir, "week12_dendrogram_track_features.png"),
  plot = cluster_track$p_dendro,
  width = 12,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "week12_cluster_membership_by_year.png"),
  plot = cluster_track$p_cluster_year,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "week12_cluster_feature_profiles.png"),
  plot = cluster_track$p_profile,
  width = 10,
  height = 5,
  dpi = 300
)
