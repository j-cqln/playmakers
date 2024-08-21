library(readr)
library(dplyr)
library(factoextra)
library(cluster)

# Load ranks data
ranks <- read.csv("data/ranks.csv")
years <- c(2021, 2022, 2023)

box_results <- NULL
results <- NULL

# Scale data
ranks_clustering <- ranks %>%
  mutate(avg_ice_time = ice_time / games_played) %>%
  select(season,
         shooter_id,
         goals,
         primary_assists, secondary_assists,
         shot_attempts, shots_on_goal,
         blocked_shot_attempts, missed_shots,
         avg_ice_time,
         total_xg, avg_xg,
         total_xrebounds, avg_xrebounds,
         total_rebound_value, avg_rebound_value
  )

# Quality
for (year in years) {
  ranks_clustering_year <- ranks_clustering %>%
    filter(season == year) %>%
    mutate(goals = scale(goals),
           primary_assists = scale(primary_assists),
           secondary_assists = scale(secondary_assists),
           shot_attempts = scale(shot_attempts),
           shots_on_goal = scale(shots_on_goal),
           blocked_shot_attempts = scale(blocked_shot_attempts),
           missed_shots = scale(missed_shots),
           avg_ice_time = scale(avg_ice_time),
           total_xg = scale(total_xg),
           avg_xg = scale(avg_xg),
           total_xrebounds = scale(total_xrebounds),
           avg_xrebounds = scale(avg_xrebounds),
           total_rebound_value = scale(total_rebound_value),
           avg_rebound_value = scale(avg_rebound_value)
    ) %>%
    select(-season)
  
  rownames(ranks_clustering_year) <- ranks_clustering_year$shooter_id
  ranks_clustering_year <- ranks_clustering_year %>% select(-shooter_id)
  
  # Hierarchical clustering (Ward's linkage) for each season, visualize dendrogram
  # clust <- agnes(ranks_clustering_year, method = "ward")
  # pltree(clust, cex = 0.2, hang = -1, main = "Dendrogram")

  # Number of clusters
  # gap_stat <- clusGap(ranks_clustering_year, FUN = hcut, K.max = 10, B = 100)
  # fviz_gap_stat(gap_stat)
  
  clusters <- 10
  
  # Compute distance matrix, perform clustering, cut into clusters
  dists <- dist(ranks_clustering_year, method = "euclidean")
  final_clust <- hclust(dists, method = "ward.D2")
  ranks_clustering_year$cluster <- cutree(final_clust, k = clusters)
  
  # Extract coordinates after multidimensional scaling
  fit <- cmdscale(dists, eig = TRUE, k = 2)
  x <- fit$points[, 1]
  y <- fit$points[, 2]
  
  # Plot
  ranks_clustering_year$shooter_id <- as.integer(rownames(ranks_clustering_year))
  ranks_clustering_year <- left_join(ranks_clustering_year, ranks, by = "shooter_id") %>%
    filter(season == year)
  
  plot(x, y,
       xlab = "MDS dimension 1", ylab="MDS dimension 2",
       main = paste0("Player archetype clustering with shot/rebound quality, ",
                     as.character(year)),
       type = "n")
  
  text(x, y,
       labels=ranks_clustering_year$shooter,
       cex=0.3,
       col=rainbow(clusters)[ranks_clustering_year$cluster])
  
  # Clustering summary statistics
  # table(ranks_clustering_year$cluster)
  # table(ranks_clustering_year$position, ranks_clustering_year$cluster)
  
  # Combine results
  if (is.null(results)) {
    results <- ranks_clustering_year
  } else {
    results <- rbind(results, ranks_clustering_year)
  }
}

# Box
ranks_clustering <- ranks_clustering %>%
  select(-total_xg, -avg_xg,
         -total_xrebounds, -avg_xrebounds,
         -total_rebound_value, -avg_rebound_value)

for (year in years) {
  ranks_clustering_year <- ranks_clustering %>%
    filter(season == year) %>%
    mutate(goals = scale(goals),
           primary_assists = scale(primary_assists),
           secondary_assists = scale(secondary_assists),
           shot_attempts = scale(shot_attempts),
           shots_on_goal = scale(shots_on_goal),
           blocked_shot_attempts = scale(blocked_shot_attempts),
           missed_shots = scale(missed_shots),
           avg_ice_time = scale(avg_ice_time)
    ) %>%
    select(-season)
  
  rownames(ranks_clustering_year) <- ranks_clustering_year$shooter_id
  ranks_clustering_year <- ranks_clustering_year %>% select(-shooter_id)
  
  # Hierarchical clustering (Ward's linkage) for each season, visualize dendrogram
  # clust <- agnes(ranks_clustering_year, method = "ward")
  # pltree(clust, cex = 0.2, hang = -1, main = "Dendrogram")

  # Number of clusters
  # gap_stat <- clusGap(ranks_clustering_year, FUN = hcut, K.max = 10, B = 100)
  # fviz_gap_stat(gap_stat)
  
  clusters <- 10
  
  # Compute distance matrix, perform clustering, cut into clusters
  dists <- dist(ranks_clustering_year, method = "euclidean")
  final_clust <- hclust(dists, method = "ward.D2")
  ranks_clustering_year$cluster <- cutree(final_clust, k = clusters)
  
  # Extract coordinates after multidimensional scaling
  fit <- cmdscale(dists, eig = TRUE, k = 2)
  x <- fit$points[, 1]
  y <- fit$points[, 2]
  
  # Plot
  ranks_clustering_year$shooter_id <- as.integer(rownames(ranks_clustering_year))
  ranks_clustering_year <- left_join(ranks_clustering_year, ranks, by = "shooter_id") %>%
    filter(season == year)
  
  plot(x, y,
       xlab = "MDS dimension 1", ylab="MDS dimension 2",
       main = paste0("Player archetype clustering with box statistics only, ",
                     as.character(year)),
       type = "n")
  
  text(x, y,
       labels=ranks_clustering_year$shooter,
       cex=0.3,
       col=rainbow(clusters)[ranks_clustering_year$cluster])
  
  # Clustering summary statistics
  # table(ranks_clustering_year$cluster)
  # table(ranks_clustering_year$position, ranks_clustering_year$cluster)
  
  # Combine results
  if (is.null(box_results)) {
    box_results <- ranks_clustering_year
  } else {
    box_results <- rbind(box_results, ranks_clustering_year)
  }
}

# Save results
write.csv(results, "data/clustering_results.csv", row.names = FALSE)
write.csv(box_results, "data/box_clustering_results.csv", row.names = FALSE)