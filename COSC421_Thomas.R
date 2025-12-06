library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(disk.frame)

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(data.table)
library(igraph)


# Movie loading
setwd("C:\\Users\\jccra")
titles_header <- fread("titles.csv", nrows = 0)
cat("Column positions:\n")
print(names(titles_header))

titles <- fread("titles.csv", 
                select = c("tconst", "primaryTitle", "startYear"))

TMmovies <- fread("TMDB_all_movies.csv",
                  select = c("imdb_id", "cast", "revenue", "release_date"))

# Filter years, and non-tt values, movies without a cast, and set the date range to be between "2018-01-01" and 2022-12-31". 

tmdb_cleaned <- TMmovies %>%
  filter(
    release_date >= as.Date("2018-01-01") &
      release_date <= as.Date("2022-12-31"),
    !is.na(cast),
    cast != "",
    grepl("^tt", imdb_id)
  )

titles_cleaned <- titles %>%
  filter(
    startYear >= 2018 & startYear <=2022 &!is.na(startYear) & startYear != "",
    grepl("^tt", tconst)
  ) %>%
  rename(imdb_id = tconst)






movies<-inner_join(titles_cleaned, tmdb_cleaned, by = "imdb_id")
setwd("C:\\Users\\jccra")

#read in names
imdb_names <-fread("names.csv",
                   select = c("nconst", "primaryName", "birthYear", "knownForTitles"))

#clean in names
names_cleaned <- imdb_names %>% 
  filter(birthYear >= 1940 & !is.na(birthYear)) %>%
  # Remove exact duplicates (all 3 columns same)
  distinct(primaryName, birthYear, knownForTitles,
           .keep_all = TRUE)

names_cleaned <- names_cleaned %>%
  group_by(primaryName) %>%
  mutate(
    isDuplicate = n() > 1,
    .groups = "drop"
  )

v_names <- names_cleaned$primaryName
#valid name lookup
library(fastmatch)
movies[, filtered_cast := lapply(strsplit(cast, ",\\s*"), function(x) {
  x[x %fin% v_names]  # %fin% from fastmatch
})]

# Filter to valid names
splt_cast <- movies$filtered_cast





# 3. Create actor-to-movies mapping from splt_cast (FAST)
# Convert splt_cast to data.table with movie IDs


cast_dt <- data.table(
  movie_id = rep(movies$imdb_id, lengths(splt_cast)),
  actor = unlist(splt_cast)
)




#duplicates matching
movie_ids <- unique(movies$imdb_id)
setDT(names_cleaned)
# DUPLICATES: Use knownForTitles
duplicates <- names_cleaned[isDuplicate == TRUE]

dup_movies <- duplicates[
  , .(known_titles = unlist(strsplit(knownForTitles, ","))),
  by = .(nconst, primaryName)
][known_titles %in% movie_ids][
  , .(movieList = list(unique(known_titles))),
  by = .(nconst, primaryName)
]

# NON-DUPLICATES: Use cast lists
non_duplicates <- names_cleaned[isDuplicate == FALSE]

# Build fast lookup: actor -> all their movies from cast
cast_dt <- data.table(
  movie_id = rep(movies$imdb_id, lengths(splt_cast)),
  actor = unlist(splt_cast)
)[actor %in% non_duplicates$primaryName]

non_dup_movies <- cast_dt[
  , .(movieList = list(unique(movie_id))),
  by = actor
]

#combine both into names_cleaned -> names_final
names_final <- rbindlist(list(
  dup_movies[, .(nconst, primaryName, movieList, isDuplicate = TRUE)],
  non_dup_movies[, .(nconst = NA_character_, primaryName = actor, movieList, isDuplicate = FALSE)]
), fill = TRUE)

#add actor revenue




actor_movie_expanded <- names_final[
  , .(movie_id = unlist(movieList)),
  by = .(nconst, primaryName)
]
actor_movie_expanded[, revenue := movies$revenue[match(movie_id, movies$imdb_id)]]

# Get max per actor
actor_max_rev <- actor_movie_expanded[
  , .(actRevenue = max(revenue, na.rm = TRUE)),
  by = .(nconst, primaryName)
]

names_final <- merge(names_final, actor_max_rev, by = c("nconst", "primaryName"), all.x = TRUE)
names_final[is.infinite(actRevenue), actRevenue := 0]



# Merge back
names_final <- merge(names_final, actor_rev, by = c("nconst", "primaryName"))

names_final[is.infinite(actRevenue), actRevenue := 0]
names_final <- merge(
  names_final,
  actor_max_revenue[, .(nconst, actRevenue)],
  by = "nconst",
  all.x = TRUE
)


#filter out movies without actors
movies_with_actors <- movies[lengths(filtered_cast) > 0]







# Create actor-movie pairs 
actor_movie_pairs <- movies_with_actors[
  , .(actor = unlist(filtered_cast)),
  by = .(imdb_id)
]
library(Matrix)
# Convert to factors for integer indices
actors <- unique(actor_movie_pairs$actor)
movies_list <- unique(actor_movie_pairs$imdb_id)

actor_idx <- match(actor_movie_pairs$actor, actors)
movie_idx <- match(actor_movie_pairs$imdb_id, movies_list)

# Sparse incidence: actors × movies
inc_mat <- sparseMatrix(
  i = actor_idx,
  j = movie_idx,
  x = 1,
  dims = c(length(actors), length(movies_list))
)

# PROJECTION: actor × actor = inc_mat %*% t(inc_mat) (FAST matrix mult)
adj_mat <- inc_mat %*% t(inc_mat)

diag(adj_mat) <- 0

# Convert to igraph
library(igraph)
actor_graph <- graph_from_adjacency_matrix(adj_mat, mode = "undirected" )
V(actor_graph)$name <- actors
#remove duplicate edges and self-loops
actor_graph <- simplify(actor_graph)

#Get Eigencentrality
actEigencen<- eigen_centrality(actor_graph)$vector
vertex_Names<-V(actor_graph)$name
eigen_lookup <- setNames(actEigencen, vertex_names)

# Map correctly
names_final[, actEigencen := eigen_lookup[primaryName]]
#get quartiles
q_breaks <- quantile(names_final$actEigencen, 
                     probs = c(0, 0.25, 0.5, 0.75, 1), 
                     na.rm = TRUE)
#assign quartiles based on eigencentrality
names_final[, Quartile := fcase(
  actEigencen <= q_breaks[2] & !is.na(actEigencen), "Q1",
  actEigencen <= q_breaks[3] & !is.na(actEigencen), "Q2",
  actEigencen <= q_breaks[4] & !is.na(actEigencen), "Q3",
  actEigencen <= q_breaks[5] & !is.na(actEigencen), "Q4",
  default = NA_character_
)]
#get louvian clusters

louvian_C<-cluster_louvain(actor_graph, resolution =0.1)


cluster_sizes <- table(membership(louvian_C))
top_4_clusters <- as.numeric(names(sort(cluster_sizes, decreasing = TRUE)[1:4]))
names_final[, cluster_letter := fcase(
  membership(louvian_C)[primaryName] == top_4_clusters[1], "A",
  membership(louvian_C)[primaryName] == top_4_clusters[2], "B",
  membership(louvian_C)[primaryName] == top_4_clusters[3], "C",
  membership(louvian_C)[primaryName] == top_4_clusters[4], "D",
  default = "E"  # Everyone else
)]

## for simplicity, the quartiles and clusters separated. 
Q1 <- names_final[Quartile == "Q1"]
Q2 <- names_final[Quartile == "Q2"] 
Q3 <- names_final[Quartile == "Q3"]
Q4 <- names_final[Quartile == "Q4"]

cluster_ids <- as.numeric(names(sort(table(membership(louvian_C)), decreasing = TRUE)[1:4]))
C1 <- names_final[membership(louvian_C)[primaryName] == cluster_ids[1]]
C2 <- names_final[membership(louvian_C)[primaryName] == cluster_ids[2]]
C3 <- names_final[membership(louvian_C)[primaryName] == cluster_ids[3]]
C4 <- names_final[membership(louvian_C)[primaryName] == cluster_ids[4]]
C5 <- names_final[!membership(louvian_C)[primaryName] %in% cluster_ids]
##Analysis question 1: P(X=0) analysis
p_r0= sum(names_final$actRevenue==0) / nrow(names_final) 


library(PropCIs)

p_null <- sum(names_final$actRevenue == 0, na.rm = TRUE) / nrow(names_final[!is.na(actRevenue)])

results <- rbindlist(lapply(list("Q1"=Q1, "Q2"=Q2, "Q3"=Q3, "Q4"=Q4), function(df) {
  test <- prop.test(sum(df$actRevenue == 0, na.rm = TRUE), 
                    nrow(df[!is.na(actRevenue)]), 
                    p = p_null)
  data.table(
    Quartile = df$Quartile[1],
    Probability = sum(df$actRevenue == 0, na.rm = TRUE) / nrow(df[!is.na(actRevenue)]),
    Z_Score = sqrt(test$statistic),
    P_Value = test$p.value
  )
}))

colnames(results) <- c("Eigenvector_Quartile", 
                       "Proportion_Zero_Revenue", 
                       "Z_Statistic", 
                       "P_Value_vs_Overall")



print(results)
###Louvian P=0


p_null <- sum(names_final$actRevenue == 0, na.rm = TRUE) / nrow(names_final[!is.na(actRevenue)])

results <- rbindlist(lapply(list("C1"=C1, "C2"=C2, "C3"=C3, "C4"=C4, "C5"=C5), function(df) {
  test <- prop.test(sum(df$actRevenue == 0, na.rm = TRUE), 
                    nrow(df[!is.na(actRevenue)]), 
                    p = p_null)
  data.table(
    Cluster = ifelse(nrow(df) > 0, 
                     # Get cluster from first row OR use list name
                     ifelse("cluster_group" %in% names(df), 
                            df$cluster_group[1], 
                            gsub("C", "Cluster ", names(df)[1])),
                     NA_character_),
    Probability = sum(df$actRevenue == 0, na.rm = TRUE) / nrow(df[!is.na(actRevenue)]),
    Z_Score = sqrt(test$statistic),
    P_Value = test$p.value
  )
}))

colnames(results) <- c("Louvian_Community", 
                       "Proportion_Zero_Revenue", 
                       "Z_Statistic", 
                       "P_Value_vs_Overall")

print(results)
###P>0 analysis
Q1_pos <- Q1[actRevenue > 0]
Q2_pos <- Q2[actRevenue > 0]
Q3_pos <- Q3[actRevenue > 0]
Q4_pos <- Q4[actRevenue > 0]

# For clusters C1-C5
C1_pos <- C1[actRevenue > 0]
C2_pos <- C2[actRevenue > 0]
C3_pos <- C3[actRevenue > 0]
C4_pos <- C4[actRevenue > 0]
C5_pos <- C5[actRevenue > 0]

tot_pos<- names_final[actRevenue>0]$actRevenue
library(ks)
tot_kde<-kde(log10(tot_pos))

q1_kde<-kde(log10(Q1_pos$actRevenue))
q2_kde<-kde(log10(Q2_pos$actRevenue))
q3_kde<-kde(log10(Q3_pos$actRevenue))
q4_kde<-kde(log10(Q4_pos$actRevenue))

plot(tot_kde$eval.points, tot_kde$estimate, type = "l", col = "black", lwd = 3,
     main = "Revenue Distributions (Log10 Scale) by Eigenvector Centrality",
     ylim = c(0, max(tot_kde$estimate, q1_kde$estimate, q2_kde$estimate, 
                     q3_kde$estimate, q4_kde$estimate) * 1.1),
     xlab = "Log10(Revenue)",
     ylab = "Density")

# Overlay quartiles
lines(q1_kde$eval.points, q1_kde$estimate, col = "red", lwd = 2, lty = 2)
lines(q2_kde$eval.points, q2_kde$estimate, col = "blue", lwd = 2, lty = 2)
lines(q3_kde$eval.points, q3_kde$estimate, col = "green", lwd = 2, lty = 2)
lines(q4_kde$eval.points, q4_kde$estimate, col = "purple", lwd = 2, lty = 2)

legend("topleft", 
       legend = c("Overall", "Q1", "Q2", "Q3", "Q4"),
       col = c("black", "red", "blue", "green", "purple"),
       lwd = c(2, 1.5, 1.5, 1.5, 1.5),
       lty = c(1, 2, 2, 2, 2),
       cex = 1)  
c1_kde <- kde(log10(C1_pos$actRevenue))
c2_kde <- kde(log10(C2_pos$actRevenue))
c3_kde <- kde(log10(C3_pos$actRevenue))
c4_kde <- kde(log10(C4_pos$actRevenue))
c5_kde <- kde(log10(C5_pos$actRevenue))

# Find max y
max_y_cluster <- max(tot_kde$estimate, c1_kde$estimate, c2_kde$estimate, 
                     c3_kde$estimate, c4_kde$estimate, c5_kde$estimate)

# Plot
plot(tot_kde$eval.points, tot_kde$estimate, type = "l", col = "black", lwd = 3,
     main = "Revenue Distributions by Louvain Community\n(Log10 Scale)",
     ylim = c(0, max_y_cluster * 1.1),
     xlab = "Log10(Revenue)",
     ylab = "Density")

# Overlay clusters
lines(c1_kde$eval.points, c1_kde$estimate, col = "red", lwd = 2, lty = 2)
lines(c2_kde$eval.points, c2_kde$estimate, col = "blue", lwd = 2, lty = 2)
lines(c3_kde$eval.points, c3_kde$estimate, col = "green", lwd = 2, lty = 2)
lines(c4_kde$eval.points, c4_kde$estimate, col = "purple", lwd = 2, lty = 2)
lines(c5_kde$eval.points, c5_kde$estimate, col = "orange", lwd = 2, lty = 2)

# Legend
legend("topleft", 
       legend = c("Overall", "C1", "C2", "C3", "C4", "C5"),
       col = c("black", "red", "blue", "green", "purple", "orange"),
       lwd = c(3, 2, 2, 2, 2, 2),
       lty = c(1, 2, 2, 2, 2, 2),
       cex = 1)



#assign quartiles
q_breaks <- quantile(names_final$actEigen, 
                     probs = c(0, 0.25, 0.5, 0.75, 1), 
                     na.rm = TRUE)

names_final[, actEigencen := eige[primaryName]]


