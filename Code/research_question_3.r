# install packages
install.packages(c("data.table", "dplyr", "tidyr", "igraph"))

# import datasets
imdb_names <- fread('/Users/dylanalexander/Desktop/term 1 2025-26/cosc 421/datasets/imdb_dataset/names.csv')
imdb_titles <- fread('/Users/dylanalexander/Desktop/term 1 2025-26/cosc 421/datasets/imdb_dataset/titles.csv')
tmdb <- fread('/Users/dylanalexander/Desktop/term 1 2025-26/cosc 421/datasets/tmdb_dataset/tmdb_dataset.csv')

# load libraries
library(data.table)
library(tidyr)
library(dplyr)
library(igraph)

colnames(imdb_names)
colnames(imdb_titles)
colnames(tmdb)




### KEY

### IMPORTANT DATASETS
# 1. tmdb_cleaned: base dataset with ROI
# 2. movie_centrality: has the degree and eigenvector centrality scores from the movie-movie network
# 3. movie_analysis: combination of 1. and 2. for correlation analysis

### INTERMEDIATE-STEP DATASETS
# 1. tmdb_actor_movie: actor-movie network: {imdb_id, title, actor_name, budget, revenue, roi_percent, release_year}
# 2. movie_edges: fed into movie_network
# 3. movie_network: used to calculate degree and eigenvector centrality scores






### CLEAN UP DATASET ###




# extrapolating release_year field from release_date

tmdb <- tmdb %>%
  mutate(release_year = as.numeric(substr(release_date, 1, 4)))

# minimum budget set at $1M, minimum release_year set at 2000, qualifying movies must have budget, revenue, release_year data
# treat revenue = 0 and / or budget = 0 as missing revenue/budget data, omit these results entirely

tmdb_cleaned <- tmdb %>%
  filter(
    budget >= 1000000,
    release_year >= 2000,
    !is.na(budget),
    budget > 0,
    !is.na(revenue),
    revenue > 0,
    !is.na(release_year)
  )
  

# calculating ROI field: ROI = ((revenue - budget) / budget) * 100

tmdb_cleaned <- tmdb_cleaned %>%
  mutate(roi_percent = ((revenue - budget) / budget) * 100)

# remove junk from tmdb_cleaned

tmdb_cleaned <- tmdb_cleaned %>% select(-poster_path, -music_composer, -imdb_votes)
tmdb_cleaned <- tmdb_cleaned %>% select(-vote_count, -vote_average)
tmdb_cleaned <- tmdb_cleaned %>% select(-writers, -producers)
tmdb_cleaned <- tmdb_cleaned %>% select(-director, -director_of_photography, -overview)
tmdb_cleaned <- tmdb_cleaned %>% select(-tagline, -production_companies)




### DEFINE FLOP, SUCCESS ### 




num_moves <- nrow(tmdb_cleaned) # num movies = 11832 
min_roi_percent <- min(tmdb_cleaned$roi_percent) # max_roi_percent = 49900%
max_roi_percent <- max(tmdb_cleaned$roi_percent) # max_roi_percent = -100%
mean_roi_percent <- mean(tmdb_cleaned$roi_percent) # mean_roi_percent = 68.4601%

# conventional wisdom states movies often need a box office revenue of 2.5x or 3x of its budget in order to break-even, after marketing and other costs
# the below classifications for movie financial outcomes have been devised with this in mind

# if a movie has 2.5x box office revenue compared to budget, that results in an ROI of 150%

# conventional wisdom also states that for a movie to be classed a "success", that movie needs to hit 100% of all expenses in profit.
# if break-even is an ROI of 150%, then 100% of all expenses in profit comes out to 300% ROI.

# movies between -Inf and 0% ROI are classed outright as failures
# movies above 0% ROI but below 150% ROI (the threshold for break-even) are classed as flops
# movies above 150% ROI (the threshold for break-even) but below 300% ROI (the threshold for success) are classed as an in-betweener
# movies above 300% ROI (the threshold for success) but below 600% ROI are classed as successes
# anything above 600% (an estimated 200% net profit return) is classed as a smash-hit

tmdb_cleaned <- tmdb_cleaned %>%
  mutate(roi_category = cut(
    roi_percent,
    breaks = c(-Inf, 0, 150, 300, 600, Inf),
    labels = c("Failure", "Flop", "In-Betweener", "Success", "Smash-Hit")
  ))

head(tmdb_cleaned, 1)

num_failure <- sum(tmdb_cleaned$roi_category == "Failure") # num_failure = 7609
num_flop <- sum(tmdb_cleaned$roi_category == "Flop") # num_flop = 1904
num_in_betweener <- sum(tmdb_cleaned$roi_category == "In-Betweener") # num_in_betweener = 1019
num_success <- sum(tmdb_cleaned$roi_category == "Success") # num_success = 768
num_smash_hit <- sum(tmdb_cleaned$roi_category == "Smash-Hit") # num_smash_hit = 532




### DEFINE MAIN CAST (CALLED 'STARS') ###




# new approach solves problem I had that tmdb 'cast' column is not meaningfully sorted like I first assumed.

# 1. 'actor_frequency' aggregates movie_count by actor_name e.g. (Samuel L. Jackson, 81)

actor_frequency <- tmdb_cleaned %>%
  select(cast) %>%
  mutate(cast_list = strsplit(cast, ", ")) %>%
  tidyr::unnest(cast_list) %>%
  group_by(cast_list) %>%
  summarise(movie_count = n()) %>%
  rename(actor_name = cast_list)

# 2. using 'actor_frequency calculation', keep only the top 5 most prolific actors in a movie (now defined as 'stars')

tmdb_stars_only <- tmdb_cleaned %>%
  mutate(cast_list = strsplit(cast, ", ")) %>%
  tidyr::unnest(cast_list) %>%
  rename(actor_name = cast_list) %>%
  left_join(actor_frequency, by = "actor_name") %>%
  group_by(imdb_id) %>%
  arrange(desc(movie_count)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 5) %>%
  ungroup()



### CREATE ACTOR - MOVIE NETWORK




# 3. Create a two mode {movie - actor} network 
tmdb_actor_movie <- tmdb_stars_only %>%
  select(imdb_id, title, actor_name, budget, revenue, roi_percent, release_year)

# 4. Filter out garbage like "Jr." and other inevitable junk (TO DO: take another look at this later)

tmdb_actor_movie <- tmdb_actor_movie %>%
  filter(!actor_name %in% c("Jr."))

# tmdb_actor_movie consists of {imdb_id, title, actor_name, budget, revenue, roi_percent, release_year}




### CREATE MOVIE - MOVIE NETWORK




# create edges between movie pairs, one for each actor the 2 movies share: {movie1, movie2, actor_name}
movie_shared_actors <- tmdb_actor_movie %>%
  inner_join(tmdb_actor_movie %>% select(imdb_id, actor_name), by = "actor_name") %>%
  filter(imdb_id.x < imdb_id.y) %>%
  select(movie1 = imdb_id.x, movie2 = imdb_id.y, actor_name)

# Aggregate shared_actor count by movie pair
movie_edges <- movie_shared_actors %>%
  group_by(movie1, movie2) %>%
  summarise(shared_actors = n()) 

# Sort by shared_actor count (descending)
movie_edges %>%
  arrange(desc(shared_actors)) %>%
  print(n = 100)

# Create the movie - movie network
movie_network <- graph_from_data_frame(movie_edges, directed = FALSE)



### CALCULATE CENTRALITIES




# the number of other movies in the network that a movie shares 1 or more of its 'stars' with.
degree_centrality <- degree(movie_network, mode = "all")

# a measure of how well-connected the movies that it shares 1 or more of its 'stars' with are in the network.
eigen_centrality <- eigen_centrality(movie_network)$vector

# movie_centrality is a dataframe containing both centrality metrics
movie_centrality <- data.frame(
  imdb_id = names(degree_centrality),
  degree = degree_centrality,
  eigenvector = eigen_centrality)

# add 'title' to movie_centrality
movie_centrality <- movie_centrality %>%
  left_join(
    tmdb_actor_movie %>% select(imdb_id, title) %>% distinct(),
    by = "imdb_id"
  ) %>%
  select(imdb_id, title, degree, eigenvector)

## print by degree centrality descending

movie_centrality %>%
  arrange(desc(degree)) 

# print by eigenvector centrality descending

movie_centrality %>%
  arrange(desc(eigenvector)) 

### TOP 10 movies by Degree
# imdb_id                                        title degree eigenvector
# 1    tt0375568                                    Astro Boy    262   0.9687563
# 2    tt0462322                                   Grindhouse    260   0.7612072
# 3    tt0145487                                   Spider-Man    259   1.0000000
# 4    tt0413300                                 Spider-Man 3    259   1.0000000
# 5    tt8385148                    Hitman's Wife's Bodyguard    259   0.8888622
# 6    tt0316654                                 Spider-Man 2    249   0.9601488
# 7    tt1860353                                        Turbo    249   0.9915584
# 8    tt0257076                                     S.W.A.T.    245   0.8999215
# 9   tt26443597                                   Zootopia 2    244   0.7753177
# 10   tt4154756                       Avengers: Infinity War    237   0.9258646

### TOP 10 movies by Eigenvector
# imdb_id                                        title degree eigenvector
# 1    tt0413300                                 Spider-Man 3    259   1.0000000
# 2    tt0145487                                   Spider-Man    259   1.0000000
# 3    tt1860353                                        Turbo    249   0.9915584
# 4    tt0375568                                    Astro Boy    262   0.9687563
# 5    tt4154796                            Avengers: Endgame    236   0.9672482
# 6    tt0316654                                 Spider-Man 2    249   0.9601488
# 7    tt4154756                       Avengers: Infinity War    237   0.9258646
# 8    tt2395427                      Avengers: Age of Ultron    225   0.9141322
# 9    tt0257076                                     S.W.A.T.    245   0.8999215
# 10   tt8385148                    Hitman's Wife's Bodyguard    259   0.8888622








