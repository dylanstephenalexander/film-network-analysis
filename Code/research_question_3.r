# load libraries
install.packages(c("data.table", "dplyr", "tidyr"))
library(data.table)
library(tidyr)
library(dplyr)

# import datasets
imdb_names <- fread('/Users/dylanalexander/Desktop/term 1 2025-26/cosc 421/datasets/imdb_dataset/names.csv')
imdb_titles <- fread('/Users/dylanalexander/Desktop/term 1 2025-26/cosc 421/datasets/imdb_dataset/titles.csv')
tmdb <- fread('/Users/dylanalexander/Desktop/term 1 2025-26/cosc 421/datasets/tmdb_dataset/tmdb_dataset.csv')

colnames(imdb_names)
colnames(imdb_titles)
colnames(tmdb)




### CLEAN UP DATASET ###




# extrapolating release_year field from release_date

tmdb <- tmdb %>%
  mutate(release_year = as.numeric(substr(release_date, 1, 4)))

# minimum budget set at $1M, minimum release_year set at 2000, qualifying movies must have budget, revenue, release_year data

tmdb_cleaned <- tmdb %>%
  filter(
    budget >= 1000000,
    release_year >= 2000,
    !is.na(budget),
    !is.na(revenue),
    !is.na(release_year)
  )

# calculating ROI field

tmdb_cleaned <- tmdb_cleaned %>%
  mutate(roi_percent = ((revenue - budget) / budget) * 100)




### DEFINE FLOP, SUCCESS ### 




num_moves <- nrow(tmdb_cleaned) # num movies = 11832 
min_roi_percent <- min(tmdb_cleaned$roi_percent) # max_roi_percent = 49900%
max_roi_percent <- max(tmdb_cleaned$roi_percent) # max_roi_percent = -100%
mean_roi_percent <- mean(tmdb_cleaned$roi_percent) # mean_roi_percent = 68.4601%

# 'flop' defined as negative ROI
num_flops <- sum(tmdb_cleaned$roi_percent < 0) # num_flops = 7591
# 'success' defined as positive ROI
num_successes <- sum(tmdb_cleaned$roi_percent > 0) # num_successes = 4223




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

# 3. Create a two mode {movie - actor} network 
tmdb_actor_movie <- tmdb_stars_only %>%
  select(imdb_id, title, actor_name, budget, revenue, roi_percent, release_year)

# 4. Filter out garbage like "Jr." and other inevitable junk (TO DO: take another look at this later)

tmdb_actor_movie <- tmdb_actor_movie %>%
  filter(!actor_name %in% c("Jr."))

# tmdb_actor_movie consists of {imdb_id, title, actor_name, budget, revenue, roi_percent, release_year}
