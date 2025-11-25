# load libraries
install.packages(c("data.table", "dplyr"))
library(data.table)
library(dplyr)

# import datasets
imdb_names <- fread("~/datasets/imdb_dataset/names.csv")
imdb_titles <- fread("~/datasets/imdb_dataset/titles.csv")
tmdb <- fread("datasets/tmdb_dataset/tmdb_dataset.csv")

colnames(imdb_names)
colnames(imdb_titles)
colnames(tmdb)

