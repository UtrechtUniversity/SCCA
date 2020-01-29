## code to prepare `carnivora` dataset
#
library(readr)
library(tidyverse)
library(magrittr)

carn_df <- read_csv('data-raw/Carnivora_incidence_marineterr.csv')

# Due to compatibility with Python, the first column contains site numbers (rownames)
#
colnames(carn_df)[1] <- 'label'
colnames(carn_df)[2] <- 'lon'
colnames(carn_df)[3] <- 'lat'

# remove disconnected species (columns) and sites (rows)
#
n_cols  <- ncol(carn_df)
carn_df <- carn_df[ , append(rep(TRUE, 3), colSums(abs(carn_df[ , 4:n_cols])) != 0)]
n_cols  <- ncol(carn_df)
carn_df <- carn_df[rowSums(abs(carn_df[ , 4:n_cols])) != 0, ]

#

# Compute rownames (labels)  out of coordinates
#
carn_df %<>% mutate(label = case_when(
              lon  < 0  & lat  < 0  ~ sprintf("W%4.1f-S%03.1f", -lon, -lat),
              lon  < 0  & lat >= 0  ~ sprintf("W%4.1f-N%03.1f", -lon,  lat),
              lon >= 0  & lat >= 0  ~ sprintf("E%4.1f-N%03.1f",  lon,  lat),
              lon >= 0  & lat  < 0  ~ sprintf("E%4.1f-S%03.1f",  lon, -lat),
              TRUE ~ "error"))

carn_df %>% group_by(label) %>% summarise(n=n()) %>% filter(n > 1)

# The second and third columns are the (lon,lat) coordinates of the sites.
# These columns are saved for displaying output on maps etc.
#
coords  <- carn_df[ , c('label', 'lon', 'lat')]

# The adjacency matrix of the bi-partite network
#
carn_matrix <- as.matrix(carn_df[ , 4:n_cols])
rownames(carn_matrix) <- carn_df %>% pull(label)    # set rownames (labels) of matrix

# Save the data as a Rdata file
#
carnivora   <- list(M = carn_matrix, coords = coords)
usethis::use_data(carnivora, overwrite = TRUE)
