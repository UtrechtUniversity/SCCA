## code to prepare `carnivora` dataset
#
library(readr)


carn_df <- read_csv('data-raw/Carnivora_incidence_marineterr.csv')

# Due to compatibility with Python, the first column contains site numbers (rownames)
#
colnames(carn_df)[1] <- 'site_id'
colnames(carn_df)[2] <- 'lon'
colnames(carn_df)[3] <- 'lat'

# remove disconnected species (columns) and sites (rows)
#
n_cols  <- ncol(carn_df)
carn_df <- carn_df[ , append(rep(TRUE, 3), colSums(abs(carn_df[ , 4:n_cols])) != 0)]
n_cols  <- ncol(carn_df)
carn_df <- carn_df[rowSums(abs(carn_df[ , 4:n_cols])) != 0, ]

#

# The second and third columns are the (lon,lat) coordinates of the sites.
# These columns are saved for displaying output on maps etc.
#
coords  <- carn_df[ , c('lon', 'lat')]

# The adjacency matrix of the bi-partite network
#
carn_matrix <- as.matrix(carn_df[ , 4:n_cols])

# Save the data as a Rdata file
#
carnivora   <- list(M = carn_matrix, coords = coords)
usethis::use_data(carnivora, overwrite = TRUE)
