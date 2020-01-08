## code to prepare `carnivora` dataset
#
library(readr)


carn_df <- read_csv('data-raw/Carnivora_incidence_marineterr.csv')

# Due to compatibility with Python, the first column contains site numbers (rownames)
#
colnames(carn_df)[1] <- 'site_id'

# remove disconnected species (columns) and sites (rows)
#
n_cols  <- ncol(carn_df)
carn_df <- carn_df[ , append(rep(TRUE, 3), colSums(abs(carn_df[ , 4:n_cols])) != 0)]
n_cols  <- ncol(carn_df)
carn_df <- carn_df[rowSums(abs(carn_df[ , 4:n_cols])) != 0, ]

#

# The second and third columns are the (x,y) coordinates of the sites.
# Together with site-id these columns are saved for displaying output
#
extra_vecs  <- carn_df[ , c('site_id', 'x', 'y')]

# The adjacency matrix of the bi-partite network
#
carn_matrix <- as.matrix(carn_df[ , 4:n_cols])

# Save the data as a Rdata file
carnivora   <- list(M = carn_matrix, E = extra_vecs)
usethis::use_data(carnivora, overwrite = TRUE)
