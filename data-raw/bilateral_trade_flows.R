## code to prepare `carnivora` dataset
#
library(readr)
library(tidyverse)


exports_df <- read_csv('data-raw/exports_matrix.csv',
                       col_names = TRUE,
                       col_types = cols(.default =                col_double(),
                                         location_name_short_en = col_character()))


# remove disconnected countries (columns) and products (rows)
#

n_cols     <- ncol(exports_df)
exports_df <- exports_df[ , append(rep(TRUE, 1), colSums(abs(exports_df[ , 2:n_cols])) != 0)]
n_cols     <- ncol(exports_df)
exports_df <- exports_df[rowSums(abs(exports_df[ , 2:n_cols])) != 0, ]

#

# The second and third columns are the (lon,lat) coordinates of the sites.
# These columns are saved for displaying output on maps etc.
#
countries           <- exports_df[ , 1]
colnames(countries) <- 'country_name'

# The adjacency matrix of the bi-partite network
#
exports_matrix           <- as.matrix(exports_df[ , 2:n_cols])
rownames(exports_matrix) <- countries %>% dplyr::pull(country_name)

# Save the data as a Rdata file
#
bilateral_trade   <- list(M = exports_matrix, countries = countries)
usethis::use_data(bilateral_trade, overwrite = TRUE)
