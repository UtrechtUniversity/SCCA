## code to prepare `exports` dataset
#
library(readr)
library(tidyverse)

# read exports data
#
  # country / products data
  #
exports_df <- read_csv(
  file      = 'data-raw/country_product_exports.csv',
  col_names = TRUE,
  col_types = cols(.default = col_double(), country_name = col_character())
)

  # GDP per country
  #
gdp_df <- read_csv(
  file = "data-raw/country_GDPpc.csv",
  col_names = TRUE
)
gdp_df %<>% rename(gdp = GDPpc) %>% select(country_name, gdp)

# check disconnected countries (columns) and products (rows)
#
n_col <- ncol(exports_df)
if (any(colSums(abs(exports_df[ , 2:n_col])) == 0)) {
  warning('disconnected columns')
}

if (any(rowSums(abs(exports_df[ , 2:n_col])) == 0)) {
  warning('disconnected rows')
}


exports_countries <- exports_df %>%
  mutate(label = sprintf('%03d', row_number())) %>%
  select(label, country_name) %>%
  left_join(y = gdp_df, by = c('country_name' = 'country_name'))


exports_products  <- tibble(product_name = names(exports_df)[-1]) %>%
  mutate(label = sprintf('%04d', row_number())) %>%
  select(label, product_name)



# Adjacency matrix
#
exports           <- as.matrix(exports_df[ , 2:n_col])
rownames(exports) <- exports_countries %>% dplyr::pull(label)
colnames(exports) <- exports_products %>% dplyr::pull(label)

# Save the data as a Rda file
#
usethis::use_data(exports, overwrite = TRUE)
usethis::use_data(exports_countries, overwrite = TRUE)
usethis::use_data(exports_products, overwrite = TRUE)
