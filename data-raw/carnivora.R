## code to prepare `carnivora` dataset
#
library(readr)
library(tidyverse)
library(magrittr)

carn_df   <- read_csv('data-raw/Carnivora.csv')

# From Python; first column contains row indices
#
carn_df %<>% select(-1)

# convert to matrix,
#
carnivora           <- as.matrix(carn_df)
rownames(carnivora) <- sprintf('%d', 0:(nrow(carnivora)-1))
colnames(carnivora) <- sprintf('%d', 0:(ncol(carnivora)-1))

# remove disconnected species (columns) and sites (rows)
#
carnivora <- carnivora[rowSums(carnivora) != 0, ]
carnivora <- carnivora[ , colSums(carnivora) != 0]





# Compute rownames (labels)  out of coordinates
#
# carn_df %<>% mutate(label = case_when(
#               lon  < 0  & lat  < 0  ~ sprintf("W%4.1f-S%03.1f", -lon, -lat),
#               lon  < 0  & lat >= 0  ~ sprintf("W%4.1f-N%03.1f", -lon,  lat),
#               lon >= 0  & lat >= 0  ~ sprintf("E%4.1f-N%03.1f",  lon,  lat),
#               lon >= 0  & lat  < 0  ~ sprintf("E%4.1f-S%03.1f",  lon, -lat),
#               TRUE ~ "error"))
#




# Save the data as a Rdata file
#
usethis::use_data(carnivora, overwrite = TRUE)
