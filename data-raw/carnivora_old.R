## code to prepare `carnivora_old` dataset
#
library(readr)
library(tidyverse)
library(magrittr)

carn_df   <- read_csv('data-raw/Carnivora_old.csv')

# Legacy of Python; first column contains row indices
#
carn_df %<>% select(-1)

# convert to matrix,
#
carnivora_old          <- as.matrix(carn_df)
rownames(carnivora_old) <- sprintf('%d', 0:(nrow(carnivora_old)-1))
colnames(carnivora_old) <- sprintf('%d', 0:(ncol(carnivora_old)-1))

carnivora_old[carnivora_old < 0] <- 0

# remove disconnected species (columns) and sites (rows)
# Under discussion!
#
# rows0        <- rowSums(carnivora) == 0
# names(rows0) <- NULL
# rows0        <- which(rows0)
# carnivora    <- carnivora[rowSums(carnivora) != 0, ]
#
# cols0        <- colSums(carnivora) == 0
# names(cols0) <- NULL
# cols0        <- which(cols0)
# carnivora    <- carnivora[ , colSums(carnivora) != 0]


# Save the data as a Rdata file
#
usethis::use_data(carnivora_old, overwrite = TRUE)
