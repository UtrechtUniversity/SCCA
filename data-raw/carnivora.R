## code to prepare `carnivora` dataset
#
library(readr)
library(tidyverse)
library(magrittr)

carn_df   <- read_csv('data-raw/Carnivora.csv')

# Legacy of Python; first column contains row indices
#
carn_df %<>% select(-1)

# convert to matrix,
#
carnivora           <- as.matrix(carn_df)
rownames(carnivora) <- sprintf('%d', 0:(nrow(carnivora)-1))
colnames(carnivora) <- sprintf('%d', 0:(ncol(carnivora)-1))

# remove disconnected species (columns) and sites (rows)
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
usethis::use_data(carnivora, overwrite = TRUE)

# extra data Carnivora dataset
#
carnivora_sites   <- read_csv('data-raw/Info_Site.csv')
carnivora_sites %<>% select(site = X1,
                            lon  = Xcoord,
                            lat  = Ycoord) %>%
                     mutate(site = sprintf("%d", site)) # %>% filter(!row_number() %in% rows0)

usethis::use_data(carnivora_sites, overwrite = TRUE)

# extra data Carnivora dataset
#
carnivora_species   <- read_csv('data-raw/Info_Spec.csv')
carnivora_species %<>% select(species_id    = X1,
                              species_name  = `0`) %>%
                       mutate(species_id = sprintf("%d", species_id)) # %>% filter(!row_number() %in% cols0)

usethis::use_data(carnivora_sites, overwrite = TRUE)
usethis::use_data(carnivora_species, overwrite = TRUE)
