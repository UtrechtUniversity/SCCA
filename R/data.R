#' Carnivora Incidence in Marine Territoria
#'
#' To explore an example of application of CA to a common-use dataset in Ecology, we used the global
#' geographical distributions of the mammalian order Carnivora. The dataset is comprised by an incidence matrix M
#' (i.e. presence-absence matrix), with 41,580 non-empty sites (rows) and 288 extant terrestrial and marine species (columns).
#' The sites represent grid-cells rasterized at a resolution of 0.78 latitudinal degrees. The distributional data results
#' from rasterizing range map distributions published as part of the Phylacine v1.2 dataset (Faurby et al. 2018), which includes
#' data on the geographic distributions of mammals both extant and extinct over the last 130,000 years. We downloaded the data
#' (last accessed on November 2019; \url{https://datadryad.org/stash/dataset/doi:10.5061/dryad.bp26v20}) and prunned distributions
#' to only include carnivores. Data was processed in R (R Core Development Team 2014) and mapped in QGIS v2.18.16 (QGIS Development Team 2015).
#' The incidences of certain species (carnivora) at marine sites are given by a bipartite adjacency matrix.
#'
#'
#' @format A list with to elements
#' \describe{
#'   \item{M}{absence-presence (0/1) matrix. The columns are species and the rows are the sites }
#' }
#'
#' @details
#'   The table \code{carnivora_species}
#' @source \url{http://www.somewhere}
"carnivora"
"carnivora_species"

#' Species of the Carnivora incidence in marine territoria.
#'
#' This table has two columns: species_id and species_name. The first column contains labels (column names) of the Carnivora
#' incidence matrix. The second column contains the names of the species.
#'
"carnivora_species"

#' Site coordinates of the Ccarnivora dataset
#'
#' The Carnivora dataset also has a table with for every site the row id in the incidence matrix. The second and third columns give
#' the coordinates (lon/lat) of the site. The Behrman cylindrical equal area projection is used with
#' a resolution of 96.5 km by 96.5 km at 30° North and 30° South.
#' The full projection description following R-notation is
#' “+proj=cea +lon_0=0+lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84+towgs84=0,0,0”.
#' Only the area between 90°N and 60°S is included.
"carnivora_sites"


#' Exports data
#'
#' The exports dataset is an adjacency matrix of 234 countries (rows) exporting 1239 products (columns).
#' The matrix value is the value of the export in US dollars in 2016.
#' The names of the countries and their GDP can be found in \code{exports_countries}. The row names of \code{exports} coorespond to the column
#' \code{label}.Product names can be found in \code{exports_products}.
#'
#' The matrix is derived from trade data originally collected by the United Nations Statistical Division (COMTRADE) that was cleaned,
#' processed and made publicly available by the Growth Growth Lab at Harvard University.
#' For detailed we refer to http://atlas.cid.harvard.edu/about-data.
#'
#' The Growth Lab at Harvard University, 2019, "country_partner_hsproduct4digit_year_2016.tab",
#' International Trade Data (HS, 92), https://doi.org/10.7910/DVN/T4CHWJ/T3KWV0, Harvard Dataverse
#'
#'
#'
#' @format
#' \describe{
#'   \item{\emph{rows}}{Exporting countries}
#'   \item{\emph{columns}}{Exported product categories}
#' }
#' @source \url{http://atlas.cid.harvard.edu/about-data}
"exports"

#' Country names of exporting countries and their GDPs
#'
#' See: \link[=exports]{exports}
#'
"exports_countries"

#' Exported products
#'
#' See: \link[=exports]{exports}
#'
"exports_products"


