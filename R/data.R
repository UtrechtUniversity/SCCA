#' Geographical distributions of the mammalian order Carnivora
#'
#' This common-used dataset in Ecology, gives the geographical distributions of the mammalian order Carnivora.
#' The data set is comprised by an incidence matrix M
#' (i.e. presence-absence matrix), with 41,580 non-empty sites (rows) and 288 extant terrestrial and marine species (columns).
#' The sites represent grid-cells rasterised at a resolution of 0.78 latitudinal degrees.
#' The distributional data results from rasterizing range map distributions published as part of
#' the Phylacine v1.2 dataset (Faurby et al. 2018), which includes
#' data on the geographic distributions of mammals both extant and extinct over the last 130,000 years.
#' The data was downloaded (last accessed on November 2019; \url{https://datadryad.org/stash/dataset/doi:10.5061/dryad.bp26v20}) and pruned
#' to only include carnivores.
#' Data was processed in R (R Core Development Team 2014) and mapped in QGIS v2.18.16 (QGIS Development Team 2015).
#'
#'
#' @format A matrix with 41,580 rows (sites) and and 288 columns (species)
#' \describe{
#'   \item{\strong{cell value}}{absence-presence (0/1) values}
#'   \item{\strong{row name}}{label which can be used to retrieve the site specs from the data frame \code{carnivora_sites}}
#'   \item{\strong{column name}}{label which can be used to retrieve the species name from the data frame \code{carnivora_species}}
#' }
#'
#' @source \url{https://datadryad.org/stash/dataset/doi:10.5061/dryad.bp26v20}
"carnivora"


#' Species of the Carnivora Incidence matrix
#'
#' This table has two columns: species_id and species_name. The first column contains labels (column names) of the Carnivora
#' incidence matrix. The second column contains the names of the species.
#'
#' @format A data frame with 288 rows (species)
#' \describe{
#'   \item{species_id}{Species labels used in matrix \code{carnivora} as column names}
#'   \item{species_name}{The actual names of the species}
#' }
#'
#' @source \url{https://datadryad.org/stash/dataset/doi:10.5061/dryad.bp26v20}
"carnivora_species"

#' Sites of the Carnivora Incidence matrix
#'
#' @format A data frame with 41,580 rows (sites) and 3 columns
#' \describe{
#'   \item{site}{Site labels used in matrix \code{carnivora} as row names}
#'   \item{lon}{Longitude of the site. See details.}
#'   \item{lat}{Lattitude of the site. See details.}
#' }
#'
#' @source \url{https://datadryad.org/stash/dataset/doi:10.5061/dryad.bp26v20}
#'
#' @details
#' The Behrman cylindrical equal area projection is used with
#' a resolution of 96.5 km by 96.5 km at 30° North and 30° South.
#' The full projection description following R-notation is
#' “+proj=cea +lon_0=0+lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84+towgs84=0,0,0”.
#' Only the area between 90°N and 60°S is included.
"carnivora_sites"


#' Exports
#'
#' An adjacency matrix of 234 countries (rows) exporting 1239 products (columns).
#' The each cell contains the value of the export in US dollars in 2016. See vignette X an CA of this data with SCCA.
#' See \strong{details} for description of the data.
#'
#' @format Matrix of 234 rows (countries) and 1239 columns (products)
#' \describe{
#'   \item{\strong{cell value}}{Value of the export in US dollars in 2016.}
#'   \item{\strong{row name}}{label of the exporting country which can be used to retrieve country data from \code{exports_countries}.}
#'   \item{\strong{column name}}{label of the exported product which can be used to retrieve product data from \code{exports_products}.}
#' }
#'
#' @details
#' The matrix is derived from trade data originally collected by the United Nations Statistical Division (COMTRADE) that was cleaned,
#' processed and made publicly available by the Growth Growth Lab at Harvard University (\url{http://atlas.cid.harvard.edu/about-data}).
#'
#' @source \url{http://atlas.cid.harvard.edu/about-data}
"exports"

#' Countries of the Exports data and their GDP per capita
#'
#' @format Data frame of 234 rows (countries) and 4 columns
#' \describe{
#'   \item{\strong{label}}{Label of the exporting country used in matrix \code{exports} as row name.}
#'   \item{\strong{country_name}}{Name of the country}
#'   \item{\strong{country_code}}{Code of the country}
#'   \item{\strong{GDPpc}}{Gross Domestic Product per capita }
#' }
#'
#'@details
#'Data on the GDP per capita was obtained from the World Bank (\url{https://data.worldbank.org/}), and is given in constant 2011 international $.
#'
#'
"exports_countries"

#' Products of the Exports data
#'
#' @format Data frame of 1239 rows (products) and 2 columns
#' \describe{
#'   \item{\strong{label}}{Label of the exported product used in matrix \code{exports} as column name.}
#'   \item{\strong{product_name}}{Name of the product}
#' }
#'
"exports_products"


