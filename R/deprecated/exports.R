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


