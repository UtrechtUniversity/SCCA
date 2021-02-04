[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)



# SCCA: Spectral Clustering Correspondence Analysis in R

## Introduction

The SCCA package implements in R the methodological approach to CA as proposed in **A network view of correspondence analysis: applications to ecology and economic complexity** (van Dam et al 2021). This package is publicly available for use with proper citation.

## Installation

The package can be installed directly from Github with the code below. Ensure that the package devtools is installed.

``` R 
#install.packages("devtools")
library(devtools)
install_github("UtrechtUniversity/scca", build_vignettes = TRUE)
```

## Exported functions and data sets

There are quite a few functions available to the user and also the two datasets which has been used in the formentioned article (Dam, Alje van, e.a. 2021). After loading the package a list of all exported functions  can be retrieved by `?SCCA` and the documentation of an individual function by `?<function name>`; e.g. `?scca_compute`.

The use of functions is explained in the included vignettes. Use `browseVignettes('SCCA')` to get the links to these vingettes.

## Exported data

The package contains two datasets: carnivora

### Carnivora

The data describes the global geographical distributions of the species of the mammalian order Carnivora. The carnivora data are described with a presence-absence incidedence matrix, with 288 extant terrestrial and marine species (rows) and 41,580 non-empty sites (columns). The sites represent grid-cells rasterized at a resolution of 0.78 latitudinal degrees.  The distributional data were extracted from the  mammal  range  map  database  Phylacine  v1.232,  which  we  downloaded from  [DRYAD](https://datadryad.org/stash/dataset/doi:10.5061/dryad.bp26v20) (last accessed in November 2019) and pruned to only include extant carnivorans. Data were processed in R (R Core Development Team 2014) and mapped in QGIS336v2.18.16 (QGIS Development Team 2015)

carnivora
carnivora_sites
carnivora_species

### Exports 

The exports dataset is a ‘presence-absence’ matrix with 234 countries (rows) and 1239 products (columns), in which a ‘presence’ indicates that a country was a significant exporter of a product in the year 2016. (see Appendix B387for an exact description of this procedure). This matrix is derived from data on international trade is from Harvard’s Growth Lab. We also use data on gross domestic product per capita (GDPpc) in 2016, as given in PPP constant 2017 international dollars, and taken from the [World Bank databank](https://databank.worldbank.org). (see Appendix B387for an exact description of this procedure).

See for more information the on-line documentation and vignette X.

## License

The software is licensed under **MIT**

## Links

## References

Dam, Alje van. e.a. 2021. "A network view of correspondence analysis: applications to ecology and economic complexity". *<name of journal>*. DOI: <doi>.

## The team

The package SCCA has been developed as part of the project "Title of project" by "Mara Baudena" etc. This project was funded by "X" and received support from the Research Engineering team of Utrecht University. The technical
implementation of the package was done by [Kees van Eijden](k.vaneijden@uu.nl) with contributions of Johan de Bruin and Raoul Schram. Ignacio Morales Castilla did the real life testing.

## How to cite SCCA

To cite the SCCA repository and R package, use

```R
citation("SCCA")
```

to retrieve the BibTex entry otherwise use the following format:

Dam, Alje van, Kees van Eijden. 2021. *SCCA: Spectral Clustering Correspondence Analysis in R*. Utrecht University. Available at https://github.com/UtrechtUniversity/SCCA. DOI: 12.1234/xxxxxx.1234567.

