# Spectral Clustering Correspondence Analysis

Several methods of data analysis, widely used in different fields, and most prominently in ecology and in economics,  have recently been recognised to be coincident (Mealy et al 2019). The method known in economics as the 'Economic Complexity Index' (ECI) (Hidalgo et al 2009, Baudena et al 2015) is in fact mathematically identical to a statistical method known as **Correspondence Analysis** (CA). Though originally derived as an ordination method in ecology (Hill, 1973), CA scores also arise in methods known in network science as spectral clustering and graph embedding. In this paper we illustrate how the use of CA can be extended and the understanding deepened thanks to the insights from network theory and we give guidelines for applications to different types of networks in ecology and economics. We focus in particular on applications to data that gives rise to modular networks.

This R-package is meant to allow application of correspondence analysis to data that gives rise to modular networks by first clustering the data using a iterative procedure based on spectral clustering and subsequently apply correspondence analysis to each of the individual clusters. The package contains funciton to compute the similarity network defined by a bipartite network using stochastic complementation, and compute the spectrum and eigenvalues of the Laplacian of this network. It also provides the option to perform an iterative procedure that clusters the data based on the properties of the spectrum of the, and to analyze the resulting clusters using correspondence analysis.

## Installation

This R package can be installed directly from Github with the code below. Ensure that the package `devtools` is installed (install with `install.packages("devtools")`).


``` R 
library(devtools)
install_github("UtrechtUniversity/scca", build_vignettes = TRUE)
```

## Usage

The main function of the package is `scca_compute`. The value is an SCCA clustering tree (recursive list).

``` R
scca <- scca_compute(m = carnivora)
```

To get the final clusters:

``` R
clusters <- scca_get_clusters(scca)
```

The value is a cluster table. For each observation a record with the label of the observation and the cluster the observation is assigned to.

SCCA is a stochastic process. Two runs will produce different outputs. The overlap between those outputs can be computed (and optionally plotted) with

``` R
scca1   <- scca_compute(m = carnivora)
overlap <- scca_overlap_test(x = scca, y = scca1, plot = TRUE)
```

### Printing and plotting

The package provides three functions for printing and plotting the hierachical analysis tree produced by \code{scca_compute}

The function \code{scca_print} prints the ...

``` R
s <- scca_compute(carnivora, decomp = 'svd')
scca_print(scca = s, 'k', 'n_labs')
```

The function \code{scca_plot_spectrum} plots the spectrum of a specific node ...

The function \code{scca_plot_vectors} plots the ....

### Stability

Let `cl` be a clustering on a set of observation and `cl_i` the clustering of same observations with variable `i` dropped. The clustering process is called stable if `cl` is (almost) the same (by some measure) as cl_i. The measure is the average proportion of overlap.

``` R
library(dplyr)
drop          <- sample(ncol(carnivora), ncol(carnivora) %/% 10)
stability     <- scca_stability_test(m = carnivora, drop_vars = drop)
avg_stability <- stability %>% summarise(mean(var_APO))
``` 
### Validity

The package `clValid` and `cluster` provide functions to calculate the internal validity of a clustering. The measures are Silhoutette Width, Dunn Index and Connectivity. This package provides a wrapper around these functions. It needs a distance matrix which must be calculated. Save the distance matrix for repeated uses, because the distance computation may take a while. 

``` R
library(dplyr)
library(readr)
d_species    <- scca_compute_dist(m = t(carnivora), filename = 'd_species')
# d_species  <- read_rds('some_path/d_species_carn') # path from working directory
scca_species <- scca_compute(m = t(carnivora))
validity     <- scca_validity_test(scca = scca_species, dist = d_species)
validity$sil
validity$dunn
validity$conn
```

Last but not least, there is a function to compare clusterings form `scca_compute` with clusterings on the same data/category from the Python implementation. See ?scca_py_overlap_test

``` R
s <- scca_compute(m = carnivora)
scca_py_overlap_test(scca = s, 
     py_output = 'location of the Python output files',
     plot      = TRUE)
```

## License and citation

## Contact

Contributors, contact info, reference to ITS and description `[[ By Alje and ITS ]]`


