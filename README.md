# Spectral Clustering Correspondence Analysis

Several methods of data analysis, widely used in different fields, and most prominently in ecology and in economics,  have recently been recognised to be coincident (Mealy et al 2019). The method known in economics as the 'Economic Complexity Index' (ECI) (Hidalgo et al 2009, Baudena et al 2015) is in fact mathematically identical to a statistical method known as **Correspondence Analysis** (CA). Though originally derived as an ordination method in ecology (Hill, 1973), CA scores also arise in methods known in network science as spectral clustering and graph embedding. In this paper we illustrate how the use of CA can be extended and the understanding deepened thanks to the insights from network theory and we give guidelines for applications to different types of networks in ecology and economics. We focus in particular on applications to data that gives rise to modular networks.

This R-package is meant to allow application of correspondence analysis to data that gives rise to modular networks by first clustering the data using a iterative procedure based on spectral clustering and subsequently apply correspondence analysis to each of the individual clusters. The package contains funciton to compute the similarity network defined by a bipartite network using stochastic complementation, and compute the spectrum and eigenvalues of the Laplacian of this network. It also provides the option to perform an iterative procedure that clusters the data based on the properties of the spectrum of the, and to analyze the resulting clusters using correspondence analysis.

## Installation

This R package can be installed directly from Github with the code below. Ensure that the package `devtools` is installed (install with `install.packages("devtools")`).


``` R 
library(devtools)
install_github("UtrechtUniversity/SCCA")
```

## Usage

Usage description `[[ By Kees ]]`

## License and citation

## Contact

Contributors, contact info, reference to ITS and description `[[ By Alje and ITS ]]`


