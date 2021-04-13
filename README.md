
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4665670.svg)](https://doi.org/10.5281/zenodo.4665670)

## **SCCA: Spectral Clustering Correspondence Analysis in R**

### Introduction

The SCCA package implements in R the methodological approach to CA as proposed in **Correspondence analysis, spectral clustering and graph embedding: applications to ecology and economic complexity** [van Dam et al; 2021](#article).

### Installation

The package can be installed directly from Github with the code below. Ensure the package `devtools` has been installed.

```
#install.packages("devtools")
library(devtools)
install_github("UtrechtUniversity/scca", build_vignettes = TRUE)
```

### Documentation of exported functions and data set

After loading the package a list of all exported functions and data sets can be retrieved by `?SCCA` and the documentation of an individual function by `?<function name>`; e.g. `?scca_compute`.

The methodology and the use of the functions and the data are explained in the included vignette. After installing package SCCA use `browseVignettes('SCCA')` in the R(Studio) console.

### License

The software code is licensed under [**MIT**](https://opensource.org/licenses/MIT). The next section (References) provides links to 
sources of the included datasets. See there for licences of those data sets.

### References

#### Software
<a name="article"></a>van Dam, Alje, Dekker, Mark,  Morales-Castilla, Ignacio, Rodríguez, Miguel Á., Wichmann, David and Baudena, Mara (2021); Correspondence analysis, spectral clustering and graph embedding: applications to ecology and economic complexity; _Scientific Reports_; DOI: 10.1038/s41598-021-87971-9

#### Included data set
Faurby, Søren e.a; 2019; [HYLACINE 1.2: The Phylogenetic Atlas of Mammal Macroecology](https://datadryad.org/stash/dataset/doi:10.5061/dryad.bp26v20)


### The team

The team members are:

* Mathematical foundations  of the code
   - Alje van Dam, Copernicus Institute of Sustainable Development and Centre for Complex Systems Studies, Utrecht University, the Netherlands
   - Mark Dekker, Department of Information and Computing Sciences and Centre for Complex Systems Studies, Utrecht University, the Netherlands

* Programming and packaging
   - [Kees van Eijden](k.vaneijden@uu.nl) Research Engineering/ITS, Utrecht University, the Netherlands

* With contributions of
   - Ignacio Morales Castilla, Global Change Ecology and Evolution Group, Department of Life Sciences, University of Alcala´, Spain 
   - Jonathan de Bruin, Research Engineering/ITS, Utrecht University, the Netherlands
   - Raoul Schram, Research Engineering/ITS, Utrecht University, the Netherlands
   - Mara Baudena, National Research Council of Italy, Institute of Atmospheric Science and Climate (CNR-ISAC), Turin, Italy; Copernicus Institute of Sustainable Development and Centre for Complex Systems Studies, Utrecht University, the Netherlands


### How to cite SCCA

To cite the SCCA repository and R package, use `citation("SCCA")` to retrieve the BibTex entry. Otherwise use the following format:

van Eijden, Kees et al; 2021; SCCA: Spectral Clustering Correspondence Analysis in R; Utrecht University; DOI: 10.5281/zenodo.4665670.
Also available at [Utrecht University](https://github.com/UtrechtUniversity/SCCA). 

Please also cite the paper [van Dam et al, 2021](#article) when using the SCCA repository.

