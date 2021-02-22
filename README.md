---
title: "SCCA: Spectral Clustering Correspondence Analysis in R"
output:
  html_document: default
  pdf_document: default
---

[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)


### Introduction

The SCCA package implements in R the methodological approach to CA as proposed in **Correspondence analysis, spectral clustering and graph embedding: applications to ecology and economic complexity** (van Dam et al; 2021).

### Installation

The package can be installed directly from Github with the code below. Ensure the package `devtools` has been installed.

```
#install.packages("devtools")
library(devtools)
install_github("UtrechtUniversity/scca", build_vignettes = TRUE)
```

### Exported functions and data sets

After loading the package a list of all exported functions and data sets can be retrieved by `?SCCA` and the documentation of an individual function by `?<function name>`; e.g. `?scca_compute`.

The methodology and the use of the functions and the data are explained in the included vignettes. After installing package SCCA use `browseVignettes('SCCA')` in the R(Studio) console.

### License

The software code is licensed under [**MIT**](https://opensource.org/licenses/MIT). The next section (References) provides links to 
sources of the included datasets. See there for licences of those data sets.

### References

Dam, Alje van., e.a. 2021; **Correspondence analysis, spectral clustering and graph embedding: applications to ecology and economic complexity**; *name of journal*; DOI: <doi>.

#### Included data sets
Faurby, Søren e.a; 2019; [HYLACINE 1.2: The Phylogenetic Atlas of Mammal Macroecology](https://datadryad.org/stash/dataset/doi:10.5061/dryad.bp26v20)

[Growth Growth Lab at Harvard University](http://atlas.cid.harvard.edu/about-data)

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

Dam, Alje van, e.a. 2021. "SCCA: Spectral Clustering Correspondence Analysis in R". Utrecht University. DOI: 12.1234/xxxxxx.1234567.
Available at [Utrecht University](https://github.com/UtrechtUniversity/SCCA). 


