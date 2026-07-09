# [REDCapDM](https://bruigtp.github.io/REDCapDM/articles/REDCapDM.html)

[![CRAN
status](https://www.r-pkg.org/badges/version/REDCapDM)](https://cran.r-project.org/package=REDCapDM)
  
[![R-CMD-check](https://github.com/bruigtp/REDCapDM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bruigtp/REDCapDM/actions/workflows/R-CMD-check.yaml)
   [![Codecov test
coverage](https://codecov.io/gh/bruigtp/REDCapDM/graph/badge.svg)](https://app.codecov.io/gh/bruigtp/REDCapDM)
[![](https://cranlogs.r-pkg.org/badges/REDCapDM)](https://cran.r-project.org/package=REDCapDM)
  
[![](https://cranlogs.r-pkg.org/badges/grand-total/REDCapDM)](https://cran.r-project.org/package=REDCapDM)

The R package
[**REDCapDM**](https://bruigtp.github.io/REDCapDM/articles/REDCapDM.html)
has been developed with the objective of facilitating the management of
data for projects using the REDCap platform. It is capable of supporting
both direct data export and access via the REDCap API. REDCapDM offers a
comprehensive variety of functions, including the ability to preprocess
data, generate detailed query reports for issues such as outliers or
missing values, and track the resolution of each identified query.

[**REDCap**](https://projectredcap.org) (Research Electronic Data
CAPture) is a widely-used web application developed at Vanderbilt
University for creating and managing online surveys and databases. It
includes an API (Application Programming Interface) that serves as an
interface allowing external applications to connect to REDCap remotely,
and is used to programmatically retrieve or modify project data or
settings within REDCap, such as importing or exporting data.

### Installation

The *release* version can be installed from
[CRAN](https://cran.r-project.org/package=REDCapDM).

``` r

install.packages("REDCapDM")
```

The *development* version can be installed from
[GitHub](https://github.com/bruigtp/REDCapDM) after installing the
`remotes` package.

``` r

install.packages("remotes") # Run this line if the 'remotes' package isn't installed already.
remotes::install_github("bruigtp/REDCapDM")
```

### Getting Started

To learn more about the package’s functionality, visit the [**REDCapDM
website**](https://bruigtp.github.io/REDCapDM/articles/REDCapDM.html).
The site includes detailed descriptions of the package’s functions and
access to vignettes that demonstrate how to use REDCapDM effectively in
your projects.

### Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on [**GitHub:
Issues**](https://github.com/bruigtp/REDCapDM/issues).

### Published Work

For an in-depth exploration of REDCapDM, refer to the published article
in *BMC Medical Research Methodology*:  
[**REDCapDM: An R package with a set of data management tools for a
REDCap project**](https://doi.org/10.1186/s12874-024-02178-6)

### Citation

``` text
> citation("REDCapDM")

To cite package ‘REDCapDM’ in publications use:

  Carmezim J, Satorra P, Peñafiel J, García E, Pallarès N, Santos N, Tebé C (2024). “REDCapDM: An R package with a
  set of data management tools for a REDCap project.” _BMC Medical Research Methodology_, *24*(1), 55.
  doi:10.1186/s12874-024-02178-6 <https://doi.org/10.1186/s12874-024-02178-6>.

A BibTeX entry for LaTeX users is

  @Article{,
    title = {REDCapDM: An R package with a set of data management tools for a REDCap project},
    author = {João Carmezim and Pau Satorra and Judith Peñafiel and Esther García and Natàlia Pallarès and Naiara Santos and Cristian Tebé},
    journal = {BMC Medical Research Methodology},
    year = {2024},
    volume = {24},
    number = {1},
    pages = {55},
    doi = {10.1186/s12874-024-02178-6},
  }
```

### About

Package: REDCapDM

Authors: João Carmezim, Pau Satorra, Judith Peñafiel, Esther García,
Natàlia Pallarès, Cristian Tebé.

Maintainer: João Carmezim

License: MIT + file LICENSE

Encoding: UTF-8

Depends: R (\>= 3.6.0)
