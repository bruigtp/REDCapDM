[REDCapDM](https://bruigtp.github.io/REDCapDM/articles/REDCapDM.html)  <img src="man/figures/logo.png" align="right" width="250"/>
=======

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/REDCapDM)](https://cran.r-project.org/package=REDCapDM) &#160;&#160; [![R-CMD-check](https://github.com/bruigtp/REDCapDM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bruigtp/REDCapDM/actions/workflows/R-CMD-check.yaml) &#160;&#160; [![downloads](http://cranlogs.r-pkg.org/badges/REDCapDM)](https://cranlogs.r-pkg.org:443/) &#160;&#160; [![total](http://cranlogs.r-pkg.org/badges/grand-total/REDCapDM)](https://cranlogs.r-pkg.org:443/)
<!-- badges: end -->


`REDCapDM` is an R package that allows users to manage data exported directly from REDCap or through an API connection. This package includes several functions designed for preprocessing data, generating reports on queries like outliers or missing values, and performing a follow-up of each identified query. 'REDCap' (Research Electronic Data CAPture; <https://projectredcap.org>) is a web application developed at Vanderbilt University, designed for creating and managing online surveys and databases. The REDCap API serves as an interface allowing external applications to connect to REDCap remotely, and is used to programmatically retrieve or modify project data or settings within REDCap, such as importing or exporting data.

The [REDCapDM website](https://bruigtp.github.io/REDCapDM/articles/REDCapDM.html) has a description of the package functions as well as access to the package vignettes.

Here you can access the published article in BMC Medical Research Methodology: [REDCapDM: An R package with a set of data management tools for a REDCap project](https://doi.org/10.1186/s12874-024-02178-6)



### Installation

The *release* version can be installed from [CRAN](https://cran.r-project.org/package=REDCapDM).

```r
install.packages("REDCapDM")
```

The *development* version can be installed from [GitHub](https://github.com/bruigtp/REDCapDM) after installing the `remotes` package.

```r
install.packages("remotes") # Run this line if the 'remotes' package isn't installed already.
remotes::install_github("OuhscBbmc/REDCapR")
```

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/bruigtp/REDCapDM/issues).

## About

Package: REDCapDM

Authors: João Carmezim, Pau Satorra, Judith Peñafiel, Esther García, Natàlia Pallarès, Cristian Tebé.

Maintainer: João Carmezim

License: MIT + file LICENSE

Encoding: UTF-8

Depends: R (>= 3.6.0)
