[![Travis-Ci Build Status](https://travis-ci.org/steffilazerte/feedr.svg?branch=master)](https://travis-ci.org/steffilazerte/feedr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/steffilazerte/feedr?branch=master&svg=true)](https://ci.appveyor.com/project/steffilazerte/feedr)
# feedr

`feedr` is a package for [R](https://www.r-project.org/). This collection of functions is designed to load, transform and visualize RFID data created when pit-tagged animals are detected by RFID loggers associated with static stations (e.g., seed-feeders, nestboxes, nectare-feeders, etc.).

## Installation
`feedr` is available from github and can be installed directly with the package 'devtools':

```r
install.packages("devtools")
devtools::install_github("steffilazerte/feedr")
```

### Windows
If you run R on Windows you may run into an error along the lines of:
```
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), version Check = vI[[j]]) :
  there is no package called 'Rcpp'
ERROR: lazy loading failed for package 'leaflet'
```

This is a bug with the current version of `devtools` which results in some dependencies (other packages used by `feedr`) being missed in the installation. A quick fix is to install these packages directly, and then to continue with the `feedr` installation:

```r
install.packages(c("dplyr", "ggplot2", "htmlwidgets", "scales", "shiny", "stringr"))
devtools::install_github("steffilazerte/feedr")
```

### Linux (e.g., Ubuntu)
On linux systems some packages require libraries to be installed on the system first. For Debian distributions (e.g., Ubuntu, etc.) these can be installed with the following command in the terminal:

Libraries for `devtools`: `sudo apt install libcurl4-openssl-dev libssh2-1-dev libssl-dev`

Libraries for `feedr`: `sudo apt install libpq-dev libxml2-dev`

## Help

** Note: ** These tutorials are in the process of being updated.

An overview of this package and how to use the functions is accessible [here](http://steffi.ca/thinkR/feedr.html).

This is also accessible as a vignette in R, provided you install the package with vignettes built:

```r
devtools::install_github("steffilazerte/feedr", build_vignettes = TRUE)
```

For documentation and examples on how to use a particular function use the `?` R command. For example:

```r
?load_raw_all
?visits
```

## Demos

And there is one demo for using `map_leaflet()` which you can run as follows:

```r
demo("demo-leaflet", package = "feedr")
```
