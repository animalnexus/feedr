[![Travis-Ci Build Status](https://travis-ci.org/animalnexus/feedr.svg?branch=master)](https://travis-ci.org/animalnexus/feedr)

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/animalnexus/feedr?branch=master&svg=true)](https://ci.appveyor.com/project/animalnexus/feedr)

# feedr

`feedr` is a package for [R](https://www.r-project.org/). This collection of functions is designed to load, transform and visualize RFID data created when pit-tagged animals are detected by RFID loggers associated with static stations (e.g., seed-feeders, nestboxes, nectar-feeders, etc.).

## Installation

To ensure the smoothest possible installation, it is recommended that you start a new R session before installing. 

`feedr` is available from github and can be installed directly with the package 'devtools'. 

```r
install.packages("devtools") # if not already installed
devtools::install_github("animalnexus/feedr")
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
devtools::install_github("animalnexus/feedr")
```

### Linux
On linux systems some packages require libraries to be installed on the system first. For Debian distributions (e.g., Ubuntu, etc.) these can be installed with the following command in the terminal:

Libraries for `devtools`: `sudo apt install libcurl4-openssl-dev libssh2-1-dev libssl-dev`

Libraries for `feedr`: `sudo apt install libpq-dev libxml2-dev`

## Usage
We have an extensive tutorial hosted on our github pages: <https://animalnexus.github.io/feedr/>

## Demos
There is a demo for using `map_leaflet()` which you can run as follows:

```r
demo("demo-leaflet", package = "feedr")
```
