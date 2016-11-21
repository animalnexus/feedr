[![Build Status](https://travis-ci.org/steffilazerte/feedr.svg?branch=master)](https://travis-ci.org/steffilazerte/feedr)

# feedr

`feedr` is a package for R. This collection of functions is designed to load and transform data about feeder visits from birds with RFID tags.

## Installation
`feedr` is available from github and can be installed directly with the package 'devtools'

```r
# install.packages("devtools")
devtools::install_github("steffilazerte/feedr")
```

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
