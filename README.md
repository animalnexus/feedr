
# feedr

feedr is a package for R. This collection of functions is designed to load and transform data about feeder visits from birds with RFID tags.

## Installation
feedr is available from github and can be installed directly with the package 'devtools'

```r
# install.packages("devtools")
devtools::install_github("steffilazerte/feedr")
```

## Help

An overview of this package and how to use the functions is accessible [here](http://steffi.ca/thinkR/feedr.html).

For documentation and examples on how to use a particular function use the `?` R command. For example:

```r
?load.raw.all
?visits
```

## Demos

And there is one demo for using `map.leaflet()` which you can run as follows:

```r
demo("demo-leaflet", package = "feedr")
```
