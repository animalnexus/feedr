<!-- badges: start -->
[![R-CMD-check](https://github.com/animalnexus/feedr/workflows/R-CMD-check/badge.svg)](https://github.com/animalnexus/feedr/actions)
<!-- badges: end -->
  
# feedr

`feedr` is a package for [R](https://www.r-project.org/). This collection of functions is designed to load, transform and visualize RFID data created when pit-tagged animals are detected by RFID loggers associated with static stations (e.g., seed-feeders, nestboxes, nectar-feeders, etc.).

## Installation

To ensure the smoothest possible installation, it is recommended that you start a new R session before installing. 

`feedr` is available from GitHub and can be installed directly with the package 'remotes'. 

```r
install.packages("remotes") # if not already installed
remotes::install_github("animalnexus/feedr")
```


## Usage
We have an extensive tutorial hosted on our github pages: <https://animalnexus.github.io/feedr/>

## Demos
There is a demo for using `map_leaflet()` which you can run as follows:

```r
demo("demo-leaflet", package = "feedr")
```
