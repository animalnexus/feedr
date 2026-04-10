
<!-- badges: start -->

[![R-CMD-check](https://github.com/animalnexus/feedr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/animalnexus/feedr/actions/workflows/R-CMD-check.yaml)
![Code Coverage:
75%](https://img.shields.io/badge/code_coverage-75%-yellowgreen)
<!-- badges: end -->

# feedr

feedr is an R package designed to load, transform and visualize RFID
data created when pit-tagged animals are detected by RFID loggers
associated with static stations (e.g., seed-feeders, nest boxes,
nectar-feeders, etc.).

## Installation

To ensure the smoothest possible installation, it is recommended that
you start a new R session before installing.

feedr is available from GitHub and can be installed directly with the
package ‘pak’.

``` r
install.packages("pak") # if not already installed
pak::pkg_install("animalnexus/feedr")
```

## History & Citation

feedr was originally (back in 2015/2016) developed as an R package *and*
Shiny App to be hosted by the animal**nexus** project. This was a large
collaborative effort to develop tools for the observation,
visualization, and analysis of animal movements registered by RFID
feeders or other static recording stations.

However, over the years the group has been exploring other avenues of
research.

In order to keep maintenance of the feedr package manageable, it was
split into two packages, the core functions (feedr) and the Shiny UI
([feedrUI](https://github.com/animalnexus/feedrUI)).

**The feedr package is actively maintained by Steffi LaZerte, but the
feedrUI package is currently on hold.**

We published an introduction to the pacagek in Ecology in Evolution in
2017 which you can use to cite this package:

LaZerte SE, Reudink MW, Otter KA, Kusack J, Bailey JM, Woolverto, A,
Paetkau M, de Jong A, and Hill DJ. feedr and animalnexus.ca: A paired R
package and user-friendly Web application for transforming and
visualizing animal movement data from static stations. Ecol Evol. 2017;
7: 7884–7896. <https://doi.org/10.1002/ece3.3240>

## Demos

There is a demo for using `map_leaflet()` which you can run as follows:

``` r
demo("demo-leaflet", package = "feedr")
```
