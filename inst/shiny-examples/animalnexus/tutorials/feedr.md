<a id = "top"></a> <!--
rmarkdown::render("navigation.Rmd", output_file = paste0("../Reports/Final Reports/NAVIGATION.html"))
-->

This is an overview of how to use functions in the `feedr` package to
transform and visual your RFID data.

Order of operations
-------------------

This package contains functions to perform three broad services:

1.  **Loading/Importing Data**  
    Raw data downloaded from the feeders or from the birdmoves website
2.  **Housekeeping**  
    Functions for checking and correcting errors before you transform
    the data
3.  **Transformations**  
    When raw data is transformed into visit data, feeding data, etc.
    which are the output of the `visits()`, `feeding()`, etc. functions
4.  **Visualizations**  
    Static or interactive maps produced by the mapping functions which
    visualize the movement and feeding patterns around your feeders.

As such, several functions depend on the output of other functions.

For example, to get daily activity patterns, you could get your data
from the web with `dl_data()`, turn this raw data to visits data with
`visits()`, turn visits data to activity data with `activity()` and
finally, turn activity data into daily activity patterns with `daily()`.

<img src="/home/steffi/Projects/feedr/inst/shiny-examples/animalnexus/tutorials/feedr_files/figure-markdown_strict/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

Tutorial Data
-------------

A data frame called 'finches' is included in this package for the
purpose of demonstration. You can access this package by mearly typing
in `finches`.

-   [Loading/Importing data](load.hml)
-   Checking data
-   Transforming data
-   Visualizing data

Function Index
--------------

### [Loading/Importing Data](load.html)

-   **load\_raw():** Load in a single file of raw feeder data
-   **load\_raw\_all():** Load in and combine multiple files of raw
    feeder data
-   **load\_web():** Load in csv file previously downloaded
-   **dl\_data():** Download and load data directly from the birdmoves
    website

### [Housekeeping](housekeeping.html)

-   **check\_ids():** Remove known bad ids (i.e. errors, wands, etc.)
-   **check\_problems():** Fix known id problems

### [Transformations](transformations.html)

-   **visits():** Turn raw data into visits data
-   **move():** Turn visits data into movements between feeders
-   **feeding():** Turn visits data into feeding bouts
-   **disp():** Turn visits data into displacements
-   **dom():** Turn displacements into dominance matrices and
    hierarchies
-   **activity():** Turn visits data into activity data
-   **daily():** Turn activity data into daily activity patterns

[Visualizations](maps.html)
---------------------------

-   **map\_ggmap():** Visualize movements and/or feeding bouts with
    ggmap (static)
-   **map\_leaflet():** Visualize movements and/or feeding bouts with
    leaflet (interactive)

------------------------------------------------------------------------

Back to [top](#top)  
Continue with [loading/importing data](load.html)
