<a id = "top"></a>

Here we will go over visualizing your data with maps. `feedr` includes
two wrapper functions that can be used to visualize your data on maps.
These two functions, `map_leaflet` and `map_ggmap` are really just
convenience functions to do a lot of the grunt work. If you want to
achieve a more fine tuned map, I suggest you check out the packages
`leaflet` and `ggmap`.

These mapping functions take summarized feeding and movement data and
turn it into circles and paths around/between feeders representing time
spent at different feeders and the amount of movement between particular
feeder paths.

Because different projects may need different summaries (totals vs.
means, corrected for number of individuals, uncorrected, etc.) we will
first create the summary data and then pass this to the functions for
plotting. This gives us more flexibility, but does mean that we have to
do some work before we can visualize the data.

Summarizing data
----------------

First we will calculate our movement and feeding data. Because we will
summarize according to the number of individual chickadees *in each
experiment* we will also calculate a new `bird_n`.

    library(plyr)

    v <- visits(chickadees)
    v <- ddply(v, "experiment", transform, bird_n_exp = length(unique(bird_id)))
    head(v)

    ##      bird_id               start                 end feeder_id bird_n
    ## 1 0620000006 2015-12-05 12:57:48 2015-12-05 12:57:48 exp1-GR10     33
    ## 2 06200000F7 2015-12-05 15:00:28 2015-12-05 15:00:30 exp1-GR10     33
    ## 3 06200000F7 2015-12-07 10:16:05 2015-12-07 10:16:05 exp1-GR10     33
    ## 4 06200000F7 2015-12-08 08:58:10 2015-12-08 08:58:10 exp1-GR10     33
    ## 5 06200000F7 2015-12-08 09:03:08 2015-12-08 09:03:08 exp1-GR10     33
    ## 6 06200000F7 2015-12-08 09:10:44 2015-12-08 09:10:44 exp1-GR10     33
    ##   feeder_n experiment      lat      lon bird_n_exp
    ## 1        7       exp1 53.89404 -122.818         13
    ## 2        7       exp1 53.89404 -122.818         13
    ## 3        7       exp1 53.89404 -122.818         13
    ## 4        7       exp1 53.89404 -122.818         13
    ## 5        7       exp1 53.89404 -122.818         13
    ## 6        7       exp1 53.89404 -122.818         13

The `transform()` function adds a new column to the current data frame,
as opposed to replacing the whole dataset (`summarize()`). `length()`
calculates the total number of items, and `unique()` returns only unique
values. Thus we have a new column `bird_n_exp` which contains the number
of unique bird\_ids for each value of `experiment`.

Now calculate our movements and feeding times:

    m <- ddply(v, "bird_id", move)
    f <- ddply(v, "bird_id", feeding)

    head(m)

    ##      bird_id                time feeder_id direction            move_dir
    ## 1 06200000F7 2015-12-11 13:10:14 exp1-GR10      left exp1-GR10_exp1-GR13
    ## 2 06200000F7 2015-12-15 12:20:02 exp1-GR13   arrived exp1-GR10_exp1-GR13
    ## 3 062000014F 2015-12-15 11:59:38 exp1-GR12      left exp1-GR12_exp1-GR13
    ## 4 062000014F 2015-12-15 12:11:05 exp1-GR13   arrived exp1-GR12_exp1-GR13
    ## 5 062000014F 2015-12-16 10:18:04 exp1-GR13      left exp1-GR13_exp1-GR12
    ## 6 062000014F 2015-12-16 10:31:54 exp1-GR12   arrived exp1-GR13_exp1-GR12
    ##             move_path   strength bird_n feeder_n experiment bird_n_exp
    ## 1 exp1-GR10_exp1-GR13 0.01050825     33        7       exp1         13
    ## 2 exp1-GR10_exp1-GR13 0.01050825     33        7       exp1         13
    ## 3 exp1-GR12_exp1-GR13 5.24017467     33        7       exp1         13
    ## 4 exp1-GR12_exp1-GR13 5.24017467     33        7       exp1         13
    ## 5 exp1-GR12_exp1-GR13 4.33734940     33        7       exp1         13
    ## 6 exp1-GR12_exp1-GR13 4.33734940     33        7       exp1         13
    ##        lat       lon
    ## 1 53.89404 -122.8180
    ## 2 53.89410 -122.8201
    ## 3 53.89324 -122.8208
    ## 4 53.89410 -122.8201
    ## 5 53.89410 -122.8201
    ## 6 53.89324 -122.8208

    head(f)

    ##      bird_id feeder_id          feed_start            feed_end feed_length
    ## 1 03000314F9 exp2-GR13 2016-01-06 13:18:44 2016-01-06 13:18:45  0.01666667
    ## 2 0620000006 exp1-GR10 2015-12-05 12:57:48 2015-12-05 12:57:48  0.00000000
    ## 3 0620000062 exp2-GR13 2016-01-12 15:32:45 2016-01-12 15:32:45  0.00000000
    ## 4 06200000F7 exp1-GR10 2015-12-05 15:00:28 2015-12-05 15:00:30  0.03333333
    ## 5 06200000F7 exp1-GR10 2015-12-07 10:16:05 2015-12-07 10:16:05  0.00000000
    ## 6 06200000F7 exp1-GR10 2015-12-08 08:58:10 2015-12-08 09:49:13 51.05000000
    ##   bird_n feeder_n experiment      lat       lon bird_n_exp
    ## 1     33        7       exp2 53.89088 -122.8208         24
    ## 2     33        7       exp1 53.89404 -122.8180         13
    ## 3     33        7       exp2 53.89088 -122.8208         24
    ## 4     33        7       exp1 53.89404 -122.8180         13
    ## 5     33        7       exp1 53.89404 -122.8180         13
    ## 6     33        7       exp1 53.89404 -122.8180         13

There are many ways we can summarize the data:

-   by population
-   by individual
-   totals (sums)
-   means
-   totals corrected for number of birds

### By Population

Calculate total feeding time at each feeder, corrected for the number of
birds total

    f.all <- ddply(f, "feeder_id", summarise,
                  amount = sum(feed_length) / bird_n_exp[1])
    head(f.all)

    ##   feeder_id    amount
    ## 1 exp1-GR10 390.18205
    ## 2 exp1-GR12  73.34359
    ## 3 exp1-GR13 291.17308
    ## 4 exp2-GR10  88.76667
    ## 5 exp2-GR11 220.62292
    ## 6 exp2-GR12 141.27986

We use `bird_n_exp[1]` because we want to divide the total sum by the
number of birds in each experiment. However, as the value `bird-n_exp`
is repeated, we only need the first one, hence the `[1]`.

If we wanted to calculate mean feeding time, we would use
`amount = mean(feed_length)`  
If we wanted to calculate total feeding time with no correction, we
would use `amount = sum(feed_length)`

**Note:** The new summary data set must have the column `amount`,
regardless of how it is calculated. That name is what the mapping
functions look for.

Now let's summarize the movement data for each movement path between
feeders. We can summarize individually for each feeder, which will
account for the double counting (one row for leaving the feeder and one
row for arriving) as well as making sure we have `feeder_id` in the
final data set.

    m.all <- ddply(m, c("feeder_id", "move_path"), summarise,
                   path_use = length(move_path) / bird_n_exp[1])
    m.all

    ##    feeder_id           move_path    path_use
    ## 1  exp1-GR10 exp1-GR10_exp1-GR12  0.30769231
    ## 2  exp1-GR10 exp1-GR10_exp1-GR13  4.92307692
    ## 3  exp1-GR10 exp1-GR10_exp2-GR11  0.07692308
    ## 4  exp1-GR12 exp1-GR10_exp1-GR12  0.30769231
    ## 5  exp1-GR12 exp1-GR12_exp1-GR13  2.69230769
    ## 6  exp1-GR13 exp1-GR10_exp1-GR13  4.92307692
    ## 7  exp1-GR13 exp1-GR12_exp1-GR13  2.69230769
    ## 8  exp1-GR13 exp1-GR13_exp2-GR10  0.07692308
    ## 9  exp1-GR13 exp1-GR13_exp2-GR13  0.15384615
    ## 10 exp2-GR10 exp1-GR13_exp2-GR10  0.04166667
    ## 11 exp2-GR10 exp2-GR10_exp2-GR11  0.33333333
    ## 12 exp2-GR10 exp2-GR10_exp2-GR12  3.70833333
    ## 13 exp2-GR10 exp2-GR10_exp2-GR13  1.08333333
    ## 14 exp2-GR11 exp1-GR10_exp2-GR11  0.04166667
    ## 15 exp2-GR11 exp2-GR10_exp2-GR11  0.33333333
    ## 16 exp2-GR11 exp2-GR11_exp2-GR12  2.16666667
    ## 17 exp2-GR11 exp2-GR11_exp2-GR13 15.25000000
    ## 18 exp2-GR12 exp2-GR10_exp2-GR12  3.70833333
    ## 19 exp2-GR12 exp2-GR11_exp2-GR12  2.16666667
    ## 20 exp2-GR12 exp2-GR12_exp2-GR13  1.54166667
    ## 21 exp2-GR13 exp1-GR13_exp2-GR13  0.08333333
    ## 22 exp2-GR13 exp2-GR10_exp2-GR13  1.08333333
    ## 23 exp2-GR13 exp2-GR11_exp2-GR13 15.25000000
    ## 24 exp2-GR13 exp2-GR12_exp2-GR13  1.54166667

**Note:** The new summary data set must have the column `path_use`,
regardless of how it is calculated. That name is what the mapping
functions look for.

Now we've summarized our data, all we need is a list of lat/lons for
each `feeder_id`. Fortunately, we have that data stored in all the
non-summarized data frames, so it's merely a matter of cleaning it up.
`unique()` will extract all non-duplicated rows of this subset of `f`
including only the columns we've specified here:

    l <- unique(f[, c("feeder_id", "lat", "lon")])
    head(l)

    ##    feeder_id      lat       lon
    ## 1  exp2-GR13 53.89088 -122.8208
    ## 2  exp1-GR10 53.89404 -122.8180
    ## 16 exp1-GR13 53.89410 -122.8201
    ## 19 exp1-GR12 53.89324 -122.8208
    ## 32 exp2-GR10 53.89086 -122.8193
    ## 33 exp2-GR12 53.88997 -122.8193

In this example, we also have a feeder that was never used by
chickadees, but we would still like to visualize it, so we will add it
to the locations list:

    l <- rbind(l, data.frame(feeder_id = "exp1-GR11", lat = 53.89304, lon = -122.81827))

Alternatively, we could have loaded in an external feeder index and used
that.

### By Individual

We can also summarize data by individual for plotting of individual
maps.

This is virtually identical to what we did above, except that we add one
more variable to the grouping in our `ddply()` functions and we don't
correct for the number of birds:

    f.indiv <- ddply(f, c("experiment", "feeder_id", "bird_id"), summarise,
                  amount = sum(feed_length))
    m.indiv <- ddply(m, c("experiment", "feeder_id", "move_path", "bird_id"), summarise,
                   path_use = length(move_path) / 2)
    head(f.indiv)

    ##   experiment feeder_id    bird_id    amount
    ## 1       exp1 exp1-GR10 0620000006    0.0000
    ## 2       exp1 exp1-GR10 06200000F7  146.9833
    ## 3       exp1 exp1-GR10 0620000318 1027.1333
    ## 4       exp1 exp1-GR10 0620000384  168.4500
    ## 5       exp1 exp1-GR10 062000039D 1034.8500
    ## 6       exp1 exp1-GR10 06200003DE  965.9000

    head(m.indiv)

    ##   experiment feeder_id           move_path    bird_id path_use
    ## 1       exp1 exp1-GR10 exp1-GR10_exp1-GR12 0620000418      1.5
    ## 2       exp1 exp1-GR10 exp1-GR10_exp1-GR12 0620000525      0.5
    ## 3       exp1 exp1-GR10 exp1-GR10_exp1-GR13 06200000F7      0.5
    ## 4       exp1 exp1-GR10 exp1-GR10_exp1-GR13 0620000318      4.5
    ## 5       exp1 exp1-GR10 exp1-GR10_exp1-GR13 0620000384      6.0
    ## 6       exp1 exp1-GR10 exp1-GR10_exp1-GR13 062000039D      2.5

### Recap:

To create maps you need the following:

-   feeding summary with a columns: `feeder_id` and `amount`
-   movement summary with a columns: `feeder_id`, `move_path` and
    `path_use`
-   location summary with columns: `feeder_id`, `lat` and `lon`
    -   (but the mapping functions will also accept `latitude`,
        `longitude` and `long`)
-   If locations are included in feeding or movement datasets, you don't
    need the extra location dataset.

If you want to look at individual data, add `bird_id` to the grouping in
the `ddply()` function, otherwise omit it.

Mapping
-------

<a id = "leaflet"></a>

`map_leaflet()`
---------------

This function creates an interactive html map using leaflet for R
(package `leaflet`).

    map_leaflet(u = f.all, p = m.all, locs = l)

![](/home/steffi/Projects/feedr/inst/shiny-examples/animalnexus/tutorials/maps_files/figure-markdown_strict/unnamed-chunk-9-1.png)

The map is interactive in that it can be zoomed, the tiles changed, and
different elements added or removed. Feeders (black dots) can also be
clicked on to reveal ids and circles and paths can clicked on to reveal
their actual numbers.

You can also adjust some of the cosmtic details:

    map_leaflet(u = f.all, p = m.all, locs = l, 
                u_scale = 5, p_scale = 5,
                u_title = "Feeding (min)", p_title = "Paths",
                u_pal = c("blue","white"),
                p_pal = c("black","red"))

![](/home/steffi/Projects/feedr/inst/shiny-examples/animalnexus/tutorials/maps_files/figure-markdown_strict/unnamed-chunk-10-1.png)

To plot by individual, simply use our individually summarized data sets:

    map_leaflet(u = f.indiv, p = m.indiv, locs = l)

![](/home/steffi/Projects/feedr/inst/shiny-examples/animalnexus/tutorials/maps_files/figure-markdown_strict/unnamed-chunk-11-1.png)

Note, however, that for leaflet maps, the individual lines are stacked
in order of magnitude and visualizations like this may not be very
useful for large numbers of individuals.

To save this map, you can zoom and set it up as you like and use the
"Export &gt; Save as image" button in RStudio.

Alternatively, you can use the `map_ggmap()` function to visualize a
static map.

<table>
<tbody>
<tr class="odd">
<td align="left">Back to <a href="#top">top</a></td>
</tr>
</tbody>
</table>

<a id = "ggmap"></a>

`map_ggmap()`
-------------

This creates static maps using the package `ggmap`. This map may take
longer to create as it downloads background images from Google Maps (but
you can also specify the source, see below).

    map_ggmap(u = f.all, p = m.all, locs = l)

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=53.89196,-122.81972&zoom=17&size=640x640&scale=2&maptype=satellite&language=en-EN&sensor=false

    ## Warning: Removed 2 rows containing missing values (geom_point).

    ## Warning: Removed 5 rows containing missing values (geom_path).

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](/home/steffi/Projects/feedr/inst/shiny-examples/animalnexus/tutorials/maps_files/figure-markdown_strict/unnamed-chunk-12-1.png)

Getting the zoom level often requires some trial and error (default =
17). If you see a warning about rows with missing values omitted, this
means that not all your data could fit on the map.

    map_ggmap(u = f.all, p = m.all, locs = l, zoom = 16)

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=53.89196,-122.81972&zoom=16&size=640x640&scale=2&maptype=satellite&language=en-EN&sensor=false

![](/home/steffi/Projects/feedr/inst/shiny-examples/animalnexus/tutorials/maps_files/figure-markdown_strict/unnamed-chunk-13-1.png)

    map_ggmap(u = f.all, p = m.all, locs = l, 
              zoom = 16, u_scale = 0.7)

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=53.89196,-122.81972&zoom=16&size=640x640&scale=2&maptype=satellite&language=en-EN&sensor=false

![](/home/steffi/Projects/feedr/inst/shiny-examples/animalnexus/tutorials/maps_files/figure-markdown_strict/unnamed-chunk-14-1.png)

Note that the lines between the two sites shows that some individual
chickadees visited both sites

    map_ggmap(u = f.all, p = m.all, locs = l, 
              zoom = 16, u_scale = 0.7, maptype = "terrain")

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=53.89196,-122.81972&zoom=16&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false

![](/home/steffi/Projects/feedr/inst/shiny-examples/animalnexus/tutorials/maps_files/figure-markdown_strict/unnamed-chunk-15-1.png)

    map_ggmap(u = f.all, p = m.all, locs = l, 
              zoom = 16, maptype = "roadmap")

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=53.89196,-122.81972&zoom=16&size=640x640&scale=2&maptype=roadmap&language=en-EN&sensor=false

![](/home/steffi/Projects/feedr/inst/shiny-examples/animalnexus/tutorials/maps_files/figure-markdown_strict/unnamed-chunk-16-1.png)

For individuals:

    map_ggmap(u = f.indiv, p = m.indiv, locs = l, 
              zoom = 16, u_scale = 0.7)

    ## Error in map_ggmap(u = f.indiv, p = m.indiv, locs = l, zoom = 16, u_scale = 0.7): You have chosen to run this function on more than 10 birds. This may overload your system. We recommend trying again using the 'which' argument to specify a subset of birds.

Okay, let's try with fewer birds

    sub_birds <- unique(f.indiv$bird_id) #Get first 5 bird_ids

    # Use only f.indiv/m.indiv where the bird_id is in these first 10.
    map_ggmap(u = f.indiv, p = m.indiv, locs = l, 
              zoom = 16, u_scale = 0.7,
              which = sub_birds[1:10])

    ## Some bird_ids removed due to lack of data: 0620000006, 0620000400

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=53.89196,-122.81972&zoom=16&size=640x640&scale=2&maptype=satellite&language=en-EN&sensor=false

    ## You have specified multiple birds and static maps, this means that an individual map will be drawn for each bird. This may take some time to display in the plots window.

![](/home/steffi/Projects/feedr/inst/shiny-examples/animalnexus/tutorials/maps_files/figure-markdown_strict/unnamed-chunk-18-1.png)

    # Use only f.indiv/m.indiv where the bird_id is in these second 10.
    map_ggmap(u = f.indiv, p = m.indiv, locs = l, 
              zoom = 16, u_scale = 0.7,
              which = sub_birds[11:20])

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=53.89196,-122.81972&zoom=16&size=640x640&scale=2&maptype=satellite&language=en-EN&sensor=false
    ## You have specified multiple birds and static maps, this means that an individual map will be drawn for each bird. This may take some time to display in the plots window.

![](/home/steffi/Projects/feedr/inst/shiny-examples/animalnexus/tutorials/maps_files/figure-markdown_strict/unnamed-chunk-18-2.png)

Note that one individual (0620000006) was removed. This is because the
individual had a feed\_length of 0 and no movements. This situation will
occur if an individual makes only one visit to a feeder (resulting in a
visit length of 0, and thus a feeding length of 0).

------------------------------------------------------------------------

Back to [top](#top)  
Go back to [main document](feedr.html) | Go back to
[transformations](transformations.html)
