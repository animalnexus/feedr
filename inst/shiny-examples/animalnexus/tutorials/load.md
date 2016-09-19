<a id = "top"></a>

This is a quick tutorial to get you started with loading in your own
data. `feedr` includes several wrapper functions that can be used to
load and format your data.

-   [`load_raw()`](#loadraw)
-   [`load_raw_web()`](#loadrawall)
-   [`load_web()`](#loadweb)
-   [`dl_data()`](#dldata)

These functions work with either **raw** feeder data (downloaded
directly from the feeders) or **web** feeder data downloaded from the
birdmoves websites (either by hand, from
<http://gaia.tru.ca/birdMOVES/datadownload.html>, or with the
`dl_data()` function.

A note about file structure
---------------------------

It's important to remember that when specifying file locations, you must
either specify a complete file id (e.g. /home/steffi/Desktop/data.csv)
**or** an appropriate relative file location.

Also, remember that file locations are relative to where R's working
directory is, and this is not necessarily the same place as the R script
with which you are working.

If you are using RStudio, it is highly recommended that you specify an
RStudio project in the directory which holds your scripts. This way,
anytime you open the file, the working directly is automatically set to
your script directly.

Otherwise you should probably use Session &gt; Set Working Directory
&gt; To Source File Location (if you're using RStudio).

**This tutorial assumes that your data is stored in a folder called
"Data" which is in turn stored in your R scripts folder.**

<a id="loadraw"></a>

`load_raw()`
------------

This loads and formats a raw data file downloaded directly from feeder
RFID readers.

    r1 <- load_raw("./Data/exp1/GR10DATA_2015_12_09.TXT")
    head(r1)

    ##      bird_id                time feeder_id
    ## 1 0620000006 2015-12-05 12:57:48      GR10
    ## 2 06200000F7 2015-12-05 15:00:28      GR10
    ## 3 06200000F7 2015-12-05 15:00:30      GR10
    ## 4 06200000F7 2015-12-07 10:16:05      GR10
    ## 5 06200000F7 2015-12-08 08:58:10      GR10
    ## 6 06200000F7 2015-12-08 09:03:08      GR10

Note that the feeder\_id is taken from the file name. This is done by
matching an expected pattern against the actual file name.

The default pattern matches GR or GPR followed by 1 or 2 digits. If you
need to specify a different pattern you can do so:

    r1 <- load_raw("./Data/exp1/Feeder_10_2015_12_09.TXT", feeder_pattern = "Feeder_[0-9]{2}")
    head(r1)

For more information on how to write patterns, see documentation for
"Regular Expressions" (e.g.
<http://www.regular-expressions.info/tutorial.html>)

<a id="loadrawall"></a>

`load_raw_all()`
----------------

The function `load_raw_all()` is a wrapper function which will
automatically load and combine data contained in several different files
in a single folder, or in a nested series of folders. Other files can be
present, but all feeder data files must be identifiable by a pattern in
the file name.

In this example our data files are stored in a folder called `raw` and
there are several sets of data, each corresponding to an individual
experiment which are then stored in their own folder called `exp1`,
`exp2`, etc. Feeder data files are identifiable by the characters 'DATA'
present in the name (as in the above example), which is the default.

    r <- load_raw_all(r_dir = "./Data/raw")
    head(r)

    ## Empty file skipped: /home/steffi/R/x86_64-pc-linux-gnu-library/3.3/feedr/extdata/raw/exp2/GR10DATA_2016_01_18.TXT

    ##      bird_id                time feeder_id
    ## 1 0620000006 2015-12-05 12:57:48      GR10
    ## 2 06200000F7 2015-12-05 15:00:28      GR10
    ## 3 06200000F7 2015-12-05 15:00:30      GR10
    ## 4 06200000F7 2015-12-07 10:16:05      GR10
    ## 5 06200000F7 2015-12-08 08:58:10      GR10
    ## 6 06200000F7 2015-12-08 09:03:08      GR10

If your feeder files didn't have a identiifable label, but were the only
csv files in the folders, you could use:

    r <- load_raw_all(r_dir = "./Data/raw", pattern = ".csv")

However, in this example we have several different experiments, which
we'll probably want to identify in our data. This is where the `extra`
arguments come in.

In our example, each experiment is stored in its own folder (exp1 and
exp2). Therefore we can tell our function to identify those patterns
(`extra_pattern`) and store the values in an extra column
(`extra_name`):

    r <- load_raw_all(r_dir = "./Data/raw", extra_pattern = "exp[1-2]{1}", extra_name = "experiment")

    ## Empty file skipped: /home/steffi/R/x86_64-pc-linux-gnu-library/3.3/feedr/extdata/raw/exp2/GR10DATA_2016_01_18.TXT

    ##      bird_id                time feeder_id experiment
    ## 1 0620000006 2015-12-05 12:57:48      GR10       exp1
    ## 2 06200000F7 2015-12-05 15:00:28      GR10       exp1
    ## 3 06200000F7 2015-12-05 15:00:30      GR10       exp1
    ## 4 06200000F7 2015-12-07 10:16:05      GR10       exp1
    ## 5 06200000F7 2015-12-08 08:58:10      GR10       exp1
    ## 6 06200000F7 2015-12-08 09:03:08      GR10       exp1

"exp\[1-2\]{1}" matches the exact characters "exp" followed by either a
1 or a 2 of which there is exactly 1.

We can also merge in some extra values for use later (visualizations,
etc.)

    ##   experiment feeder_id      lat       lon
    ## 1       exp1      GR10 53.89404 -122.8180
    ## 2       exp1      GR11 53.89304 -122.8183
    ## 3       exp1      GR12 53.89324 -122.8208
    ## 4       exp1      GR13 53.89410 -122.8201
    ## 5       exp2      GR10 53.89086 -122.8193
    ## 6       exp2      GR11 53.88999 -122.8210
    ## 7       exp2      GR12 53.88997 -122.8193
    ## 8       exp2      GR13 53.89088 -122.8208

    f.index <- read.csv("./Data/feeder_index.csv")
    f.index

    r <- merge(r, f.index, by = c("experiment", "feeder_id"))
    head(r)

    ##   experiment feeder_id    bird_id                time      lat      lon
    ## 1       exp1      GR10 0620000006 2015-12-05 12:57:48 53.89404 -122.818
    ## 2       exp1      GR10 06200000F7 2015-12-05 15:00:28 53.89404 -122.818
    ## 3       exp1      GR10 06200000F7 2015-12-05 15:00:30 53.89404 -122.818
    ## 4       exp1      GR10 06200000F7 2015-12-07 10:16:05 53.89404 -122.818
    ## 5       exp1      GR10 06200000F7 2015-12-08 08:58:10 53.89404 -122.818
    ## 6       exp1      GR10 06200000F7 2015-12-08 09:03:08 53.89404 -122.818

Because here, the feeder units were reused for different experiments,
some feeders have the same id, but a different lat/lon. This will create
problems later on, so let's create unique feeder\_id names:

    r$feeder_id <- paste(r$experiment, r$feeder_id, sep = "-")
    head(r)

    ##   experiment feeder_id    bird_id                time      lat      lon
    ## 1       exp1 exp1-GR10 0620000006 2015-12-05 12:57:48 53.89404 -122.818
    ## 2       exp1 exp1-GR10 06200000F7 2015-12-05 15:00:28 53.89404 -122.818
    ## 3       exp1 exp1-GR10 06200000F7 2015-12-05 15:00:30 53.89404 -122.818
    ## 4       exp1 exp1-GR10 06200000F7 2015-12-07 10:16:05 53.89404 -122.818
    ## 5       exp1 exp1-GR10 06200000F7 2015-12-08 08:58:10 53.89404 -122.818
    ## 6       exp1 exp1-GR10 06200000F7 2015-12-08 09:03:08 53.89404 -122.818

<a id="loadweb"></a>

`load_web()`
------------

You can go to <http://gaia.tru.ca/birdMOVES/datadownload.html> and
download a csv file of data from the BirdMoves website. To load and
format this data for use with the feedr package, run:

    r <- load_web("./Data/load_web.csv")
    head(r)

    ##      bird_id                time feeder_id     species       lon      lat
    ## 1 0620000514 2016-01-28 12:34:25      2200 House Finch -120.3612 50.66778
    ## 2 0620000514 2016-01-28 12:34:28      2200 House Finch -120.3612 50.66778
    ## 3 041868D861 2016-01-28 12:35:41      2200 House Finch -120.3612 50.66778
    ## 4 06200004F8 2016-01-28 12:35:52      2200 House Finch -120.3612 50.66778
    ## 5 06200004F8 2016-01-28 12:35:59      2200 House Finch -120.3612 50.66778
    ## 6 06200004F8 2016-01-28 12:36:02      2200 House Finch -120.3612 50.66778

The default timezone is "America/Vancouver", therefore, if your data is
from a different time zone, you need to specify that manually. Make sure
you use a timezone name from the output of OlsonNames(). For example:

    r <- load_web("./Data/load_web.csv", tz = "America/Costa_Rica")
    r$time[1]
    head(r)

    ## [1] "2016-01-28 12:34:25 CST"

    ##      bird_id                time feeder_id     species       lon      lat
    ## 1 0620000514 2016-01-28 12:34:25      2200 House Finch -120.3612 50.66778
    ## 2 0620000514 2016-01-28 12:34:28      2200 House Finch -120.3612 50.66778
    ## 3 041868D861 2016-01-28 12:35:41      2200 House Finch -120.3612 50.66778
    ## 4 06200004F8 2016-01-28 12:35:52      2200 House Finch -120.3612 50.66778
    ## 5 06200004F8 2016-01-28 12:35:59      2200 House Finch -120.3612 50.66778
    ## 6 06200004F8 2016-01-28 12:36:02      2200 House Finch -120.3612 50.66778

**Note:** This doesn't *convert* the time from one zone to another, it
merely *assigns* the timezone. Make sure this matches the timezone of
the original data download.

<a id="dldata"></a>

`dl_data()`
-----------

This is likely the easiest way to get data (provided the data you're
interested in is hosted on the BirdMoves website). This function
requests data from the BirdMoves website and formats it for use with the
feedr transformation functions.

If you don't specify anything, all data will be downloaded for the
default site (Kamloops) and the default extra columns (loc and species)
with the default timezone (America/Vancouver). See ?dl\_data for more
details.

    ##      bird_id                time feeder_id            species       lon
    ## 1 062000031A 2015-09-02 08:25:58      2100 Mountain Chickadee -120.3624
    ## 2 062000031A 2015-09-02 08:26:01      2100 Mountain Chickadee -120.3624
    ## 3 062000031A 2015-09-03 10:12:20      2100 Mountain Chickadee -120.3624
    ## 4 062000031A 2015-09-03 10:12:23      2100 Mountain Chickadee -120.3624
    ## 5 062000031A 2015-09-05 16:32:20      2100 Mountain Chickadee -120.3624
    ## 6 062000031A 2015-09-05 16:32:23      2100 Mountain Chickadee -120.3624
    ##        lat
    ## 1 50.66896
    ## 2 50.66896
    ## 3 50.66896
    ## 4 50.66896
    ## 5 50.66896
    ## 6 50.66896

    r <- dl_data()
    head(r)

You can specify start and end times:

    r <- dl_data(start = "2015-10-01", end = "2015-10-02")
    head(r)

    ##      bird_id                time feeder_id     species       lon      lat
    ## 1 06200003AA 2015-10-01 08:24:44      2700 House Finch -120.3632 50.66909
    ## 2 06200003AA 2015-10-01 08:24:46      2700 House Finch -120.3632 50.66909
    ## 3 06200003AA 2015-10-01 08:24:49      2700 House Finch -120.3632 50.66909
    ## 4 06200003AA 2015-10-01 08:24:51      2700 House Finch -120.3632 50.66909
    ## 5 06200003AA 2015-10-01 08:24:53      2700 House Finch -120.3632 50.66909
    ## 6 06200003AA 2015-10-01 08:24:55      2700 House Finch -120.3632 50.66909

    r <- dl_data(start = "2015-10-01 09:00:00", end = "2015-10-02")
    head(r)

    ##      bird_id                time feeder_id     species       lon      lat
    ## 1 06200003AA 2015-10-01 09:15:32      2700 House Finch -120.3632 50.66909
    ## 2 06200004F8 2015-10-01 09:15:50      2700 House Finch -120.3632 50.66909
    ## 3 06200004F8 2015-10-01 09:16:13      2700 House Finch -120.3632 50.66909
    ## 4 06200004F8 2015-10-01 09:16:15      2700 House Finch -120.3632 50.66909
    ## 5 06200004F8 2015-10-01 09:16:17      2700 House Finch -120.3632 50.66909
    ## 6 06200004F8 2015-10-01 09:16:20      2700 House Finch -120.3632 50.66909

You can also specify only a start or an end, all data up to or after
that point will be grabbed:

    r <- dl_data(end = "2015-09-06")
    head(r)

    ##      bird_id                time feeder_id            species       lon
    ## 1 062000031A 2015-09-02 08:25:58      2100 Mountain Chickadee -120.3624
    ## 2 062000031A 2015-09-02 08:26:01      2100 Mountain Chickadee -120.3624
    ## 3 062000031A 2015-09-03 10:12:20      2100 Mountain Chickadee -120.3624
    ## 4 062000031A 2015-09-03 10:12:23      2100 Mountain Chickadee -120.3624
    ## 5 062000031A 2015-09-05 16:32:20      2100 Mountain Chickadee -120.3624
    ## 6 062000031A 2015-09-05 16:32:23      2100 Mountain Chickadee -120.3624
    ##        lat
    ## 1 50.66896
    ## 2 50.66896
    ## 3 50.66896
    ## 4 50.66896
    ## 5 50.66896
    ## 6 50.66896

Extra bird or feeder related details (?dl\_data for more):

    r <- dl_data(end = "2015-09-06", feeder_details = c("loc","site_name"), bird_details = c("species", "age", "sex"))
    head(r)

    ##      bird_id                time feeder_id    site_name            species
    ## 1 062000031A 2015-09-02 08:25:58      2100 Kamloops, BC Mountain Chickadee
    ## 2 062000031A 2015-09-02 08:26:01      2100 Kamloops, BC Mountain Chickadee
    ## 3 062000031A 2015-09-03 10:12:20      2100 Kamloops, BC Mountain Chickadee
    ## 4 062000031A 2015-09-03 10:12:23      2100 Kamloops, BC Mountain Chickadee
    ## 5 062000031A 2015-09-05 16:32:20      2100 Kamloops, BC Mountain Chickadee
    ## 6 062000031A 2015-09-05 16:32:23      2100 Kamloops, BC Mountain Chickadee
    ##   age sex       lon      lat
    ## 1  SY   M -120.3624 50.66896
    ## 2  SY   M -120.3624 50.66896
    ## 3  SY   M -120.3624 50.66896
    ## 4  SY   M -120.3624 50.66896
    ## 5  SY   M -120.3624 50.66896
    ## 6  SY   M -120.3624 50.66896

------------------------------------------------------------------------

Back to [top](#top)  
Go back to [main document](feedr.html) | Continue with
[housekeeping](housekeeping.html)
