<a id = "top"></a>

This is a quick tutorial covering a couple functions you can use to
clean and fix problems in your data.

-   [`check_ids()`](#checkids)
-   [`check_problems()`](#checkproblems)

These functions can be skipped, or used one after the other. It's up to
you. Basically, they are wrapper functions for cleaning your data while
maintaining appropriate categorical (factor) levels.

This tutorial assumes that you already know how to
[load/import](load.html) your data. We will be using the feedr package
example dataset: finches.

    head(finches)

    ##      bird_id                time feeder_id     species       lon      lat
    ## 1 0620000514 2016-01-28 12:34:25      2200 House Finch -120.3612 50.66778
    ## 2 0620000514 2016-01-28 12:34:28      2200 House Finch -120.3612 50.66778
    ## 3 041868D861 2016-01-28 12:35:41      2200 House Finch -120.3612 50.66778
    ## 4 06200004F8 2016-01-28 12:35:52      2200 House Finch -120.3612 50.66778
    ## 5 06200004F8 2016-01-28 12:35:59      2200 House Finch -120.3612 50.66778
    ## 6 06200004F8 2016-01-28 12:36:02      2200 House Finch -120.3612 50.66778

<a id = "checkids"></a>

`check_ids()`
-------------

This function can be used to remove any bird\_ids that are present, but
which you know aren't really birds. For example, if you use a 'wand' to
test the deployment of your feeders, this is a bird\_id that you should
remove prior to analysis. Further, occasionally there are bird\_ids that
are error codes (e.g. 0000000000), you may wish to determine why these
are present, but once again, for analysis they should be removed.

This function works by comparing the list of bird\_ids in the data to an
external, bird\_id dataset. The dataset is expected to have at least two
columns: bird\_id and species. The species column should either contain
species identity (e.g. House Finch) or the error code (e.g. wand, or
error).

In addition to removing error or wand ids, this function will also
report which bird\_ids are in your data sets, but not in your bird\_id
index, and which are in your bird\_id index, but not in your data sets.

### With no error/wand ids

Let's load a bird index file. Note that there are no errors or wands
ids.

    bird_index <- read.csv("./Data/bird_index.csv")
    bird_index

    ##      bird_id     species
    ## 1 0620000514 House Finch
    ## 2 041868D861 House Finch
    ## 3 041868FF93 House Finch
    ## 4 06200004F8 House Finch
    ## 5 062000043E House Finch
    ## 6 041868D396 House Finch

You need to give this function a data set, and it will return a cleaned
data set. Here we'll save it as r.clean:

    r.clean <- check_ids(finches, bird_ids = bird_index)

    ## All ids in your data are also in your bird_id index

    ## All ids in your bird_id index are also in your data

    ## No ids have been omitted

This output shows that all the bird\_ids in the data are also in the
index and vice versa. Further, there were no omitted ids (error or wand
ids).

### With error/wand ids

Let's see how it works if you did have a 'wand' or 'error' code in your
index file that matched a bird\_id in your dataset.

    bird_index

    ##      bird_id     species
    ## 1 0620000514        wand
    ## 2 041868D861       error
    ## 3 041868FF93 House Finch
    ## 4 06200004F8 House Finch
    ## 5 062000043E House Finch
    ## 6 041868D396 House Finch

    r.clean <- check_ids(finches, bird_ids = bird_index)

    ## All ids in your data are also in your bird_id index

    ## All ids in your bird_id index are also in your data

    ## The following bird ids have been omitted: 0620000514, 041868D861

Here we omited the two bird\_ids associated with a wand (0620000514) and
with an error (041868D861).

Note that nothing else changed.

### bird\_ids present in dataset but not in the index

    bird_index

    ##      bird_id     species
    ## 1 0620000514        wand
    ## 2 041868D861       error
    ## 3 041868FF93 House Finch
    ## 4 06200004F8 House Finch
    ## 6 041868D396 House Finch

    r.clean <- check_ids(finches, bird_ids = bird_index)

    ## Some ids present in your data do not exist in the bird_id index: 062000043E

    ## All ids in your bird_id index are also in your data

    ## The following bird ids have been omitted: 0620000514, 041868D861

<a id = "checkproblems"></a>

`check_problems()`
------------------

This function is only necessary if for some reason, you're getting
errors in the bird\_ids in your data set.

This function will correct all instances of a bird\_id according to the
list provided.

    problems <- read.csv("./Data/problems.csv")
    problems

    ##   original_id corrected_id
    ## 1  06200004F8   041B6BEF6B
    ## 2  041868D396   041B999F6B

Original bird\_ids:

    finches$bird_id[1:5]

    ## [1] 0620000514 0620000514 041868D861 06200004F8 06200004F8
    ## 6 Levels: 041868D396 041868D861 041868FF93 062000043E ... 0620000514

Fix problems and new bird\_ids:

    r.clean <- check_problems(finches, problems = problems)

    ## The following bird ids have been corrected:

    ## 06200004F8 to 041B6BEF6B
    ## 041868D396 to 041B999F6B

    r.clean$bird_id[1:5]

    ## [1] 0620000514 0620000514 041868D861 041B6BEF6B 041B6BEF6B
    ## 6 Levels: 041868D861 041868FF93 062000043E 0620000514 ... 041B999F6B

Note that the bird\_ids have been modified, but also that the levels
have been updated to match.

------------------------------------------------------------------------

Back to [top](#top)  
Go back to [main document](feedr.html) | Go back to [loading/importing
data](load.html) | Continue with [transformations](transformations.html)
