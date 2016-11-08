# feedr 0.7.0.0001

## Bug Fixes

- Fixed minor errors in activity(), move() and feeding() 
- Fixed errors in documentation examples
- Added new documentation examples
- Changed factoring to allow use of dplyr's group_by to run transformations on groups of data (e.g. differen sites or experiments)
- Updated "Current activity" style


# feedr 0.7.0 (2016-11-03)

This update provides a large amount of new functionality and bug fixes, making the app smoother overall.

## New functionality

- Add loading messages to various calculations
- Force user to wait until app is loaded
- Add functions for various shiny mods which can be called from the R console
    - map_animate()
    - map\_animate\_indiv()
    - map_summary()
    - import_file()
- Add new visualization mods (map\_animate\_indiv(), map\_summary()) to animal\_nexus.

## Updated functionality

- Add date column to transformation outputs
- Update `pass = TRUE` to also keep track of data associated with date
- Update manual entries for various mods
- Add more details and descriptions to animal_nexus_.
- Add new column, move_id, to movements (output of move()).
- Change `activity()` to accept multiple bird_ids
- Add option to specify date format when importing

## Bug fixes

- Fix `check_problems()` which failed if only one line in problems index.
- Fix bugs that prevent switching between data sets
- Fix bug that prevents switching between sites when accessing the database
- Fix scaling error in `map_ggmap()`

    

# feedr 0.6.2

This update fixed a couple of minor bugs in the main package, and includes some fairly substantial changes to the animalnexus server.

- Fixed bug which omitted columns even when pass = TRUE in transformations.
- Added ability to import personal data for use in animalnexus


# feedr 0.6.1

- Added animalnexus shiny app to feedr package (`animalnexus()`)
- Fixed some bugs in visualizations
- Correct animalnexus UI issues



# feedr 0.6.0 (2016-06-20)

This update fixes a couple of small bugs and mostly involves rewriting code to use `dplyr`/`tidyr` as opposed to `plyr`/`reshape2`, and further includes `magrittr` as an import. 

- Clarify error codes
- Fix scaling errors in mapping
- Fix examples to match newer options (especially in `move()`)

# feedr <= 0.5.0 (2016-03-16 through 2016-06-20)

These versions encompassed early days of the package. Added and developed functions for transforming and visuallizing RFID data as well as documentation and vignettes.
