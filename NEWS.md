# feedr 0.9.0

## Major Changes
- animalnexus.ca now includes 'Settings' tab to change function arguments
- New 'convert' functions: convert_asnipe, convert_anidom, convert_dominance to convert data for use by other packages (namely [asnipe](https://cran.r-project.org/package=asnipe), [aniDom](https://cran.r-project.org/package=aniDom) and [Dominance](https://cran.r-project.org/package=Dominance))
- New summary options for map_leaflet() and map_ggmap(). These arguments perform common data summarizations before plotting, so users have the option of passing unsummarized movement and presence data.

## Bug fixes/tweaks
- `load_format()`, `ui_import()` and animalnexus Import tab are now much more forgiving for column names
    - will ignores capitalizations
    - will accept lon, long, and longitude, as well as lat, and latitude for coordinates
    - will accept animal_id or bird_id, and logger_id or feeder_id
    - will rename all to be consistent: animal_id, logger_id, time, lon, lat
- `load_format()` has new `verbose` argument to allow/prevent messages about renaming
- `load_raw()` and `load_raw_all()` have new `verbose` argument to allow/prevent progress messages
- `load_raw()` and `load_raw_all()` stop if logger_ids extracted as NA

## Other
- Internal changes to include more testing

# feedr 0.8.2

## Changes

- Update database service for `dl_data()`
- Update database service for animalnexus

## Bug fixes

- Fix bug in `visits()` where visit was premature terminated if another animal_id was detected at a different logger.


# feedr 0.8.1 (2017-02-24)

## Changes

- Streamlined animalnexus database UI
- Warnings added to ui's if launched without internet or database access
- Updated package imports to include package updates and only packages on CRAN

## Bug fixes

- Timezone usage is more consistent and uses non-DST by default
- Fixed numerous timezone issues in animalnexus
- Fixed bug that prevented importing a second dataset in animalnexus
- Fixed bug that reverted to original database dataset if tried to load second dataset in animalnexus
- Species names are now English names
- Active dataset name only shows first data set if multiple files loaded
- Can't download data if zero selected
- Can't download empty zip in trans if no data available
- UI Trans gives more informative error when data is private
- UI animate forces a pause on inputs to prevent too much activity

# feedr 0.8.0 (2016-12-08)

## New features

- New standalone shiny app to launch dialogue for transformations: `ui_trans()`
- Can now import and combine multiple files in `ui_import`

## Changes

- Combined all animations into one
- Updated "Current activity" style
- Altered display of pictures of species/individuals on animalnexus.ca
- Added new documentation examples
- Renamed standalone shiny apps to reflect the user-interface
    - `import_data()` -> `ui_import()`
    - `map_animate()` -> `ui_animate()`
- Changed termniology to reflect more general usage
    - `bird_id -> animal_id`
    - `feeder_id -> logger_id`
    - `feeding() -> presence()`

## Bug Fixes

- Fixed minor errors in `activity()`, `move()` and `feeding()`
- Fixed errors in documentation examples

- Changed factoring to allow use of dplyr's group_by to run transformations on groups of data (e.g. different sites or experiments)

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
