# feedr 0.6.2.9999

## New functionality

- Add functions for various shiny mods
    - map_animate()
    - map\_animate\_indiv()
    - map_summary()
    - import_file()
- Add new visualization mods (map\_animate\_indiv(), map\_summary()) to animal\_nexus.

- Fix bugs that prevent switching between data sets
- Fix bug that prevents switching between sites when accessing the database
    

## Updated functionality

- Add date column to transformation outputs
- Update `pass = TRUE` to also keep track of data associated with date
- Update manual entries for various mods
- Add more details and descriptions to animal_nexus_.
- Add new column, move_id, to movements (output of move()).
- Change `activity()` to accept multiple bird_ids

## Bug fixes

- Fix `check_problems()` which failed if only one line in problems index.
    
    

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
