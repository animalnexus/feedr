# CLAUDE.md

This file provides guidance to Claude Code and other AI agents when working with
code in this repository.

@../CODE_DESIGN.md

## Agent notes

- `RELEASE.R` is a checklist the developer runs line by line. Don't source it.
- Leave code that looks dead unless asked to remove it, and don't build on it:
  - Dot-named functions in `R/maps.R` (such as `map.leaflet()`) and
    `map_ggmap()` are deprecation stubs.
