* This is a new release.

## Test environments
* local
    * MacOS Ventura 13.2.1, R 4.2.2
    * R devel via devtools::check_win_devel()
* github-actions
    * macOS (release)
    * ubuntu (release, devel, oldrel-1)
    * windows (release)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking Rd line widths ... NOTE
  Rd file 'narrate_descriptive.Rd':
    \usage lines wider than 90 characters:

Usage lines wider than 90 characters for several functions are required since function arguments contain narrative templates and are longer.
