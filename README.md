# r-bootcamp-2018

This repository contains files for the 2018 R Bootcamp sponsored by the Department of Psychology at Penn State.

To view the HTML site, go here: <https://psu-psychology.github.io/r-bootcamp-2018>

## Contents

- `*.Rmd` files are rendered into the HTML pages that constitute the top level of the website along with helper `*.html` files
- `data/`: data files
- `docs/`: root folder for site; `rmarkdown::render_site()` copies rendered files here.
- `figures`: figures generated by the `codebook()` command.
- `img/`: figures used in site and talks.
- `R/`: Helper scripts and functions.
- `talks/`: source and helper files for bootcamp talks and tutorials.

## Rendering the web site

The `*.Rmd` files in the project's root directory are used to generate the web site. Run `rmarkdown::render_site()` to regenerate it. The output `*.html` and related files go in `docs/`.

`source("R/Make_site.R")` will re-render all of the talks and tutorials and update the site pages.


