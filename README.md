# Metrics Report R Package

This package contains functions for DSSV metrics reporting, starting from establishing
connection to the database to cleaning the tables.

## Requirements

Before attempting to use, install, and maintain this package, you should have 
the following packages.

- DBI
- httr
- readxl
- RPostgres
- dplyr (part of tidyverse)
- lubridate (part of tidyverse)
- stringr (part of tidyverse)
- tidyr (part of tidyverse)
- purrr (part of tidyverse)
- ggplot2 (part of tidyverse)
- recipes
- flextable
- knitr
- devtools

You can install with:

```
install.packages(c("DBI", "httr", "readxl", "RPostgres", "dplyr", "lubridate", "stringr", "tidyr", "purrr", "ggplot2", "recipes", "flextable", "knitr", "devtools"))
```

Additionally, `render_sankey()` requires the `networkD3`, `htmlwidgets`, `webshot2`, and `htmltools` packages. These are not installed automatically with this package since they are only needed for that one function.

## Installation

You have multiple ways to install the `metricsReport` package.

### Method 1

Install `pak` if you don't already have it (`install.packages("pak")`), then install directly from the repo via this command:

```
pak::pak("rcds-dssv/metricsReport")
```

(`devtools::install_github()` also still works, but it is deprecated in favor of `pak`.)

### Method 2

Clone the repository (and unzip if zipped). Open `metricsReport.Rproj`, this should open R Studio
in the project environment. Then you can run the following:

```
devtools::install()
```
