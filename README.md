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
- recipes
- devtools

You can install with:

```
install.packages(c("DBI", "httr", "readxl", "RPostgres", "dplyr", "lubridate", "stringr", "recipes", "devtools"))
```

## Installation

You have multiple ways to install the `metricsReport` package.

### Method 1

You can install directly from the repo via this command:

```
devtools::install_github("https://github.com/rcds-dssv/metricsReport")
```

### Method 2

Clone the repository (and unzip if zipped). Open `metricsReport.Rproj`, this should open R Studio
in the project environment. Then you can run the following:

```
devtools::install()
```
