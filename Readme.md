[![Build Status](https://travis-ci.org/MarkusLoew/CampbellLogger.svg?branch=master)](https://travis-ci.org/MarkusLoew/CampbellLogger)



CampbellLogger
==============

R-package to import Campbell Scientific TOA5 *.dat files

See 

	help(package = CampbellLogger) 

for details on the functions provided by this package.

### Installation

To install this package straight from github the `pak` library us needed (previously install from github was done via `devtools`).

```r
install.packages("pak")
```

Then, CampbellLogger package installation via pak or (previously) via devtools:

```{r}
# devtools::install_github("MarkusLoew/CampbellLogger")
pak::pak("MarkusLoew/CampbellLogger")
```

Installation under Windows might require the installation of Rtools. There will be a prompt for it if needed.

### Example usage

```{r}
# using the CampbellLogger library to import files

library(CampbellLogger)

d <- CampbellFileImport(file = "filename.dat")

# import data and units
d <- CampbellFileImport(file = "filename.dat", importunits = TRUE)
data  <- d[["data"]]
units <- d[["units"]]

