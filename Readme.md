[![Build Status](https://travis-ci.org/MarkusLoew/CampbellLogger.svg?branch=master)](https://travis-ci.org/MarkusLoew/CampbellLogger)



CampbellLogger
==============

R-package to import, merge, and visualise Campbell Scientific *.dat files

See 

	help(package = CampbellLogger) 

for details on the functions provided by this package.

### Installation

To install this package straight from github the "devtools" is needed.

```r
install.packages("devtools")
library(devtools)
```

Then, CampbellLogger package installation via pak or (previously) via devtools.

```{r}
# devtools::install_github("MarkusLoew/CampbellLogger")
pak::pak("MarkusLoew/CampbellLogger")
```

Installation under Windows might require the installation of Rtools. There will be a prompt for it if needed.

### Example usage

```{r}
# using the CampbellLogger library to import files

library(CampbellLogger)

df <- CampbellFileImport(file = "filename.dat")

# put all sensors of similar type together
df.cast <- CampbellCast(df)

