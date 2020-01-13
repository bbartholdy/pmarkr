# pmarkr

This package provides functions for calculating additional cut-off points for two-group linear discriminant analysis based on probability.
The cut-off points (PMarks) can be adjusted based on the 
The main function calculates the additional cut-off points (PMarks) using bootstrapping.
It also contains an auxillary function to estimate sex using humeral measurements (max length, head diameter, and epicondylar breadth).

# Installation
1. To install pmarkr, R (>= 3.5.1) must first be installed (https://cran.r-project.org/).

The next steps can be copied and directly pasted into the R-console.
```r
#2. Open R, and install the package "devtools" by entering the following into the R-console:
install.packages("devtools")

#3. The pmarkr package can then be downloaded and loaded:
devtools::install_github("bbartholdy/pmarkr")
library(pmarkr)
```

```r
#4. (optional) Open the pmarkr shiny app
runPMark()
```

#Usage
