# spsRdata: Preparing Site-level Variables

**Description:**

R package `spsRdata`

**Authors:**

-   [Naoki Egami](https://naokiegami.com) (Maintainer)
-   [Diana Da In Lee](https://www.dianadainlee.com)

**Reference:**

-   Egami and Lee. (2023+). Designing Multi-Context Studies for External
    Validity: Site Selection via Synthetic Purposive Sampling.

### Installation Instructions

You can install the most recent development version using the `devtools`
package. First you have to install `devtools` using the following code.
Note that you only have to do this once:

``` r
if(!require(devtools)) install.packages("spsRdata")
```

Then, load `devtools` and use the function `install_github()` to install
`spsRdata`:

``` r
library(devtools)
install_github("naoki-egami/spsRdata", dependencies = TRUE)
```
