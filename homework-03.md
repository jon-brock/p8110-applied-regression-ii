P8110 - Applied Regression II - Homework \#3
================

``` r
library(survival)
library(survminer)
library(tidyverse)
```

The ”HW3Data” gives the time until staphylococcus infection (in days)
for 154 patients with a burn wound. The purpose of this study was to
compare a routine bathing care treatment with a body-cleansing method.
The time until staphylococcus infection was recorded, along with an
indicator variable – whether or not an infection had occurred. The
severity of the burn was measured by percentage of total surface area of
body burned.

| variable            | description                                             |
|---------------------|---------------------------------------------------------|
| id                  | patient id                                              |
| treatment           | 0 = routine bathing <br> 1 = body cleansing             |
| pct\_burned         | percentage of total surface area burned                 |
| time\_to\_infection | time to staphylocous aureaus infection or on study time |
| infection           | 0 = infection did not occur <br> 1 = infection occurred |

------------------------------------------------------------------------

1.  Use R to compute the Kaplan-Meier estimator and 95% CI of the
    survival function for the routine bathing group, and also for the
    body-cleansing group (only keep the K-M estimates for unique event
    time points). Generate a graph of the survival functions in the two
    treatment groups. Interpret the graph. (*Hint: What do you observe
    about the difference of the survival functions between the two
    groups?*)

``` r
hw3_data <- read_csv("./data/hw3_data.csv",
                     col_names = c("id", "treatment", "pct_burned", "time_to_infection", "infection"))
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   id = col_double(),
    ##   treatment = col_double(),
    ##   pct_burned = col_double(),
    ##   time_to_infection = col_double(),
    ##   infection = col_double()
    ## )
