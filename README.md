
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mutationtypes

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/mutationtypes)](https://CRAN.R-project.org/package=mutationtypes)
<!-- badges: end -->

Say you want to describe the impact of a mutation. There exists several
dictionaries you could use, including

1)  [Sequence Ontology](http://www.sequenceontology.org/). These terms
    are what VEP uses

2)  [MAF Variant
    Classification](https://docs.gdc.cancer.gov/Encyclopedia/pages/Mutation_Annotation_Format_TCGAv2/)

**mutationtypes** makes it easy to check whether your vector of mutation
types aligns to one of these dictionaries. It also simplifies
translation from one dictionary to another.

## Installation

You can install the development version of mutationtypes like so:

``` r
# install.packages('remotes')
remotes::install_github('selkamand/mutation_types.R')
```

## Usage

``` r
library(mutationtypes)

# Check which mutation impact dictionary you're vector is closest too
mutation_types_identify(
  c(
    "INTRAGENIC", 
    "INTRAGENIC", 
    "intergenic_region", 
    "stop_lost", 
    "missense_variant", 
    "missense_variant"
  )
)
#> 
#> ── Identify Class ──────────────────────────────────────────────────────────────
#> ℹ Found 4 unique mutation types in input set
#> ℹ 4/4 mutation types were valid SO terms
#> ℹ 0/4 mutation types were valid MAF terms
#> [1] "SO"

# Convert sequence ontology mutation classes to MAF terms
mutation_types_convert_so_to_maf(
  so_mutation_types = c(
    "INTRAGENIC", 
    "INTRAGENIC", 
    "intergenic_region", 
    "stop_lost", 
    "missense_variant", 
    "missense_variant"
  )
)
#> 
#> ── Validating Input ────────────────────────────────────────────────────────────
#> ✔ Supplied mutation types are valid so terms
#> ✔ All input SO terms have valid mappings to MAF terms
#> [1] "Intron"            "Intron"            "IGR"              
#> [4] "Nonstop_Mutation"  "Missense_Mutation" "Missense_Mutation"

# Check what 
```
