
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mutationtypes <img src="man/figures/logo.png" align="right" height="108" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/mutationtypes)](https://CRAN.R-project.org/package=mutationtypes)
[![R-CMD-check](https://github.com/selkamand/mutationtypes/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/selkamand/mutationtypes/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/selkamand/mutationtypes/branch/master/graph/badge.svg)](https://app.codecov.io/gh/selkamand/mutationtypes?branch=master)
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

# Get list of valid MAF terms
mutation_types_maf()
#>  [1] "Splice_Site"            "Nonsense_Mutation"      "Frame_Shift_Del"       
#>  [4] "In_Frame_Ins"           "In_Frame_Del"           "Missense_Mutation"     
#>  [7] "Splice_Region"          "Intron"                 "Nonstop_Mutation"      
#> [10] "Translation_Start_Site" "Silent"                 "RNA"                   
#> [13] "5'UTR"                  "3'UTR"                  "5'Flank"               
#> [16] "3'Flank"                "Targeted_Region"        "IGR"

# Get list of valid SO terms
mutation_types_so()
#>  [1] "transcript_ablation"                           
#>  [2] "splice_acceptor_variant"                       
#>  [3] "splice_donor_variant"                          
#>  [4] "exon_loss_variant"                             
#>  [5] "stop_gained"                                   
#>  [6] "frameshift_variant"                            
#>  [7] "stop_lost"                                     
#>  [8] "start_lost"                                    
#>  [9] "initiator_codon_variant"                       
#> [10] "transcript_amplification"                      
#> [11] "inframe_insertion"                             
#> [12] "inframe_deletion"                              
#> [13] "missense_variant"                              
#> [14] "splice_region_variant"                         
#> [15] "splice_donor_5th_base_variant"                 
#> [16] "splice_donor_region_variant"                   
#> [17] "splice_polypyrimidine_tract_variant"           
#> [18] "incomplete_terminal_codon_variant"             
#> [19] "start_retained_variant"                        
#> [20] "stop_retained_variant"                         
#> [21] "synonymous_variant"                            
#> [22] "coding_sequence_variant"                       
#> [23] "conservative_missense_variant"                 
#> [24] "rare_amino_acid_variant"                       
#> [25] "INTRAGENIC"                                    
#> [26] "intragenic_variant"                            
#> [27] "mature_miRNA_variant"                          
#> [28] "5_prime_UTR_variant"                           
#> [29] "3_prime_UTR_variant"                           
#> [30] "non_coding_transcript_exon_variant"            
#> [31] "intron_variant"                                
#> [32] "NMD_transcript_variant"                        
#> [33] "non_coding_transcript_variant"                 
#> [34] "upstream_gene_variant"                         
#> [35] "downstream_gene_variant"                       
#> [36] "TFBS_ablation"                                 
#> [37] "TFBS_amplification"                            
#> [38] "TF_binding_site_variant"                       
#> [39] "regulatory_region_ablation"                    
#> [40] "regulatory_region_amplification"               
#> [41] "feature_elongation"                            
#> [42] "regulatory_region_variant"                     
#> [43] "feature_truncation"                            
#> [44] "intergenic_variant"                            
#> [45] "exon_variant"                                  
#> [46] "non_coding_exon_variant"                       
#> [47] "nc_transcript_variant"                         
#> [48] "5_prime_UTR_premature_start_codon_gain_variant"
#> [49] "regulatory_region"                             
#> [50] "intergenic_region"                             
#> [51] "protein_altering_variant"

# Get Palettes for MAF/SO terms
mutation_types_maf_palette()
#>            Splice_Site      Nonsense_Mutation        Frame_Shift_Del 
#>              "#33A02C"              "#FB9A99"              "#E31A1C" 
#>           In_Frame_Ins           In_Frame_Del      Missense_Mutation 
#>              "#CAB2D6"              "#6A3D9A"              "#1F78B4" 
#>          Splice_Region                 Intron       Nonstop_Mutation 
#>              "#B2DF8A"              "#FDBF6F"              "#A6CEE3" 
#> Translation_Start_Site                 Silent                    RNA 
#>              "#B15928"              "#FDBF6F"              "#FDBF6F" 
#>                  5'UTR                  3'UTR                5'Flank 
#>              "#FDBF6F"              "#FDBF6F"              "#FDBF6F" 
#>                3'Flank        Targeted_Region                    IGR 
#>              "#FDBF6F"              "#FDBF6F"              "#FDBF6F"
```
