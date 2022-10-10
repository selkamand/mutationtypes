Notes

Vep/SO terms
All other has to be ``

1. The SO term 'protein_altering_variant'

This is an annoying VEP/SO term to map across to MAF terms:

VCF2MAF uses additional information from VCFs (REF & Alt columns) (i.e. whether its inframe and an insertion/deletion to figure out how to translate)

For now I'll leave the mapping blank so we can throw an error message like ('no unambiguous mapping available for properties: protein_altering_variant')