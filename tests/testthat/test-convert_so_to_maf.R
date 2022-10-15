test_that("mutation_types_convert_so_to_maf works", {

  so_terms <- c("transcript_ablation","transcript_ablation", "transcript_ablation", "splice_acceptor_variant", "splice_donor_variant",
    "exon_loss_variant", "stop_gained", "frameshift_variant", "stop_lost",
    "start_lost", "initiator_codon_variant", "transcript_amplification",
    "inframe_insertion", "inframe_deletion", "missense_variant",
    "splice_region_variant", "splice_donor_5th_base_variant", "splice_donor_region_variant",
    "splice_polypyrimidine_tract_variant", "incomplete_terminal_codon_variant",
    "start_retained_variant", "stop_retained_variant", "synonymous_variant",
    "coding_sequence_variant", "conservative_missense_variant", "rare_amino_acid_variant",
    "INTRAGENIC", "intragenic_variant", "mature_miRNA_variant", "5_prime_UTR_variant",
    "3_prime_UTR_variant", "non_coding_transcript_exon_variant",
    "intron_variant", "NMD_transcript_variant", "non_coding_transcript_variant","non_coding_transcript_variant",
    "upstream_gene_variant", "downstream_gene_variant", "TFBS_ablation",
    "TFBS_amplification", "TF_binding_site_variant", "regulatory_region_ablation",
    "regulatory_region_amplification", "feature_elongation", "regulatory_region_variant",
    "feature_truncation", "intergenic_variant", "exon_variant", "non_coding_exon_variant",
    "nc_transcript_variant", "5_prime_UTR_premature_start_codon_gain_variant",
    "regulatory_region", "intergenic_region"
  )

  expect_error(
    mutation_types_convert_so_to_maf(so_terms[1], verbose = FALSE),
    NA
  )

  expect_error(
    mutation_types_convert_so_to_maf(so_terms, verbose = FALSE),
    NA
  )

  expect_type(
    mutation_types_convert_so_to_maf(so_terms, verbose = FALSE),
    "character"
  )

  expect_length(
    mutation_types_convert_so_to_maf(so_terms, verbose = FALSE),
    n=length(so_terms)
  )

  expect_equal(
    mutation_types_convert_so_to_maf("missense_variant", verbose = FALSE),
    "Missense_Mutation"
  )

  expect_error(
    mutation_types_convert_so_to_maf("protein_altering_variant", verbose = FALSE),
    "mapping.*protein_altering_variant"
  )
})
