test_that("mutation_types_convert_so_to_maf works", {

  so_terms <- c("transcript_ablation","transcript_ablation", "transcript_ablation", "splice_acceptor_variant", "splice_donor_variant",
    "exon_loss_variant", "stop_gained", "stop_lost",
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
    mutation_types_convert_so_to_maf(so_terms[1], verbose = TRUE) |> suppressMessages(),
    NA
  )

  expect_error(
    mutation_types_convert_so_to_maf(so_terms, verbose = TRUE) |> suppressMessages(),
    NA
  )

  expect_type(
    mutation_types_convert_so_to_maf(so_terms, verbose = TRUE) |> suppressMessages(),
    "character"
  )

  expect_length(
    mutation_types_convert_so_to_maf(so_terms, verbose = TRUE) |> suppressMessages(),
    n=length(so_terms)
  )

  expect_equal(
    mutation_types_convert_so_to_maf("missense_variant", verbose = TRUE) |> suppressMessages(),
    "Missense_Mutation"
  )

  expect_error(
    mutation_types_convert_so_to_maf("protein_altering_variant", verbose = TRUE) |> suppressMessages(),
    "protein_altering_variant.*variant_type.*inframe"
  )

  expect_error(
    mutation_types_convert_so_to_maf("frameshift_variant", verbose = TRUE) |> suppressMessages(),
    "frameshift_variant.*variant_type"
  )

  # Test conversions of frameshift_variant and protein_altering_variant
  expect_equal(
    mutation_types_convert_so_to_maf(
      c("frameshift_variant", "frameshift_variant", "protein_altering_variant", "protein_altering_variant", "protein_altering_variant", "protein_altering_variant"),
      variant_type = c("DEL", "INS", "DEL", "INS", "DEL", "INS"),
      inframe = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
      verbose = FALSE) |> suppressMessages(),
    c("Frame_Shift_Del", "Frame_Shift_Ins", "In_Frame_Del", "In_Frame_Ins", "Frame_Shift_Del", "Frame_Shift_Ins")
  )
  # Test & seperated consequences are resolved correctly
  expect_equal(
    mutation_types_convert_so_to_maf("missense_variant&transcript_ablation", verbose = TRUE) |> suppressMessages(),
    "Splice_Site"
  )

  # Test & seperated consequences are resolved correctly
  expect_equal(
    mutation_types_convert_so_to_maf(c("stop_lost&missense_variant", "stop_gained&upstream_gene_variant&downstream_gene_variant"), verbose = TRUE) |> suppressMessages(),
    c("Nonstop_Mutation","Nonsense_Mutation")
  )

  # Throw an error if you supply empty strings as mutation_types
  expect_error(
    mutation_types_convert_so_to_maf(so_mutation_types = "") |> suppressMessages(),
    regexp = "Found 1 variant with no mutation type value", fixed=TRUE
  )

  # Throw an error if you supply missing (NA) values in mutation_types
  expect_error(
    mutation_types_convert_so_to_maf(so_mutation_types = NA_character_) |> suppressMessages(),
    regexp = "'so_mutation_types' must have no missing values! Found 1", fixed=TRUE
  )

  # Return SILENT if you supply empty strings (or missing NA values) as mutation_types and set  missing_to_silent = TRUE
  expect_equal(
    mutation_types_convert_so_to_maf(so_mutation_types = "", missing_to_silent = TRUE) |> suppressMessages(),
    "Silent"
  )
  expect_equal(
    mutation_types_convert_so_to_maf(so_mutation_types = NA_character_, missing_to_silent = TRUE) |> suppressMessages(),
    "Silent"
  )
})

test_that("mutation_types_convert_so_to_maf() throws appropriate errors", {
  expect_error(assert_all_mutations_are_valid_so("bob"), regexp =  "Found 1 mutation type which was NOT a valid SO terms: [bob]", fixed = TRUE)
})
