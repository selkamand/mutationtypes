test_that("mutation_types_convert_pave_to_genic works", {

  # Define all pave terms
  pave_terms <- c("upstream_gene_variant", "intron_variant", "5_prime_UTR_variant",
                  "3_prime_UTR_variant", "non_coding_transcript_exon_variant",
                  "synonymous_variant", "phased_synonymous", "missense_variant",
                  "inframe_insertion", "inframe_deletion", "phased_inframe_insertion",
                  "phased_inframe_deletion", "phased_missense", "stop_gained", "start_lost", "stop_lost", "splice_donor_variant",
                  "splice_acceptor_variant", 'frameshift_variant')


  expect_error(
    mutation_types_convert_pave_to_genic(pave_terms[1], verbose = TRUE) |> suppressMessages(),
    NA
  )

  expect_error(
    mutation_types_convert_pave_to_genic(pave_terms, verbose = TRUE) |> suppressMessages(),
    NA
  )

  expect_type(
    mutation_types_convert_pave_to_genic(pave_terms, verbose = TRUE) |> suppressMessages(),
    "character"
  )

  expect_length(
    mutation_types_convert_pave_to_genic(pave_terms, verbose = TRUE) |> suppressMessages(),
    n = length(pave_terms)
  )

  expect_equal(
    mutation_types_convert_pave_to_genic("missense_variant", verbose = TRUE) |> suppressMessages(),
    "intragenic"
  )

  # Test & seperated consequences are resolved correctly
  expect_equal(
    mutation_types_convert_pave_to_genic("missense_variant&splice_acceptor_variant", verbose = TRUE) |> suppressMessages(),
    "intragenic"
  )

  # Test & seperated consequences are resolved correctly
  expect_equal(
    mutation_types_convert_pave_to_genic(c("stop_lost&missense_variant", "stop_gained&upstream_gene_variant&phased_synonymous"), verbose = TRUE) |> suppressMessages(),
    c("intragenic","intragenic")
  )

  # Throw an error if you supply empty strings as mutation_types
  expect_error(
    mutation_types_convert_pave_to_genic(pave_mutation_types = "") |> suppressMessages(),
    regexp = "Found 1 variant with no mutation type value", fixed = TRUE
  )

  # Throw an error if you supply missing (NA) values in mutation_types
  expect_error(
    mutation_types_convert_pave_to_genic(pave_mutation_types = NA_character_) |> suppressMessages(),
    regexp = "'pave_mutation_types' must have no missing values! Found 1", fixed = TRUE
  )

  # Return intergenic if you supply empty strings (or missing NA values) as mutation_types and set  missing_to_intergenic = TRUE
  expect_equal(
    mutation_types_convert_pave_to_genic(pave_mutation_types = "", missing_to_intergenic = TRUE) |> suppressMessages(),
    "intergenic"
  )
  expect_equal(
    mutation_types_convert_pave_to_genic(pave_mutation_types = NA_character_, missing_to_intergenic = TRUE) |> suppressMessages(),
    "intergenic"
  )
})

test_that("mutation_types_convert_pave_to_coding works", {

  # Define all pave terms
  pave_terms <- c("upstream_gene_variant", "intron_variant", "5_prime_UTR_variant",
                  "3_prime_UTR_variant", "non_coding_transcript_exon_variant",
                  "synonymous_variant", "phased_synonymous", "missense_variant",
                  "inframe_insertion", "inframe_deletion", "phased_inframe_insertion",
                  "phased_inframe_deletion", "phased_missense", "stop_gained", "start_lost", "stop_lost", "splice_donor_variant",
                  "splice_acceptor_variant", 'frameshift_variant')


  expect_error(
    mutation_types_convert_pave_to_coding(pave_terms[1], verbose = TRUE) |> suppressMessages(),
    NA
  )

  expect_error(
    mutation_types_convert_pave_to_coding(pave_terms, verbose = TRUE) |> suppressMessages(),
    NA
  )

  expect_type(
    mutation_types_convert_pave_to_coding(pave_terms, verbose = TRUE) |> suppressMessages(),
    "character"
  )

  expect_length(
    mutation_types_convert_pave_to_coding(pave_terms, verbose = TRUE) |> suppressMessages(),
    n = length(pave_terms)
  )

  expect_equal(
    mutation_types_convert_pave_to_coding("missense_variant", verbose = TRUE) |> suppressMessages(),
    "coding"
  )

  expect_equal(
    mutation_types_convert_pave_to_coding(c("upstream_gene_variant", "splice_acceptor_variant"), verbose = TRUE) |> suppressMessages(),
    c("noncoding", "noncoding")
  )

  # Test & separated consequences are resolved correctly
  expect_equal(
    mutation_types_convert_pave_to_coding("missense_variant&upstream_gene_variant", verbose = TRUE) |> suppressMessages(),
    "coding"
  )

  # Test & seperated consequences are resolved correctly
  expect_equal(
    mutation_types_convert_pave_to_coding(c("stop_lost&missense_variant", "stop_gained&upstream_gene_variant&phased_synonymous"), verbose = TRUE) |> suppressMessages(),
    c("coding","coding")
  )

  # Throw an error if you supply empty strings as mutation_types
  expect_error(
    mutation_types_convert_pave_to_coding(pave_mutation_types = "") |> suppressMessages(),
    regexp = "Found 1 variant with no mutation type value", fixed = TRUE
  )

  # Throw an error if you supply missing (NA) values in mutation_types
  expect_error(
    mutation_types_convert_pave_to_coding(pave_mutation_types = NA_character_) |> suppressMessages(),
    regexp = "'pave_mutation_types' must have no missing values! Found 1", fixed = TRUE
  )

  # Return noncoding if you supply empty strings (or missing NA values) as mutation_types and set  missing_to_noncoding = TRUE
  expect_equal(
    mutation_types_convert_pave_to_coding(pave_mutation_types = "", missing_to_noncoding = TRUE) |> suppressMessages(),
    "noncoding"
  )
  expect_equal(
    mutation_types_convert_pave_to_coding(pave_mutation_types = NA_character_, missing_to_noncoding = TRUE) |> suppressMessages(),
    "noncoding"
  )
})

